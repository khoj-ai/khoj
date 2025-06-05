import json
import logging
import math
import os
import threading
import uuid
from typing import List, Optional, Union

import cron_descriptor
import openai
import pytz
from apscheduler.job import Job
from apscheduler.triggers.cron import CronTrigger
from fastapi import APIRouter, Depends, File, HTTPException, Request, UploadFile
from fastapi.requests import Request
from fastapi.responses import Response
from starlette.authentication import has_required_scope, requires

from khoj.configure import initialize_content
from khoj.database import adapters
from khoj.database.adapters import (
    AutomationAdapters,
    ConversationAdapters,
    EntryAdapters,
    get_user_photo,
)
from khoj.database.models import KhojUser, SpeechToTextModelOptions
from khoj.processor.conversation.offline.whisper import transcribe_audio_offline
from khoj.processor.conversation.openai.whisper import transcribe_audio
from khoj.processor.conversation.utils import clean_json
from khoj.routers.helpers import (
    ApiUserRateLimiter,
    CommonQueryParams,
    ConversationCommandRateLimiter,
    execute_search,
    get_user_config,
    schedule_automation,
    schedule_query,
    update_telemetry_state,
)
from khoj.utils import state
from khoj.utils.helpers import is_none_or_empty
from khoj.utils.rawconfig import SearchResponse
from khoj.utils.state import SearchType

# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)
conversation_command_rate_limiter = ConversationCommandRateLimiter(
    trial_rate_limit=2, subscribed_rate_limit=100, slug="command"
)


@api.delete("/self")
@requires(["authenticated"])
def delete_self(request: Request):
    user = request.user.object
    user.delete()
    return {"status": "ok"}


@api.get("/search", response_model=List[SearchResponse])
@requires(["authenticated"])
async def search(
    q: str,
    request: Request,
    common: CommonQueryParams,
    n: Optional[int] = 5,
    t: Optional[SearchType] = SearchType.All,
    r: Optional[bool] = False,
    max_distance: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
):
    user = request.user.object

    results = await execute_search(
        user=user,
        q=q,
        n=n,
        t=t,
        r=r,
        max_distance=max_distance or math.inf,
        dedupe=dedupe,
    )

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="search",
        **common.__dict__,
    )

    return results


@api.get("/update")
@requires(["authenticated"])
def update(
    request: Request,
    common: CommonQueryParams,
    t: Optional[SearchType] = None,
    force: Optional[bool] = False,
):
    user = request.user.object
    if not state.config:
        error_msg = f"🚨 Khoj is not configured.\nConfigure it via http://localhost:42110/settings, plugins or by editing {state.config_file}."
        logger.warning(error_msg)
        raise HTTPException(status_code=500, detail=error_msg)
    try:
        initialize_content(user=user, regenerate=force, search_type=t)
    except Exception as e:
        error_msg = f"🚨 Failed to update server via API: {e}"
        logger.error(error_msg, exc_info=True)
        raise HTTPException(status_code=500, detail=error_msg)
    else:
        components = []
        if state.search_models:
            components.append("Search models")
        components_msg = ", ".join(components)
        logger.info(f"📪 {components_msg} updated via API")

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="update",
        **common.__dict__,
    )

    return {"status": "ok", "message": "khoj reloaded"}


@api.post("/transcribe")
@requires(["authenticated"])
async def transcribe(
    request: Request,
    common: CommonQueryParams,
    file: UploadFile = File(...),
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=20, subscribed_requests=20, window=60, slug="transcribe_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=60, subscribed_requests=600, window=60 * 60 * 24, slug="transcribe_day")
    ),
):
    user: KhojUser = request.user.object
    audio_filename = f"{user.uuid}-{str(uuid.uuid4())}.webm"
    user_message: str = None

    # If the file is too large, return an unprocessable entity error
    if file.size > 10 * 1024 * 1024:
        logger.warning(f"Audio file too large to transcribe. Audio file size: {file.size}. Exceeds 10Mb limit.")
        return Response(content="Audio size larger than 10Mb limit", status_code=422)

    # Transcribe the audio from the request
    try:
        # Store the audio from the request in a temporary file
        audio_data = await file.read()
        with open(audio_filename, "wb") as audio_file_writer:
            audio_file_writer.write(audio_data)
        audio_file = open(audio_filename, "rb")

        # Send the audio data to the Whisper API
        speech_to_text_config = await ConversationAdapters.get_speech_to_text_config()
        if not speech_to_text_config:
            # If the user has not configured a speech to text model, return an unsupported on server error
            status_code = 501
        elif speech_to_text_config.model_type == SpeechToTextModelOptions.ModelType.OFFLINE:
            speech2text_model = speech_to_text_config.model_name
            user_message = await transcribe_audio_offline(audio_filename, speech2text_model)
        elif speech_to_text_config.model_type == SpeechToTextModelOptions.ModelType.OPENAI:
            speech2text_model = speech_to_text_config.model_name
            if speech_to_text_config.ai_model_api:
                api_key = speech_to_text_config.ai_model_api.api_key
                api_base_url = speech_to_text_config.ai_model_api.api_base_url
                openai_client = openai.OpenAI(api_key=api_key, base_url=api_base_url)
            elif state.openai_client:
                openai_client = state.openai_client
            if openai_client:
                user_message = await transcribe_audio(audio_file, speech2text_model, client=openai_client)
            else:
                status_code = 501
    finally:
        # Close and Delete the temporary audio file
        audio_file.close()
        os.remove(audio_filename)

    if user_message is None:
        return Response(status_code=status_code or 500)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="transcribe",
        **common.__dict__,
    )

    # Return the spoken text
    content = json.dumps({"text": user_message})
    return Response(content=content, media_type="application/json", status_code=200)


@api.get("/settings", response_class=Response)
@requires(["authenticated"])
def get_settings(request: Request, detailed: Optional[bool] = False) -> Response:
    user = request.user.object
    user_config = get_user_config(user, request, is_detailed=detailed)
    del user_config["request"]

    # Return config data as a JSON response
    return Response(content=json.dumps(user_config), media_type="application/json", status_code=200)


@api.patch("/user/name", status_code=200)
@requires(["authenticated"])
def set_user_name(
    request: Request,
    name: str,
    client: Optional[str] = None,
):
    user = request.user.object

    split_name = name.split(" ")

    if len(split_name) > 2:
        raise HTTPException(status_code=400, detail="Name must be in the format: Firstname Lastname")

    if len(split_name) == 1:
        first_name = split_name[0]
        last_name = ""
    else:
        first_name, last_name = split_name[0], split_name[-1]

    adapters.set_user_name(user, first_name, last_name)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_user_name",
        client=client,
    )

    return {"status": "ok"}


@api.get("/health", response_class=Response)
@requires(["authenticated"], status_code=200)
def health_check(request: Request) -> Response:
    response_obj = {"email": request.user.object.email}
    return Response(content=json.dumps(response_obj), media_type="application/json", status_code=200)


@api.get("/v1/user", response_class=Response)
@requires(["authenticated"])
def user_info(request: Request) -> Response:
    # Get user information
    user: KhojUser = request.user.object
    user_picture = get_user_photo(user=user)
    is_active = has_required_scope(request, ["premium"])
    has_documents = EntryAdapters.user_has_entries(user=user)

    # Collect user information in a dictionary
    user_info = {
        "email": user.email,
        "username": user.username,
        "photo": user_picture,
        "is_active": is_active,
        "has_documents": has_documents,
        "khoj_version": state.khoj_version,
    }

    # Return user information as a JSON response
    return Response(content=json.dumps(user_info), media_type="application/json", status_code=200)


@api.get("/automations", response_class=Response)
@requires(["authenticated"])
def get_automations(request: Request) -> Response:
    user: KhojUser = request.user.object

    # Collate all automations created by user that are still active
    automations_info = [automation_info for automation_info in AutomationAdapters.get_automations_metadata(user)]

    # Return tasks information as a JSON response
    return Response(content=json.dumps(automations_info), media_type="application/json", status_code=200)


@api.delete("/automation", response_class=Response)
@requires(["authenticated"])
def delete_automation(request: Request, automation_id: str) -> Response:
    user: KhojUser = request.user.object

    try:
        automation_info = AutomationAdapters.delete_automation(user, automation_id)
    except ValueError:
        return Response(status_code=204)

    # Return deleted automation information as a JSON response
    return Response(content=json.dumps(automation_info), media_type="application/json", status_code=200)


@api.post("/automation", response_class=Response)
@requires(["authenticated"])
def post_automation(
    request: Request,
    q: str,
    crontime: str,
    subject: Optional[str] = None,
    city: Optional[str] = None,
    region: Optional[str] = None,
    country: Optional[str] = None,
    timezone: Optional[str] = None,
) -> Response:
    user: KhojUser = request.user.object

    # Perform validation checks
    if is_none_or_empty(q) or is_none_or_empty(crontime):
        return Response(content="A query and crontime is required", status_code=400)
    if not cron_descriptor.get_description(crontime):
        return Response(content="Invalid crontime", status_code=400)

    # Infer subject, query to run
    _, query_to_run, generated_subject = schedule_query(q, chat_history=[], user=user)
    subject = subject or generated_subject

    # Normalize query parameters
    # Add /automated_task prefix to query if not present
    query_to_run = query_to_run.strip()
    if not query_to_run.startswith("/automated_task"):
        query_to_run = f"/automated_task {query_to_run}"

    # Normalize crontime for AP Scheduler CronTrigger
    crontime = crontime.strip()
    if len(crontime.split(" ")) > 5:
        # Truncate crontime to 5 fields
        crontime = " ".join(crontime.split(" ")[:5])

    # Convert crontime to standard unix crontime
    crontime = crontime.replace("?", "*")

    # Disallow minute level automation recurrence
    minute_value = crontime.split(" ")[0]
    if not minute_value.isdigit():
        return Response(
            content="Minute level recurrence is unsupported. Please create a less frequent schedule.",
            status_code=400,
        )

    # Create new Conversation Session associated with this new task
    title = f"Automation: {subject}"
    conversation = ConversationAdapters.create_conversation_session(user, request.user.client_app, title=title)

    # Schedule automation with query_to_run, timezone, subject directly provided by user
    try:
        # Use the query to run as the scheduling request if the scheduling request is unset
        calling_url = request.url.replace(query=f"{request.url.query}")
        automation = schedule_automation(
            query_to_run, subject, crontime, timezone, q, user, calling_url, str(conversation.id)
        )
    except Exception as e:
        logger.error(f"Error creating automation {q} for {user.email}: {e}", exc_info=True)
        return Response(
            content=f"Unable to create automation. Ensure the automation doesn't already exist.",
            media_type="text/plain",
            status_code=500,
        )

    # Collate info about the created user automation
    automation_info = AutomationAdapters.get_automation_metadata(user, automation)

    # Return information about the created automation as a JSON response
    return Response(content=json.dumps(automation_info), media_type="application/json", status_code=200)


@api.post("/trigger/automation", response_class=Response)
@requires(["authenticated"])
def trigger_manual_job(
    request: Request,
    automation_id: str,
):
    user: KhojUser = request.user.object

    # Check, get automation to edit
    try:
        automation: Job = AutomationAdapters.get_automation(user, automation_id)
    except ValueError as e:
        logger.error(f"Error triggering automation {automation_id} for {user.email}: {e}", exc_info=True)
        return Response(content="Invalid automation", status_code=403)

    # Trigger the job without waiting for the result.
    scheduled_chat_func = automation.func

    # Run the function in a separate thread
    thread = threading.Thread(target=scheduled_chat_func, args=automation.args, kwargs=automation.kwargs)
    thread.start()

    return Response(content="Automation triggered", status_code=200)


@api.put("/automation", response_class=Response)
@requires(["authenticated"])
def edit_job(
    request: Request,
    automation_id: str,
    q: Optional[str],
    subject: Optional[str],
    crontime: Optional[str],
    city: Optional[str] = None,
    region: Optional[str] = None,
    country: Optional[str] = None,
    timezone: Optional[str] = None,
) -> Response:
    user: KhojUser = request.user.object

    # Perform validation checks
    if is_none_or_empty(q) or is_none_or_empty(subject) or is_none_or_empty(crontime):
        return Response(content="A query, subject and crontime is required", status_code=400)
    if not cron_descriptor.get_description(crontime):
        return Response(content="Invalid crontime", status_code=400)

    # Check, get automation to edit
    try:
        automation: Job = AutomationAdapters.get_automation(user, automation_id)
    except ValueError as e:
        logger.error(f"Error editing automation {automation_id} for {user.email}: {e}", exc_info=True)
        return Response(content="Invalid automation", status_code=403)

    # Infer subject, query to run
    _, query_to_run, _ = schedule_query(q, chat_history=[], user=user)
    subject = subject

    # Normalize query parameters
    # Add /automated_task prefix to query if not present
    query_to_run = query_to_run.strip()
    if not query_to_run.startswith("/automated_task"):
        query_to_run = f"/automated_task {query_to_run}"
    # Normalize crontime for AP Scheduler CronTrigger
    crontime = crontime.strip()
    if len(crontime.split(" ")) > 5:
        # Truncate crontime to 5 fields
        crontime = " ".join(crontime.split(" ")[:5])
    # Convert crontime to standard unix crontime
    crontime = crontime.replace("?", "*")

    # Disallow minute level automation recurrence
    minute_value = crontime.split(" ")[0]
    if not minute_value.isdigit():
        return Response(
            content="Recurrence of every X minutes is unsupported. Please create a less frequent schedule.",
            status_code=400,
        )

    # Construct updated automation metadata
    automation_metadata: dict[str, str] = json.loads(clean_json(automation.name))
    automation_metadata["scheduling_request"] = q
    automation_metadata["query_to_run"] = query_to_run
    automation_metadata["subject"] = subject.strip()
    automation_metadata["crontime"] = crontime
    conversation_id = automation_metadata.get("conversation_id")

    if not conversation_id:
        title = f"Automation: {subject}"

        # Create new Conversation Session associated with this new task
        conversation = ConversationAdapters.create_conversation_session(user, request.user.client_app, title=title)

        conversation_id = str(conversation.id)
        automation_metadata["conversation_id"] = conversation_id

    # Modify automation with updated query, subject
    automation.modify(
        name=json.dumps(automation_metadata),
        kwargs={
            "query_to_run": query_to_run,
            "subject": subject,
            "scheduling_request": q,
            "user": user,
            "calling_url": request.url,
            "conversation_id": conversation_id,
        },
    )

    # Reschedule automation if crontime updated
    user_timezone = pytz.timezone(timezone)
    trigger = CronTrigger.from_crontab(crontime, user_timezone)
    if automation.trigger != trigger:
        automation.reschedule(trigger=trigger)

    # Collate info about the updated user automation
    automation = AutomationAdapters.get_automation(user, automation.id)
    automation_info = AutomationAdapters.get_automation_metadata(user, automation)

    # Return modified automation information as a JSON response
    return Response(content=json.dumps(automation_info), media_type="application/json", status_code=200)
