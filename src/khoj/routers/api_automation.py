import json
import logging
import threading
from typing import Optional

import cron_descriptor
import pytz
from apscheduler.job import Job
from apscheduler.triggers.cron import CronTrigger
from fastapi import APIRouter, Request
from fastapi.responses import Response
from starlette.authentication import requires

from khoj.database.adapters import AutomationAdapters, ConversationAdapters
from khoj.database.models import KhojUser
from khoj.processor.conversation.utils import clean_json
from khoj.routers.helpers import schedule_automation, schedule_query
from khoj.utils.helpers import is_none_or_empty

# Initialize Router
api_automation = APIRouter()
logger = logging.getLogger(__name__)


@api_automation.get("", response_class=Response)
@requires(["authenticated"])
def get_automations(request: Request) -> Response:
    user: KhojUser = request.user.object

    # Collate all automations created by user that are still active
    automations_info = [automation_info for automation_info in AutomationAdapters.get_automations_metadata(user)]

    # Return tasks information as a JSON response
    return Response(content=json.dumps(automations_info), media_type="application/json", status_code=200)


@api_automation.delete("", response_class=Response)
@requires(["authenticated"])
def delete_automation(request: Request, automation_id: str) -> Response:
    user: KhojUser = request.user.object

    try:
        automation_info = AutomationAdapters.delete_automation(user, automation_id)
    except ValueError:
        return Response(status_code=204)

    # Return deleted automation information as a JSON response
    return Response(content=json.dumps(automation_info), media_type="application/json", status_code=200)


@api_automation.post("", response_class=Response)
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
        calling_url = str(request.url.replace(query=f"{request.url.query}"))
        automation = schedule_automation(
            query_to_run, subject, crontime, timezone, q, user, calling_url, str(conversation.id)
        )
    except Exception as e:
        logger.error(f"Error creating automation {q} for {user.email}: {e}", exc_info=True)
        return Response(
            content="Unable to create automation. Ensure the automation doesn't already exist.",
            media_type="text/plain",
            status_code=500,
        )

    # Collate info about the created user automation
    automation_info = AutomationAdapters.get_automation_metadata(user, automation)

    # Return information about the created automation as a JSON response
    return Response(content=json.dumps(automation_info), media_type="application/json", status_code=200)


@api_automation.post("/trigger", response_class=Response)
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


@api_automation.put("", response_class=Response)
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
            "calling_url": str(request.url),
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
