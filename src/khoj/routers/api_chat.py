import asyncio
import base64
import json
import logging
import time
from datetime import datetime
from functools import partial
from typing import Dict, Optional
from urllib.parse import unquote

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Depends, HTTPException, Request
from fastapi.responses import Response, StreamingResponse
from starlette.authentication import requires

from khoj.app.settings import ALLOWED_HOSTS
from khoj.database.adapters import (
    AgentAdapters,
    ConversationAdapters,
    EntryAdapters,
    FileObjectAdapters,
    PublicConversationAdapters,
    aget_user_name,
)
from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation.prompts import help_message, no_entries_found
from khoj.processor.conversation.utils import save_to_conversation_log
from khoj.processor.image.generate import text_to_image
from khoj.processor.speech.text_to_speech import generate_text_to_speech
from khoj.processor.tools.online_search import read_webpages, search_online
from khoj.routers.api import extract_references_and_questions
from khoj.routers.helpers import (
    ApiImageRateLimiter,
    ApiUserRateLimiter,
    ChatEvent,
    ChatRequestBody,
    CommonQueryParams,
    ConversationCommandRateLimiter,
    agenerate_chat_response,
    aget_relevant_information_sources,
    aget_relevant_output_modes,
    construct_automation_created_message,
    create_automation,
    extract_relevant_summary,
    generate_excalidraw_diagram,
    get_conversation_command,
    is_query_empty,
    is_ready_to_chat,
    read_chat_stream,
    update_telemetry_state,
    validate_conversation_config,
)
from khoj.routers.storage import upload_image_to_bucket
from khoj.utils import state
from khoj.utils.helpers import (
    AsyncIteratorWrapper,
    ConversationCommand,
    command_descriptions,
    convert_image_to_webp,
    get_country_code_from_timezone,
    get_country_name_from_timezone,
    get_device,
    is_none_or_empty,
)
from khoj.utils.rawconfig import FileFilterRequest, FilesFilterRequest, LocationData

# Initialize Router
logger = logging.getLogger(__name__)
conversation_command_rate_limiter = ConversationCommandRateLimiter(
    trial_rate_limit=100, subscribed_rate_limit=6000, slug="command"
)


api_chat = APIRouter()

from pydantic import BaseModel

from khoj.routers.email import send_query_feedback


@api_chat.get("/conversation/file-filters/{conversation_id}", response_class=Response)
@requires(["authenticated"])
def get_file_filter(request: Request, conversation_id: str) -> Response:
    conversation = ConversationAdapters.get_conversation_by_user(request.user.object, conversation_id=conversation_id)
    if not conversation:
        return Response(content=json.dumps({"status": "error", "message": "Conversation not found"}), status_code=404)

    # get all files from "computer"
    file_list = EntryAdapters.get_all_filenames_by_source(request.user.object, "computer")
    file_filters = []
    for file in conversation.file_filters:
        if file in file_list:
            file_filters.append(file)
    return Response(content=json.dumps(file_filters), media_type="application/json", status_code=200)


@api_chat.delete("/conversation/file-filters/bulk", response_class=Response)
@requires(["authenticated"])
def remove_files_filter(request: Request, filter: FilesFilterRequest) -> Response:
    conversation_id = filter.conversation_id
    files_filter = filter.filenames
    file_filters = ConversationAdapters.remove_files_from_filter(request.user.object, conversation_id, files_filter)
    return Response(content=json.dumps(file_filters), media_type="application/json", status_code=200)


@api_chat.post("/conversation/file-filters/bulk", response_class=Response)
@requires(["authenticated"])
def add_files_filter(request: Request, filter: FilesFilterRequest):
    try:
        conversation_id = filter.conversation_id
        files_filter = filter.filenames
        file_filters = ConversationAdapters.add_files_to_filter(request.user.object, conversation_id, files_filter)
        return Response(content=json.dumps(file_filters), media_type="application/json", status_code=200)
    except Exception as e:
        logger.error(f"Error adding file filter {filter.filename}: {e}", exc_info=True)
        raise HTTPException(status_code=422, detail=str(e))


@api_chat.post("/conversation/file-filters", response_class=Response)
@requires(["authenticated"])
def add_file_filter(request: Request, filter: FileFilterRequest):
    try:
        conversation_id = filter.conversation_id
        files_filter = [filter.filename]
        file_filters = ConversationAdapters.add_files_to_filter(request.user.object, conversation_id, files_filter)
        return Response(content=json.dumps(file_filters), media_type="application/json", status_code=200)
    except Exception as e:
        logger.error(f"Error adding file filter {filter.filename}: {e}", exc_info=True)
        raise HTTPException(status_code=422, detail=str(e))


@api_chat.delete("/conversation/file-filters", response_class=Response)
@requires(["authenticated"])
def remove_file_filter(request: Request, filter: FileFilterRequest) -> Response:
    conversation_id = filter.conversation_id
    files_filter = [filter.filename]
    file_filters = ConversationAdapters.remove_files_from_filter(request.user.object, conversation_id, files_filter)
    return Response(content=json.dumps(file_filters), media_type="application/json", status_code=200)


class FeedbackData(BaseModel):
    uquery: str
    kquery: str
    sentiment: str


@api_chat.post("/feedback")
@requires(["authenticated"])
async def sendfeedback(request: Request, data: FeedbackData):
    user: KhojUser = request.user.object
    await send_query_feedback(data.uquery, data.kquery, data.sentiment, user.email)


@api_chat.post("/speech")
@requires(["authenticated"])
async def text_to_speech(
    request: Request,
    common: CommonQueryParams,
    text: str,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=20, subscribed_requests=20, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=50, subscribed_requests=300, window=60 * 60 * 24, slug="chat_day")
    ),
) -> Response:
    voice_model = await ConversationAdapters.aget_voice_model_config(request.user.object)

    params = {"text_to_speak": text}

    if voice_model:
        params["voice_id"] = voice_model.model_id

    speech_stream = generate_text_to_speech(**params)
    return StreamingResponse(speech_stream.iter_content(chunk_size=1024), media_type="audio/mpeg")


@api_chat.get("/starters", response_class=Response)
@requires(["authenticated"])
async def chat_starters(
    request: Request,
    common: CommonQueryParams,
) -> Response:
    user: KhojUser = request.user.object
    starter_questions = await ConversationAdapters.aget_conversation_starters(user)
    return Response(content=json.dumps(starter_questions), media_type="application/json", status_code=200)


@api_chat.get("/history")
@requires(["authenticated"])
def chat_history(
    request: Request,
    common: CommonQueryParams,
    conversation_id: Optional[str] = None,
    n: Optional[int] = None,
):
    user = request.user.object
    validate_conversation_config(user)

    # Load Conversation History
    conversation = ConversationAdapters.get_conversation_by_user(
        user=user, client_application=request.user.client_app, conversation_id=conversation_id
    )

    if conversation is None:
        return Response(
            content=json.dumps({"status": "error", "message": f"Conversation: {conversation_id} not found"}),
            status_code=404,
        )

    agent_metadata = None
    if conversation.agent:
        if conversation.agent.privacy_level == Agent.PrivacyLevel.PRIVATE and conversation.agent.creator != user:
            conversation.agent = None
        else:
            agent_metadata = {
                "slug": conversation.agent.slug,
                "name": conversation.agent.name,
                "isCreator": conversation.agent.creator == user,
                "color": conversation.agent.style_color,
                "icon": conversation.agent.style_icon,
                "persona": conversation.agent.personality,
            }

    meta_log = conversation.conversation_log
    meta_log.update(
        {
            "conversation_id": conversation.id,
            "slug": conversation.title if conversation.title else conversation.slug,
            "agent": agent_metadata,
        }
    )

    if n:
        # Get latest N messages if N > 0
        if n > 0 and meta_log.get("chat"):
            meta_log["chat"] = meta_log["chat"][-n:]
        # Else return all messages except latest N
        elif n < 0 and meta_log.get("chat"):
            meta_log["chat"] = meta_log["chat"][:n]

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat_history",
        **common.__dict__,
    )

    return {"status": "ok", "response": meta_log}


@api_chat.get("/share/history")
def get_shared_chat(
    request: Request,
    common: CommonQueryParams,
    public_conversation_slug: str,
    n: Optional[int] = None,
):
    user = request.user.object if request.user.is_authenticated else None

    # Load Conversation History
    conversation = PublicConversationAdapters.get_public_conversation_by_slug(public_conversation_slug)

    if conversation is None:
        return Response(
            content=json.dumps({"status": "error", "message": f"Conversation: {public_conversation_slug} not found"}),
            status_code=404,
        )

    agent_metadata = None
    if conversation.agent:
        if conversation.agent.privacy_level == Agent.PrivacyLevel.PRIVATE:
            conversation.agent = None
        else:
            agent_metadata = {
                "slug": conversation.agent.slug,
                "name": conversation.agent.name,
                "isCreator": conversation.agent.creator == user,
                "color": conversation.agent.style_color,
                "icon": conversation.agent.style_icon,
                "persona": conversation.agent.personality,
            }

    meta_log = conversation.conversation_log
    scrubbed_title = conversation.title if conversation.title else conversation.slug

    if scrubbed_title:
        scrubbed_title = scrubbed_title.replace("-", " ")

    meta_log.update(
        {
            "conversation_id": conversation.id,
            "slug": scrubbed_title,
            "agent": agent_metadata,
        }
    )

    if n:
        # Get latest N messages if N > 0
        if n > 0 and meta_log.get("chat"):
            meta_log["chat"] = meta_log["chat"][-n:]
        # Else return all messages except latest N
        elif n < 0 and meta_log.get("chat"):
            meta_log["chat"] = meta_log["chat"][:n]

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="get_shared_chat_history",
        **common.__dict__,
    )

    return {"status": "ok", "response": meta_log}


@api_chat.delete("/history")
@requires(["authenticated"])
async def clear_chat_history(
    request: Request,
    common: CommonQueryParams,
    conversation_id: Optional[str] = None,
):
    user = request.user.object

    # Clear Conversation History
    await ConversationAdapters.adelete_conversation_by_user(user, request.user.client_app, conversation_id)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="clear_chat_history",
        **common.__dict__,
    )

    return {"status": "ok", "message": "Conversation history cleared"}


@api_chat.post("/share/fork")
@requires(["authenticated"])
def fork_public_conversation(
    request: Request,
    common: CommonQueryParams,
    public_conversation_slug: str,
):
    user = request.user.object

    # Load Conversation History
    public_conversation = PublicConversationAdapters.get_public_conversation_by_slug(public_conversation_slug)

    # Duplicate Public Conversation to User's Private Conversation
    new_conversation = ConversationAdapters.create_conversation_from_public_conversation(
        user, public_conversation, request.user.client_app
    )

    chat_metadata = {"forked_conversation": public_conversation.slug}

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="fork_public_conversation",
        **common.__dict__,
        metadata=chat_metadata,
    )

    redirect_uri = str(request.app.url_path_for("chat_page"))

    return Response(
        status_code=200,
        content=json.dumps(
            {
                "status": "ok",
                "next_url": redirect_uri,
                "conversation_id": new_conversation.id,
            }
        ),
    )


@api_chat.post("/share")
@requires(["authenticated"])
def duplicate_chat_history_public_conversation(
    request: Request,
    common: CommonQueryParams,
    conversation_id: str,
):
    user = request.user.object
    domain = request.headers.get("host")
    scheme = request.url.scheme

    # Throw unauthorized exception if domain not in ALLOWED_HOSTS
    host_domain = domain.split(":")[0]
    if host_domain not in ALLOWED_HOSTS:
        raise HTTPException(status_code=401, detail="Unauthorized domain")

    # Duplicate Conversation History to Public Conversation
    conversation = ConversationAdapters.get_conversation_by_user(user, request.user.client_app, conversation_id)
    public_conversation = ConversationAdapters.make_public_conversation_copy(conversation)
    public_conversation_url = PublicConversationAdapters.get_public_conversation_url(public_conversation)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="post_chat_share",
        **common.__dict__,
    )

    return Response(
        status_code=200, content=json.dumps({"status": "ok", "url": f"{scheme}://{domain}{public_conversation_url}"})
    )


@api_chat.get("/sessions")
@requires(["authenticated"])
def chat_sessions(
    request: Request,
    common: CommonQueryParams,
    recent: Optional[bool] = False,
):
    user = request.user.object

    # Load Conversation Sessions
    conversations = ConversationAdapters.get_conversation_sessions(user, request.user.client_app)
    if recent:
        conversations = conversations[:8]

    sessions = conversations.values_list(
        "id", "slug", "title", "agent__slug", "agent__name", "created_at", "updated_at"
    )

    session_values = [
        {
            "conversation_id": str(session[0]),
            "slug": session[2] or session[1],
            "agent_name": session[4],
            "created": session[5].strftime("%Y-%m-%d %H:%M:%S"),
            "updated": session[6].strftime("%Y-%m-%d %H:%M:%S"),
        }
        for session in sessions
    ]

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat_sessions",
        **common.__dict__,
    )

    return Response(content=json.dumps(session_values), media_type="application/json", status_code=200)


@api_chat.post("/sessions")
@requires(["authenticated"])
async def create_chat_session(
    request: Request,
    common: CommonQueryParams,
    agent_slug: Optional[str] = None,
):
    user = request.user.object

    # Create new Conversation Session
    conversation = await ConversationAdapters.acreate_conversation_session(user, request.user.client_app, agent_slug)

    response = {"conversation_id": str(conversation.id)}

    conversation_metadata = {
        "agent": agent_slug,
    }

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="create_chat_sessions",
        metadata=conversation_metadata,
        **common.__dict__,
    )

    return Response(content=json.dumps(response), media_type="application/json", status_code=200)


@api_chat.get("/options", response_class=Response)
async def chat_options(
    request: Request,
    common: CommonQueryParams,
) -> Response:
    cmd_options = {}
    for cmd in ConversationCommand:
        if cmd in command_descriptions:
            cmd_options[cmd.value] = command_descriptions[cmd]

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat_options",
        **common.__dict__,
    )
    return Response(content=json.dumps(cmd_options), media_type="application/json", status_code=200)


@api_chat.patch("/title", response_class=Response)
@requires(["authenticated"])
async def set_conversation_title(
    request: Request,
    common: CommonQueryParams,
    title: str,
    conversation_id: Optional[str] = None,
) -> Response:
    user = request.user.object
    title = title.strip()[:200]

    # Set Conversation Title
    conversation = await ConversationAdapters.aset_conversation_title(
        user, request.user.client_app, conversation_id, title
    )

    success = True if conversation else False

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_conversation_title",
        **common.__dict__,
    )

    return Response(
        content=json.dumps({"status": "ok", "success": success}), media_type="application/json", status_code=200
    )


@api_chat.post("")
@requires(["authenticated"])
async def chat(
    request: Request,
    common: CommonQueryParams,
    body: ChatRequestBody,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=60, subscribed_requests=200, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=600, subscribed_requests=6000, window=60 * 60 * 24, slug="chat_day")
    ),
    image_rate_limiter=Depends(ApiImageRateLimiter(max_images=10, max_combined_size_mb=20)),
):
    # Access the parameters from the body
    q = body.q
    n = body.n
    d = body.d
    stream = body.stream
    title = body.title
    conversation_id = body.conversation_id
    city = body.city
    region = body.region
    country = body.country or get_country_name_from_timezone(body.timezone)
    country_code = body.country_code or get_country_code_from_timezone(body.timezone)
    timezone = body.timezone
    raw_images = body.images

    async def event_generator(q: str, images: list[str]):
        start_time = time.perf_counter()
        ttft = None
        chat_metadata: dict = {}
        connection_alive = True
        user: KhojUser = request.user.object
        event_delimiter = "␃🔚␗"
        q = unquote(q)
        nonlocal conversation_id

        uploaded_images: list[str] = []
        if images:
            for image in images:
                decoded_string = unquote(image)
                base64_data = decoded_string.split(",", 1)[1]
                image_bytes = base64.b64decode(base64_data)
                webp_image_bytes = convert_image_to_webp(image_bytes)
                uploaded_image = upload_image_to_bucket(webp_image_bytes, request.user.object.id)
                if uploaded_image:
                    uploaded_images.append(uploaded_image)

        async def send_event(event_type: ChatEvent, data: str | dict):
            nonlocal connection_alive, ttft
            if not connection_alive or await request.is_disconnected():
                connection_alive = False
                logger.warning(f"User {user} disconnected from {common.client} client")
                return
            try:
                if event_type == ChatEvent.END_LLM_RESPONSE:
                    collect_telemetry()
                if event_type == ChatEvent.START_LLM_RESPONSE:
                    ttft = time.perf_counter() - start_time
                if event_type == ChatEvent.MESSAGE:
                    yield data
                elif event_type == ChatEvent.REFERENCES or stream:
                    yield json.dumps({"type": event_type.value, "data": data}, ensure_ascii=False)
            except asyncio.CancelledError as e:
                connection_alive = False
                logger.warn(f"User {user} disconnected from {common.client} client: {e}")
                return
            except Exception as e:
                connection_alive = False
                logger.error(f"Failed to stream chat API response to {user} on {common.client}: {e}", exc_info=True)
                return
            finally:
                yield event_delimiter

        async def send_llm_response(response: str):
            async for result in send_event(ChatEvent.START_LLM_RESPONSE, ""):
                yield result
            async for result in send_event(ChatEvent.MESSAGE, response):
                yield result
            async for result in send_event(ChatEvent.END_LLM_RESPONSE, ""):
                yield result

        def collect_telemetry():
            # Gather chat response telemetry
            nonlocal chat_metadata
            latency = time.perf_counter() - start_time
            cmd_set = set([cmd.value for cmd in conversation_commands])
            chat_metadata = chat_metadata or {}
            chat_metadata["conversation_command"] = cmd_set
            chat_metadata["agent"] = conversation.agent.slug if conversation.agent else None
            chat_metadata["latency"] = f"{latency:.3f}"
            chat_metadata["ttft_latency"] = f"{ttft:.3f}"

            logger.info(f"Chat response time to first token: {ttft:.3f} seconds")
            logger.info(f"Chat response total time: {latency:.3f} seconds")
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="chat",
                client=common.client,
                user_agent=request.headers.get("user-agent"),
                host=request.headers.get("host"),
                metadata=chat_metadata,
            )

        conversation_commands = [get_conversation_command(query=q, any_references=True)]

        conversation = await ConversationAdapters.aget_conversation_by_user(
            user,
            client_application=request.user.client_app,
            conversation_id=conversation_id,
            title=title,
            create_new=body.create_new,
        )
        if not conversation:
            async for result in send_llm_response(f"Conversation {conversation_id} not found"):
                yield result
            return
        conversation_id = conversation.id

        agent: Agent | None = None
        default_agent = await AgentAdapters.aget_default_agent()
        if conversation.agent and conversation.agent != default_agent:
            agent = conversation.agent

        if not conversation.agent:
            conversation.agent = default_agent
            await conversation.asave()
            agent = default_agent

        await is_ready_to_chat(user)

        user_name = await aget_user_name(user)
        location = None
        if city or region or country or country_code:
            location = LocationData(city=city, region=region, country=country, country_code=country_code)

        if is_query_empty(q):
            async for result in send_llm_response("Please ask your query to get started."):
                yield result
            return

        user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        meta_log = conversation.conversation_log
        is_automated_task = conversation_commands == [ConversationCommand.AutomatedTask]

        if conversation_commands == [ConversationCommand.Default] or is_automated_task:
            conversation_commands = await aget_relevant_information_sources(
                q,
                meta_log,
                is_automated_task,
                user=user,
                query_images=uploaded_images,
                agent=agent,
            )
            conversation_commands_str = ", ".join([cmd.value for cmd in conversation_commands])
            async for result in send_event(
                ChatEvent.STATUS, f"**Chose Data Sources to Search:** {conversation_commands_str}"
            ):
                yield result

            mode = await aget_relevant_output_modes(q, meta_log, is_automated_task, user, uploaded_images, agent)
            async for result in send_event(ChatEvent.STATUS, f"**Decided Response Mode:** {mode.value}"):
                yield result
            if mode not in conversation_commands:
                conversation_commands.append(mode)

        for cmd in conversation_commands:
            await conversation_command_rate_limiter.update_and_check_if_valid(request, cmd)
            q = q.replace(f"/{cmd.value}", "").strip()

        used_slash_summarize = conversation_commands == [ConversationCommand.Summarize]
        file_filters = conversation.file_filters if conversation else []
        # Skip trying to summarize if
        if (
            # summarization intent was inferred
            ConversationCommand.Summarize in conversation_commands
            # and not triggered via slash command
            and not used_slash_summarize
            # but we can't actually summarize
            and len(file_filters) != 1
        ):
            conversation_commands.remove(ConversationCommand.Summarize)
        elif ConversationCommand.Summarize in conversation_commands:
            response_log = ""
            agent_has_entries = await EntryAdapters.aagent_has_entries(agent)
            if len(file_filters) == 0 and not agent_has_entries:
                response_log = "No files selected for summarization. Please add files using the section on the left."
                async for result in send_llm_response(response_log):
                    yield result
            elif len(file_filters) > 1 and not agent_has_entries:
                response_log = "Only one file can be selected for summarization."
                async for result in send_llm_response(response_log):
                    yield result
            else:
                try:
                    file_object = None
                    if await EntryAdapters.aagent_has_entries(agent):
                        file_names = await EntryAdapters.aget_agent_entry_filepaths(agent)
                        if len(file_names) > 0:
                            file_object = await FileObjectAdapters.async_get_file_objects_by_name(
                                None, file_names[0], agent
                            )

                    if len(file_filters) > 0:
                        file_object = await FileObjectAdapters.async_get_file_objects_by_name(user, file_filters[0])

                    if len(file_object) == 0:
                        response_log = "Sorry, I couldn't find the full text of this file. Please re-upload the document and try again."
                        async for result in send_llm_response(response_log):
                            yield result
                        return
                    contextual_data = " ".join([file.raw_text for file in file_object])
                    if not q:
                        q = "Create a general summary of the file"
                    async for result in send_event(
                        ChatEvent.STATUS, f"**Constructing Summary Using:** {file_object[0].file_name}"
                    ):
                        yield result

                    response = await extract_relevant_summary(
                        q,
                        contextual_data,
                        conversation_history=meta_log,
                        query_images=uploaded_images,
                        user=user,
                        agent=agent,
                    )
                    response_log = str(response)
                    async for result in send_llm_response(response_log):
                        yield result
                except Exception as e:
                    response_log = "Error summarizing file. Please try again, or contact support."
                    logger.error(f"Error summarizing file for {user.email}: {e}", exc_info=True)
                    async for result in send_llm_response(response_log):
                        yield result
            await sync_to_async(save_to_conversation_log)(
                q,
                response_log,
                user,
                meta_log,
                user_message_time,
                intent_type="summarize",
                client_application=request.user.client_app,
                conversation_id=conversation_id,
                query_images=uploaded_images,
            )
            return

        custom_filters = []
        if conversation_commands == [ConversationCommand.Help]:
            if not q:
                conversation_config = await ConversationAdapters.aget_user_conversation_config(user)
                if conversation_config == None:
                    conversation_config = await ConversationAdapters.aget_default_conversation_config()
                model_type = conversation_config.model_type
                formatted_help = help_message.format(model=model_type, version=state.khoj_version, device=get_device())
                async for result in send_llm_response(formatted_help):
                    yield result
                return
            # Adding specification to search online specifically on khoj.dev pages.
            custom_filters.append("site:khoj.dev")
            conversation_commands.append(ConversationCommand.Online)

        if ConversationCommand.Automation in conversation_commands:
            try:
                automation, crontime, query_to_run, subject = await create_automation(
                    q, timezone, user, request.url, meta_log
                )
            except Exception as e:
                logger.error(f"Error scheduling task {q} for {user.email}: {e}")
                error_message = f"Unable to create automation. Ensure the automation doesn't already exist."
                async for result in send_llm_response(error_message):
                    yield result
                return

            llm_response = construct_automation_created_message(automation, crontime, query_to_run, subject)
            await sync_to_async(save_to_conversation_log)(
                q,
                llm_response,
                user,
                meta_log,
                user_message_time,
                intent_type="automation",
                client_application=request.user.client_app,
                conversation_id=conversation_id,
                inferred_queries=[query_to_run],
                automation_id=automation.id,
                query_images=uploaded_images,
            )
            async for result in send_llm_response(llm_response):
                yield result
            return

        # Gather Context
        ## Extract Document References
        compiled_references, inferred_queries, defiltered_query = [], [], q
        try:
            async for result in extract_references_and_questions(
                request,
                meta_log,
                q,
                (n or 7),
                d,
                conversation_id,
                conversation_commands,
                location,
                partial(send_event, ChatEvent.STATUS),
                query_images=uploaded_images,
                agent=agent,
            ):
                if isinstance(result, dict) and ChatEvent.STATUS in result:
                    yield result[ChatEvent.STATUS]
                else:
                    compiled_references.extend(result[0])
                    inferred_queries.extend(result[1])
                    defiltered_query = result[2]
        except Exception as e:
            error_message = f"Error searching knowledge base: {e}. Attempting to respond without document references."
            logger.error(error_message, exc_info=True)
            async for result in send_event(
                ChatEvent.STATUS, "Document search failed. I'll try respond without document references"
            ):
                yield result

        if not is_none_or_empty(compiled_references):
            headings = "\n- " + "\n- ".join(set([c.get("compiled", c).split("\n")[0] for c in compiled_references]))
            # Strip only leading # from headings
            headings = headings.replace("#", "")
            async for result in send_event(ChatEvent.STATUS, f"**Found Relevant Notes**: {headings}"):
                yield result

        online_results: Dict = dict()

        if conversation_commands == [ConversationCommand.Notes] and not await EntryAdapters.auser_has_entries(user):
            async for result in send_llm_response(f"{no_entries_found.format()}"):
                yield result
            return

        if ConversationCommand.Notes in conversation_commands and is_none_or_empty(compiled_references):
            conversation_commands.remove(ConversationCommand.Notes)

        ## Gather Online References
        if ConversationCommand.Online in conversation_commands:
            try:
                async for result in search_online(
                    defiltered_query,
                    meta_log,
                    location,
                    user,
                    partial(send_event, ChatEvent.STATUS),
                    custom_filters,
                    query_images=uploaded_images,
                    agent=agent,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        online_results = result
            except Exception as e:
                error_message = f"Error searching online: {e}. Attempting to respond without online results"
                logger.warning(error_message)
                async for result in send_event(
                    ChatEvent.STATUS, "Online search failed. I'll try respond without online references"
                ):
                    yield result

        ## Gather Webpage References
        if ConversationCommand.Webpage in conversation_commands:
            try:
                async for result in read_webpages(
                    defiltered_query,
                    meta_log,
                    location,
                    user,
                    partial(send_event, ChatEvent.STATUS),
                    query_images=uploaded_images,
                    agent=agent,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        direct_web_pages = result
                webpages = []
                for query in direct_web_pages:
                    if online_results.get(query):
                        online_results[query]["webpages"] = direct_web_pages[query]["webpages"]
                    else:
                        online_results[query] = {"webpages": direct_web_pages[query]["webpages"]}

                    for webpage in direct_web_pages[query]["webpages"]:
                        webpages.append(webpage["link"])
                async for result in send_event(ChatEvent.STATUS, f"**Read web pages**: {webpages}"):
                    yield result
            except Exception as e:
                logger.warning(
                    f"Error reading webpages: {e}. Attempting to respond without webpage results",
                    exc_info=True,
                )
                async for result in send_event(
                    ChatEvent.STATUS, "Webpage read failed. I'll try respond without webpage references"
                ):
                    yield result

        ## Send Gathered References
        async for result in send_event(
            ChatEvent.REFERENCES,
            {
                "inferredQueries": inferred_queries,
                "context": compiled_references,
                "onlineContext": online_results,
            },
        ):
            yield result

        # Generate Output
        ## Generate Image Output
        if ConversationCommand.Image in conversation_commands:
            async for result in text_to_image(
                defiltered_query,
                user,
                meta_log,
                location_data=location,
                references=compiled_references,
                online_results=online_results,
                send_status_func=partial(send_event, ChatEvent.STATUS),
                query_images=uploaded_images,
                agent=agent,
            ):
                if isinstance(result, dict) and ChatEvent.STATUS in result:
                    yield result[ChatEvent.STATUS]
                else:
                    generated_image, status_code, improved_image_prompt, intent_type = result

            if generated_image is None or status_code != 200:
                content_obj = {
                    "content-type": "application/json",
                    "intentType": intent_type,
                    "detail": improved_image_prompt,
                    "image": None,
                }
                async for result in send_llm_response(json.dumps(content_obj)):
                    yield result
                return

            await sync_to_async(save_to_conversation_log)(
                q,
                generated_image,
                user,
                meta_log,
                user_message_time,
                intent_type=intent_type,
                inferred_queries=[improved_image_prompt],
                client_application=request.user.client_app,
                conversation_id=conversation_id,
                compiled_references=compiled_references,
                online_results=online_results,
                query_images=uploaded_images,
            )
            content_obj = {
                "intentType": intent_type,
                "inferredQueries": [improved_image_prompt],
                "image": generated_image,
            }
            async for result in send_llm_response(json.dumps(content_obj)):
                yield result
            return

        if ConversationCommand.Diagram in conversation_commands:
            async for result in send_event(ChatEvent.STATUS, f"Creating diagram"):
                yield result

            intent_type = "excalidraw"
            inferred_queries = []
            diagram_description = ""

            async for result in generate_excalidraw_diagram(
                q=defiltered_query,
                conversation_history=meta_log,
                location_data=location,
                note_references=compiled_references,
                online_results=online_results,
                query_images=uploaded_images,
                user=user,
                agent=agent,
                send_status_func=partial(send_event, ChatEvent.STATUS),
            ):
                if isinstance(result, dict) and ChatEvent.STATUS in result:
                    yield result[ChatEvent.STATUS]
                else:
                    better_diagram_description_prompt, excalidraw_diagram_description = result
                    inferred_queries.append(better_diagram_description_prompt)
                    diagram_description = excalidraw_diagram_description

            content_obj = {
                "intentType": intent_type,
                "inferredQueries": inferred_queries,
                "image": diagram_description,
            }

            await sync_to_async(save_to_conversation_log)(
                q,
                excalidraw_diagram_description,
                user,
                meta_log,
                user_message_time,
                intent_type="excalidraw",
                inferred_queries=[better_diagram_description_prompt],
                client_application=request.user.client_app,
                conversation_id=conversation_id,
                compiled_references=compiled_references,
                online_results=online_results,
                query_images=uploaded_images,
            )

            async for result in send_llm_response(json.dumps(content_obj)):
                yield result
            return

        ## Generate Text Output
        async for result in send_event(ChatEvent.STATUS, f"**Generating a well-informed response**"):
            yield result
        llm_response, chat_metadata = await agenerate_chat_response(
            defiltered_query,
            meta_log,
            conversation,
            compiled_references,
            online_results,
            inferred_queries,
            conversation_commands,
            user,
            request.user.client_app,
            conversation_id,
            location,
            user_name,
            uploaded_images,
        )

        # Send Response
        async for result in send_event(ChatEvent.START_LLM_RESPONSE, ""):
            yield result

        continue_stream = True
        iterator = AsyncIteratorWrapper(llm_response)
        async for item in iterator:
            if item is None:
                async for result in send_event(ChatEvent.END_LLM_RESPONSE, ""):
                    yield result
                logger.debug("Finished streaming response")
                return
            if not connection_alive or not continue_stream:
                continue
            try:
                async for result in send_event(ChatEvent.MESSAGE, f"{item}"):
                    yield result
            except Exception as e:
                continue_stream = False
                logger.info(f"User {user} disconnected. Emitting rest of responses to clear thread: {e}")

    ## Stream Text Response
    if stream:
        return StreamingResponse(event_generator(q, images=raw_images), media_type="text/plain")
    ## Non-Streaming Text Response
    else:
        response_iterator = event_generator(q, images=raw_images)
        response_data = await read_chat_stream(response_iterator)
        return Response(content=json.dumps(response_data), media_type="application/json", status_code=200)
