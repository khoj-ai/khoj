import asyncio
import base64
import json
import logging
import time
import uuid
from datetime import datetime
from functools import partial
from typing import Any, Dict, List, Optional
from urllib.parse import unquote

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Depends, HTTPException, Request
from fastapi.responses import RedirectResponse, Response, StreamingResponse
from starlette.authentication import has_required_scope, requires

from khoj.app.settings import ALLOWED_HOSTS
from khoj.database.adapters import (
    AgentAdapters,
    ConversationAdapters,
    EntryAdapters,
    PublicConversationAdapters,
    aget_user_name,
)
from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.prompts import help_message, no_entries_found
from khoj.processor.conversation.utils import (
    OperatorRun,
    ResponseWithThought,
    defilter_query,
    save_to_conversation_log,
)
from khoj.processor.image.generate import text_to_image
from khoj.processor.operator import operate_environment
from khoj.processor.speech.text_to_speech import generate_text_to_speech
from khoj.processor.tools.online_search import (
    deduplicate_organic_results,
    read_webpages,
    search_online,
)
from khoj.processor.tools.run_code import run_code
from khoj.routers.email import send_query_feedback
from khoj.routers.helpers import (
    ApiImageRateLimiter,
    ApiUserRateLimiter,
    ChatEvent,
    ChatRequestBody,
    CommonQueryParams,
    ConversationCommandRateLimiter,
    DeleteMessageRequestBody,
    FeedbackData,
    acreate_title_from_history,
    agenerate_chat_response,
    aget_data_sources_and_output_format,
    construct_automation_created_message,
    create_automation,
    gather_raw_query_files,
    generate_mermaidjs_diagram,
    generate_summary_from_files,
    get_conversation_command,
    is_query_empty,
    is_ready_to_chat,
    read_chat_stream,
    search_documents,
    update_telemetry_state,
    validate_chat_model,
)
from khoj.routers.research import ResearchIteration, research
from khoj.routers.storage import upload_user_image_to_bucket
from khoj.utils import state
from khoj.utils.helpers import (
    ConversationCommand,
    clean_text_for_db,
    command_descriptions,
    convert_image_to_webp,
    get_country_code_from_timezone,
    get_country_name_from_timezone,
    get_device,
    is_none_or_empty,
    is_operator_enabled,
)
from khoj.utils.rawconfig import (
    ChatRequestBody,
    FileAttachment,
    FileFilterRequest,
    FilesFilterRequest,
    LocationData,
)

# Initialize Router
logger = logging.getLogger(__name__)
conversation_command_rate_limiter = ConversationCommandRateLimiter(
    trial_rate_limit=20, subscribed_rate_limit=75, slug="command"
)


api_chat = APIRouter()


@api_chat.get("/stats", response_class=Response)
@requires(["authenticated"])
def chat_stats(request: Request, common: CommonQueryParams) -> Response:
    num_conversations = ConversationAdapters.get_num_conversations(request.user.object)
    return Response(
        content=json.dumps({"num_conversations": num_conversations}), media_type="application/json", status_code=200
    )


@api_chat.get("/export", response_class=Response)
@requires(["authenticated"])
def export_conversation(request: Request, common: CommonQueryParams, page: Optional[int] = 1) -> Response:
    all_conversations = ConversationAdapters.get_all_conversations_for_export(request.user.object, page=page)
    return Response(content=json.dumps(all_conversations), media_type="application/json", status_code=200)


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
        logger.error(f"Error adding file filter {filter.filenames}: {e}", exc_info=True)
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
        ApiUserRateLimiter(requests=30, subscribed_requests=30, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=100, subscribed_requests=600, window=60 * 60 * 24, slug="chat_day")
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
    validate_chat_model(user)

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
                "is_creator": conversation.agent.creator == user,
                "color": conversation.agent.style_color,
                "icon": conversation.agent.style_icon,
                "persona": conversation.agent.personality,
                "is_hidden": conversation.agent.is_hidden,
            }

    meta_log = conversation.conversation_log
    meta_log.update(
        {
            "conversation_id": conversation.id,
            "slug": conversation.title if conversation.title else conversation.slug,
            "agent": agent_metadata,
            "is_owner": conversation.user == user,
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
        if conversation.agent.privacy_level == Agent.PrivacyLevel.PRIVATE and conversation.agent.creator != user:
            if conversation.agent.is_hidden:
                default_agent = AgentAdapters.get_default_agent()
                agent_metadata = {
                    "slug": default_agent.slug,
                    "name": default_agent.name,
                    "is_creator": False,
                    "color": default_agent.style_color,
                    "icon": default_agent.style_icon,
                    "persona": default_agent.personality,
                    "is_hidden": default_agent.is_hidden,
                }
            else:
                conversation.agent = None
        else:
            agent_metadata = {
                "slug": conversation.agent.slug,
                "name": conversation.agent.name,
                "is_creator": conversation.agent.creator == user,
                "color": conversation.agent.style_color,
                "icon": conversation.agent.style_icon,
                "persona": conversation.agent.personality,
                "is_hidden": conversation.agent.is_hidden,
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
            "is_owner": conversation.source_owner == user,
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
                "conversation_id": str(new_conversation.id),
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


@api_chat.delete("/share")
@requires(["authenticated"])
def delete_public_conversation(
    request: Request,
    common: CommonQueryParams,
    public_conversation_slug: str,
):
    user = request.user.object

    # Delete Public Conversation
    PublicConversationAdapters.delete_public_conversation_by_slug(user=user, slug=public_conversation_slug)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="delete_chat_share",
        **common.__dict__,
    )

    # Redirect to the main chat page
    redirect_uri = str(request.app.url_path_for("chat_page"))
    return RedirectResponse(
        url=redirect_uri,
        status_code=301,
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
        "id",
        "slug",
        "title",
        "agent__slug",
        "agent__name",
        "created_at",
        "updated_at",
        "agent__style_icon",
        "agent__style_color",
        "agent__is_hidden",
    )

    session_values = [
        {
            "conversation_id": str(session[0]),
            "slug": session[2] or session[1],
            "agent_name": session[4],
            "created": session[5].strftime("%Y-%m-%d %H:%M:%S"),
            "updated": session[6].strftime("%Y-%m-%d %H:%M:%S"),
            "agent_icon": session[7],
            "agent_color": session[8],
            "agent_is_hidden": session[9],
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
    # Add parameters here to create a custom hidden agent on the fly
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
        if cmd == ConversationCommand.Operator and not is_operator_enabled():
            continue
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


@api_chat.post("/title")
@requires(["authenticated"])
async def generate_chat_title(
    request: Request,
    common: CommonQueryParams,
    conversation_id: str,
):
    user: KhojUser = request.user.object
    conversation = await ConversationAdapters.aget_conversation_by_user(user=user, conversation_id=conversation_id)

    # Conversation.title is explicitly set by the user. Do not override.
    if conversation.title:
        return {"status": "ok", "title": conversation.title}

    if not conversation:
        raise HTTPException(status_code=404, detail="Conversation not found")

    new_title = await acreate_title_from_history(request.user.object, conversation=conversation)
    conversation.slug = clean_text_for_db(new_title[:200])

    await conversation.asave()

    return {"status": "ok", "title": new_title}


@api_chat.delete("/conversation/message", response_class=Response)
@requires(["authenticated"])
def delete_message(request: Request, delete_request: DeleteMessageRequestBody) -> Response:
    user = request.user.object
    success = ConversationAdapters.delete_message_by_turn_id(
        user, delete_request.conversation_id, delete_request.turn_id
    )
    if success:
        return Response(content=json.dumps({"status": "ok"}), media_type="application/json", status_code=200)
    else:
        return Response(content=json.dumps({"status": "error", "message": "Message not found"}), status_code=404)


@api_chat.post("")
@requires(["authenticated"])
async def chat(
    request: Request,
    common: CommonQueryParams,
    body: ChatRequestBody,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=20, subscribed_requests=20, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=100, subscribed_requests=600, window=60 * 60 * 24, slug="chat_day")
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
    turn_id = str(body.turn_id or uuid.uuid4())
    city = body.city
    region = body.region
    country = body.country or get_country_name_from_timezone(body.timezone)
    country_code = body.country_code or get_country_code_from_timezone(body.timezone)
    timezone = body.timezone
    raw_images = body.images
    raw_query_files = body.files
    interrupt_flag = body.interrupt

    async def event_generator(q: str, images: list[str]):
        start_time = time.perf_counter()
        ttft = None
        chat_metadata: dict = {}
        conversation = None
        user: KhojUser = request.user.object
        is_subscribed = has_required_scope(request, ["premium"])
        q = unquote(q)
        train_of_thought = []
        nonlocal conversation_id
        nonlocal raw_query_files
        cancellation_event = asyncio.Event()

        tracer: dict = {
            "mid": turn_id,
            "cid": conversation_id,
            "uid": user.id,
            "khoj_version": state.khoj_version,
        }

        uploaded_images: list[str] = []
        if images:
            for image in images:
                decoded_string = unquote(image)
                base64_data = decoded_string.split(",", 1)[1]
                image_bytes = base64.b64decode(base64_data)
                webp_image_bytes = convert_image_to_webp(image_bytes)
                uploaded_image = upload_user_image_to_bucket(webp_image_bytes, request.user.object.id)
                if not uploaded_image:
                    base64_webp_image = base64.b64encode(webp_image_bytes).decode("utf-8")
                    uploaded_image = f"data:image/webp;base64,{base64_webp_image}"
                uploaded_images.append(uploaded_image)

        query_files: Dict[str, str] = {}
        if raw_query_files:
            for file in raw_query_files:
                query_files[file.name] = file.content

        research_results: List[ResearchIteration] = []
        online_results: Dict = dict()
        code_results: Dict = dict()
        operator_results: List[OperatorRun] = []
        compiled_references: List[Any] = []
        inferred_queries: List[Any] = []
        attached_file_context = gather_raw_query_files(query_files)

        generated_images: List[str] = []
        generated_files: List[FileAttachment] = []
        generated_mermaidjs_diagram: str = None
        generated_asset_results: Dict = dict()
        program_execution_context: List[str] = []

        # Create a task to monitor for disconnections
        disconnect_monitor_task = None

        async def monitor_disconnection():
            try:
                msg = await request.receive()
                if msg["type"] == "http.disconnect":
                    logger.debug(f"Request cancelled. User {user} disconnected from {common.client} client.")
                    cancellation_event.set()
                    # ensure partial chat state saved on interrupt
                    # shield the save against task cancellation
                    if conversation:
                        await asyncio.shield(
                            save_to_conversation_log(
                                q,
                                chat_response="",
                                user=user,
                                chat_history=chat_history,
                                compiled_references=compiled_references,
                                online_results=online_results,
                                code_results=code_results,
                                operator_results=operator_results,
                                research_results=research_results,
                                inferred_queries=inferred_queries,
                                client_application=request.user.client_app,
                                conversation_id=conversation_id,
                                query_images=uploaded_images,
                                train_of_thought=train_of_thought,
                                raw_query_files=raw_query_files,
                                generated_images=generated_images,
                                raw_generated_files=generated_asset_results,
                                generated_mermaidjs_diagram=generated_mermaidjs_diagram,
                                tracer=tracer,
                            )
                        )
            except Exception as e:
                logger.error(f"Error in disconnect monitor: {e}")

        # Cancel the disconnect monitor task if it is still running
        async def cancel_disconnect_monitor():
            if disconnect_monitor_task and not disconnect_monitor_task.done():
                logger.debug(f"Cancelling disconnect monitor task for user {user}")
                disconnect_monitor_task.cancel()
                try:
                    await disconnect_monitor_task
                except asyncio.CancelledError:
                    pass

        async def send_event(event_type: ChatEvent, data: str | dict):
            nonlocal ttft, train_of_thought
            event_delimiter = "␃🔚␗"
            if cancellation_event.is_set():
                return
            try:
                if event_type == ChatEvent.END_LLM_RESPONSE:
                    collect_telemetry()
                elif event_type == ChatEvent.START_LLM_RESPONSE:
                    ttft = time.perf_counter() - start_time
                elif event_type == ChatEvent.STATUS:
                    train_of_thought.append({"type": event_type.value, "data": data})
                elif event_type == ChatEvent.THOUGHT:
                    # Append the data to the last thought as thoughts are streamed
                    if (
                        len(train_of_thought) > 0
                        and train_of_thought[-1]["type"] == ChatEvent.THOUGHT.value
                        and type(train_of_thought[-1]["data"]) == type(data) == str
                    ):
                        train_of_thought[-1]["data"] += data
                    else:
                        train_of_thought.append({"type": event_type.value, "data": data})

                if event_type == ChatEvent.MESSAGE:
                    yield data
                elif event_type == ChatEvent.REFERENCES or ChatEvent.METADATA or stream:
                    yield json.dumps({"type": event_type.value, "data": data}, ensure_ascii=False)
            except Exception as e:
                if not cancellation_event.is_set():
                    logger.error(
                        f"Failed to stream chat API response to {user} on {common.client}: {e}.",
                        exc_info=True,
                    )
            finally:
                if not cancellation_event.is_set():
                    yield event_delimiter
                # Cancel the disconnect monitor task if it is still running
                if cancellation_event.is_set() or event_type == ChatEvent.END_RESPONSE:
                    await cancel_disconnect_monitor()

        async def send_llm_response(response: str, usage: dict = None):
            # Check if the client is still connected
            if cancellation_event.is_set():
                return
            # Send Chat Response
            async for result in send_event(ChatEvent.START_LLM_RESPONSE, ""):
                yield result
            async for result in send_event(ChatEvent.MESSAGE, response):
                yield result
            async for result in send_event(ChatEvent.END_LLM_RESPONSE, ""):
                yield result
            # Send Usage Metadata once llm interactions are complete
            if usage:
                async for event in send_event(ChatEvent.USAGE, usage):
                    yield event
            async for result in send_event(ChatEvent.END_RESPONSE, ""):
                yield result

        def collect_telemetry():
            # Gather chat response telemetry
            nonlocal chat_metadata
            latency = time.perf_counter() - start_time
            cmd_set = set([cmd.value for cmd in conversation_commands])
            cost = (tracer.get("usage", {}) or {}).get("cost", 0)
            chat_metadata = chat_metadata or {}
            chat_metadata["conversation_command"] = cmd_set
            chat_metadata["agent"] = conversation.agent.slug if conversation and conversation.agent else None
            chat_metadata["cost"] = f"{cost:.5f}"
            chat_metadata["latency"] = f"{latency:.3f}"
            if ttft:
                chat_metadata["ttft_latency"] = f"{ttft:.3f}"
                logger.info(f"Chat response time to first token: {ttft:.3f} seconds")
            logger.info(f"Chat response total time: {latency:.3f} seconds")
            logger.info(f"Chat response cost: ${cost:.5f}")
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="chat",
                client=common.client,
                user_agent=request.headers.get("user-agent"),
                host=request.headers.get("host"),
                metadata=chat_metadata,
            )

        # Start the disconnect monitor in the background
        disconnect_monitor_task = asyncio.create_task(monitor_disconnection())

        if is_query_empty(q):
            async for result in send_llm_response("Please ask your query to get started.", tracer.get("usage")):
                yield result
            return

        # Automated tasks are handled before to allow mixing them with other conversation commands
        cmds_to_rate_limit = []
        is_automated_task = False
        if q.startswith("/automated_task"):
            is_automated_task = True
            q = q.replace("/automated_task", "").lstrip()
            cmds_to_rate_limit += [ConversationCommand.AutomatedTask]

        # Extract conversation command from query
        conversation_commands = [get_conversation_command(query=q)]

        conversation = await ConversationAdapters.aget_conversation_by_user(
            user,
            client_application=request.user.client_app,
            conversation_id=conversation_id,
            title=title,
            create_new=body.create_new,
        )
        if not conversation:
            async for result in send_llm_response(f"Conversation {conversation_id} not found", tracer.get("usage")):
                yield result
            return
        conversation_id = str(conversation.id)

        async for event in send_event(ChatEvent.METADATA, {"conversationId": conversation_id, "turnId": turn_id}):
            yield event

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
        user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        chat_history = conversation.messages

        # If interrupt flag is set, wait for the previous turn to be saved before proceeding
        if interrupt_flag:
            max_wait_time = 20.0  # seconds
            wait_interval = 0.3  # seconds
            wait_start = wait_current = time.time()
            while wait_current - wait_start < max_wait_time:
                # Refresh conversation to check if interrupted message saved to DB
                conversation = await ConversationAdapters.aget_conversation_by_user(
                    user,
                    client_application=request.user.client_app,
                    conversation_id=conversation_id,
                )
                if (
                    conversation
                    and conversation.messages
                    and conversation.messages[-1].by == "khoj"
                    and not conversation.messages[-1].message
                ):
                    logger.info(f"Detected interrupted message save to conversation {conversation_id}.")
                    break
                await asyncio.sleep(wait_interval)
                wait_current = time.time()

            if wait_current - wait_start >= max_wait_time:
                logger.warning(
                    f"Timeout waiting to load interrupted context from conversation {conversation_id}. Proceed without previous context."
                )

        # If interrupted message in DB
        if (
            conversation
            and conversation.messages
            and conversation.messages[-1].by == "khoj"
            and not conversation.messages[-1].message
        ):
            # Populate context from interrupted message
            last_message = conversation.messages[-1]
            online_results = {key: val.model_dump() for key, val in last_message.onlineContext.items() or []}
            code_results = {key: val.model_dump() for key, val in last_message.codeContext.items() or []}
            compiled_references = [ref.model_dump() for ref in last_message.context or []]
            research_results = [
                ResearchIteration(**iter_dict)
                for iter_dict in last_message.researchContext or []
                if iter_dict.get("summarizedResult")
            ]
            operator_results = [OperatorRun(**iter_dict) for iter_dict in last_message.operatorContext or []]
            train_of_thought = [thought.model_dump() for thought in last_message.trainOfThought or []]
            # Drop the interrupted message from conversation history
            chat_history.pop()
            logger.info(f"Loaded interrupted partial context from conversation {conversation_id}.")

        if conversation_commands == [ConversationCommand.Default]:
            try:
                chosen_io = await aget_data_sources_and_output_format(
                    q,
                    chat_history,
                    is_automated_task,
                    user=user,
                    query_images=uploaded_images,
                    agent=agent,
                    query_files=attached_file_context,
                    tracer=tracer,
                )
            except ValueError as e:
                logger.error(f"Error getting data sources and output format: {e}. Falling back to default.")
                conversation_commands = [ConversationCommand.General]

            conversation_commands = chosen_io.get("sources") + [chosen_io.get("output")]

            # If we're doing research, we don't want to do anything else
            if ConversationCommand.Research in conversation_commands:
                conversation_commands = [ConversationCommand.Research]

            conversation_commands_str = ", ".join([cmd.value for cmd in conversation_commands])
            async for result in send_event(ChatEvent.STATUS, f"**Selected Tools:** {conversation_commands_str}"):
                yield result

        cmds_to_rate_limit += conversation_commands
        for cmd in cmds_to_rate_limit:
            try:
                await conversation_command_rate_limiter.update_and_check_if_valid(request, cmd)
                q = q.replace(f"/{cmd.value}", "").strip()
            except HTTPException as e:
                async for result in send_llm_response(str(e.detail), tracer.get("usage")):
                    yield result
                return

        defiltered_query = defilter_query(q)
        file_filters = conversation.file_filters if conversation and conversation.file_filters else []

        if conversation_commands == [ConversationCommand.Research]:
            async for research_result in research(
                user=user,
                query=defiltered_query,
                conversation_id=conversation_id,
                conversation_history=chat_history,
                previous_iterations=list(research_results),
                query_images=uploaded_images,
                agent=agent,
                send_status_func=partial(send_event, ChatEvent.STATUS),
                user_name=user_name,
                location=location,
                file_filters=file_filters,
                query_files=attached_file_context,
                tracer=tracer,
                cancellation_event=cancellation_event,
            ):
                if isinstance(research_result, ResearchIteration):
                    if research_result.summarizedResult:
                        if research_result.onlineContext:
                            online_results.update(research_result.onlineContext)
                        if research_result.codeContext:
                            code_results.update(research_result.codeContext)
                        if research_result.context:
                            compiled_references.extend(research_result.context)
                    if not research_results or research_results[-1] is not research_result:
                        research_results.append(research_result)
                else:
                    yield research_result

                # Track operator results across research and operator iterations
                # This relies on two conditions:
                # 1. Check to append new (partial) operator results
                #    Relies on triggering this check on every status updates.
                #    Status updates cascade up from operator to research to chat api on every step.
                # 2. Keep operator results in sync with each research operator step
                #    Relies on python object references to ensure operator results
                #    are implicitly kept in sync after the initial append
                if (
                    research_results
                    and research_results[-1].operatorContext
                    and (not operator_results or operator_results[-1] is not research_results[-1].operatorContext)
                ):
                    operator_results.append(research_results[-1].operatorContext)

            # researched_results = await extract_relevant_info(q, researched_results, agent)
            if state.verbose > 1:
                logger.debug(f'Researched Results: {"".join(r.summarizedResult for r in research_results)}')

        # Gather Context
        ## Extract Document References
        if not ConversationCommand.Research in conversation_commands:
            try:
                async for result in search_documents(
                    user,
                    chat_history,
                    q,
                    (n or 7),
                    d,
                    conversation_id,
                    conversation_commands,
                    location,
                    partial(send_event, ChatEvent.STATUS),
                    query_images=uploaded_images,
                    agent=agent,
                    query_files=attached_file_context,
                    tracer=tracer,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        compiled_references.extend(result[0])
                        inferred_queries.extend(result[1])
                        defiltered_query = result[2]
            except Exception as e:
                error_message = (
                    f"Error searching knowledge base: {e}. Attempting to respond without document references."
                )
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

            if conversation_commands == [ConversationCommand.Notes] and not await EntryAdapters.auser_has_entries(user):
                async for result in send_llm_response(f"{no_entries_found.format()}", tracer.get("usage")):
                    yield result
                return

        if ConversationCommand.Notes in conversation_commands and is_none_or_empty(compiled_references):
            conversation_commands.remove(ConversationCommand.Notes)

        ## Gather Online References
        if ConversationCommand.Online in conversation_commands:
            try:
                async for result in search_online(
                    defiltered_query,
                    chat_history,
                    location,
                    user,
                    partial(send_event, ChatEvent.STATUS),
                    custom_filters=[],
                    max_online_searches=3,
                    query_images=uploaded_images,
                    query_files=attached_file_context,
                    agent=agent,
                    tracer=tracer,
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
                    chat_history,
                    location,
                    user,
                    partial(send_event, ChatEvent.STATUS),
                    max_webpages_to_read=1,
                    query_images=uploaded_images,
                    agent=agent,
                    query_files=attached_file_context,
                    tracer=tracer,
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

        ## Gather Code Results
        if ConversationCommand.Code in conversation_commands:
            try:
                context = f"# Iteration 1:\n#---\nNotes:\n{compiled_references}\n\nOnline Results:{online_results}"
                async for result in run_code(
                    defiltered_query,
                    chat_history,
                    context,
                    location,
                    user,
                    partial(send_event, ChatEvent.STATUS),
                    query_images=uploaded_images,
                    agent=agent,
                    query_files=attached_file_context,
                    tracer=tracer,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        code_results = result
            except ValueError as e:
                program_execution_context.append(f"Failed to run code")
                logger.warning(
                    f"Failed to use code tool: {e}. Attempting to respond without code results",
                    exc_info=True,
                )
        if ConversationCommand.Operator in conversation_commands:
            try:
                async for result in operate_environment(
                    defiltered_query,
                    user,
                    chat_history,
                    location,
                    list(operator_results)[-1] if operator_results else None,
                    query_images=uploaded_images,
                    query_files=attached_file_context,
                    send_status_func=partial(send_event, ChatEvent.STATUS),
                    agent=agent,
                    cancellation_event=cancellation_event,
                    tracer=tracer,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    elif isinstance(result, OperatorRun):
                        if not operator_results or operator_results[-1] is not result:
                            operator_results.append(result)
                        # Add webpages visited while operating browser to references
                        if result.webpages:
                            if not online_results.get(defiltered_query):
                                online_results[defiltered_query] = {"webpages": result.webpages}
                            elif not online_results[defiltered_query].get("webpages"):
                                online_results[defiltered_query]["webpages"] = result.webpages
                            else:
                                online_results[defiltered_query]["webpages"] += result.webpages
            except ValueError as e:
                program_execution_context.append(f"Browser operation error: {e}")
                logger.warning(f"Failed to operate browser with {e}", exc_info=True)
                async for result in send_event(
                    ChatEvent.STATUS, "Operating browser failed. I'll try respond appropriately"
                ):
                    yield result

        ## Send Gathered References
        unique_online_results = deduplicate_organic_results(online_results)
        async for result in send_event(
            ChatEvent.REFERENCES,
            {
                "inferredQueries": inferred_queries,
                "context": compiled_references,
                "onlineContext": unique_online_results,
                "codeContext": code_results,
            },
        ):
            yield result

        # Generate Output
        ## Generate Image Output
        if ConversationCommand.Image in conversation_commands:
            async for result in text_to_image(
                defiltered_query,
                user,
                chat_history,
                location_data=location,
                references=compiled_references,
                online_results=online_results,
                send_status_func=partial(send_event, ChatEvent.STATUS),
                query_images=uploaded_images,
                agent=agent,
                query_files=attached_file_context,
                tracer=tracer,
            ):
                if isinstance(result, dict) and ChatEvent.STATUS in result:
                    yield result[ChatEvent.STATUS]
                else:
                    generated_image, status_code, improved_image_prompt = result

            inferred_queries.append(improved_image_prompt)
            if generated_image is None or status_code != 200:
                program_execution_context.append(f"Failed to generate image with {improved_image_prompt}")
                async for result in send_event(ChatEvent.STATUS, f"Failed to generate image"):
                    yield result
            else:
                generated_images.append(generated_image)

                generated_asset_results["images"] = {
                    "query": improved_image_prompt,
                }

                async for result in send_event(
                    ChatEvent.GENERATED_ASSETS,
                    {
                        "images": [generated_image],
                    },
                ):
                    yield result

        if ConversationCommand.Diagram in conversation_commands:
            async for result in send_event(ChatEvent.STATUS, f"Creating diagram"):
                yield result

            inferred_queries = []
            diagram_description = ""

            async for result in generate_mermaidjs_diagram(
                q=defiltered_query,
                chat_history=chat_history,
                location_data=location,
                note_references=compiled_references,
                online_results=online_results,
                query_images=uploaded_images,
                user=user,
                agent=agent,
                send_status_func=partial(send_event, ChatEvent.STATUS),
                query_files=attached_file_context,
                tracer=tracer,
            ):
                if isinstance(result, dict) and ChatEvent.STATUS in result:
                    yield result[ChatEvent.STATUS]
                else:
                    better_diagram_description_prompt, mermaidjs_diagram_description = result
                    if better_diagram_description_prompt and mermaidjs_diagram_description:
                        inferred_queries.append(better_diagram_description_prompt)
                        diagram_description = mermaidjs_diagram_description

                        generated_mermaidjs_diagram = diagram_description

                        generated_asset_results["diagrams"] = {
                            "query": better_diagram_description_prompt,
                        }

                        async for result in send_event(
                            ChatEvent.GENERATED_ASSETS,
                            {
                                "mermaidjsDiagram": mermaidjs_diagram_description,
                            },
                        ):
                            yield result
                    else:
                        error_message = "Failed to generate diagram. Please try again later."
                        program_execution_context.append(
                            prompts.failed_diagram_generation.format(
                                attempted_diagram=better_diagram_description_prompt
                            )
                        )

                        async for result in send_event(ChatEvent.STATUS, error_message):
                            yield result

        # Check if the user has disconnected
        if cancellation_event.is_set():
            logger.debug(f"Stopping LLM response to user {user} on {common.client} client.")
            # Cancel the disconnect monitor task if it is still running
            await cancel_disconnect_monitor()
            return

        ## Generate Text Output
        async for result in send_event(ChatEvent.STATUS, f"**Generating a well-informed response**"):
            yield result

        llm_response, chat_metadata = await agenerate_chat_response(
            defiltered_query,
            chat_history,
            conversation,
            compiled_references,
            online_results,
            code_results,
            operator_results,
            research_results,
            user,
            location,
            user_name,
            uploaded_images,
            attached_file_context,
            generated_files,
            program_execution_context,
            generated_asset_results,
            is_subscribed,
            tracer,
        )

        full_response = ""
        async for item in llm_response:
            # Should not happen with async generator. Skip.
            if item is None or not isinstance(item, ResponseWithThought):
                logger.warning(f"Unexpected item type in LLM response: {type(item)}. Skipping.")
                continue
            if cancellation_event.is_set():
                break
            message = item.response
            full_response += message if message else ""
            if item.thought:
                async for result in send_event(ChatEvent.THOUGHT, item.thought):
                    yield result
                continue

            # Start sending response
            async for result in send_event(ChatEvent.START_LLM_RESPONSE, ""):
                yield result

            try:
                async for result in send_event(ChatEvent.MESSAGE, message):
                    yield result
            except Exception as e:
                if not cancellation_event.is_set():
                    logger.warning(f"Error during streaming. Stopping send: {e}")
                break

        # Save conversation once finish streaming
        asyncio.create_task(
            save_to_conversation_log(
                q,
                chat_response=full_response,
                user=user,
                chat_history=chat_history,
                compiled_references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                research_results=research_results,
                inferred_queries=inferred_queries,
                client_application=request.user.client_app,
                conversation_id=str(conversation.id),
                query_images=uploaded_images,
                train_of_thought=train_of_thought,
                raw_query_files=raw_query_files,
                generated_images=generated_images,
                raw_generated_files=generated_files,
                generated_mermaidjs_diagram=generated_mermaidjs_diagram,
                tracer=tracer,
            )
        )

        # Signal end of LLM response after the loop finishes
        if not cancellation_event.is_set():
            async for result in send_event(ChatEvent.END_LLM_RESPONSE, ""):
                yield result
            # Send Usage Metadata once llm interactions are complete
            if tracer.get("usage"):
                async for event in send_event(ChatEvent.USAGE, tracer.get("usage")):
                    yield event
            async for result in send_event(ChatEvent.END_RESPONSE, ""):
                yield result
            logger.debug("Finished streaming response")

        # Cancel the disconnect monitor task if it is still running
        await cancel_disconnect_monitor()

    ## Stream Text Response
    if stream:
        return StreamingResponse(event_generator(q, images=raw_images), media_type="text/plain")
    ## Non-Streaming Text Response
    else:
        response_iterator = event_generator(q, images=raw_images)
        response_data = await read_chat_stream(response_iterator)
        return Response(content=json.dumps(response_data), media_type="application/json", status_code=200)
