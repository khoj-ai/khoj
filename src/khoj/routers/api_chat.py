import asyncio
import base64
import json
import logging
import time
import uuid
from dataclasses import dataclass
from datetime import datetime
from functools import partial
from typing import Any, Dict, List, Optional
from urllib.parse import unquote

from fastapi import (
    APIRouter,
    Depends,
    HTTPException,
    Request,
    WebSocket,
    WebSocketDisconnect,
)
from fastapi.responses import RedirectResponse, Response, StreamingResponse
from fastapi.websockets import WebSocketState
from starlette.authentication import has_required_scope, requires
from starlette.requests import URL, Headers

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
from khoj.processor.conversation.openai.utils import is_local_api
from khoj.processor.conversation.prompts import no_entries_found
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
    WebSocketConnectionManager,
    acreate_title_from_history,
    agenerate_chat_response,
    aget_data_sources_and_output_format,
    gather_raw_query_files,
    generate_mermaidjs_diagram,
    get_conversation_command,
    get_message_from_queue,
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
    is_env_var_true,
    is_none_or_empty,
    is_operator_enabled,
)
from khoj.utils.rawconfig import (
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
    # Force https upgrade if not explicitly disabled and not local host
    if scheme == "http" and not is_env_var_true("KHOJ_NO_HTTPS") and not is_local_api(f"{request.base_url}"):
        scheme = "https"

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


async def event_generator(
    body: ChatRequestBody,
    user_scope: Any,
    common: CommonQueryParams,
    headers: Headers,
    request_obj: Request | WebSocket,
    parent_interrupt_queue: asyncio.Queue = None,
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
    raw_images = body.images
    raw_query_files = body.files

    start_time = time.perf_counter()
    ttft = None
    chat_metadata: dict = {}
    conversation = None
    user: KhojUser = user_scope.object
    is_subscribed = has_required_scope(request_obj, ["premium"])
    q = unquote(q)
    defiltered_query = defilter_query(q)
    train_of_thought = []
    cancellation_event = asyncio.Event()
    child_interrupt_queue: asyncio.Queue = asyncio.Queue(maxsize=10)

    tracer: dict = {
        "mid": turn_id,
        "cid": conversation_id,
        "uid": user.id,
        "khoj_version": state.khoj_version,
    }

    uploaded_images: list[str] = []
    if raw_images:
        for image in raw_images:
            decoded_string = unquote(image)
            base64_data = decoded_string.split(",", 1)[1]
            image_bytes = base64.b64decode(base64_data)
            webp_image_bytes = convert_image_to_webp(image_bytes)
            uploaded_image = upload_user_image_to_bucket(webp_image_bytes, user.id)
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
    generated_mermaidjs_diagram: str = None
    generated_asset_results: Dict = dict()
    program_execution_context: List[str] = []
    user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Create a task to monitor for disconnections
    disconnect_monitor_task = None

    async def monitor_disconnection():
        nonlocal q, defiltered_query
        if isinstance(request_obj, Request):
            try:
                msg = await request_obj.receive()
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
                                compiled_references=compiled_references,
                                online_results=online_results,
                                code_results=code_results,
                                operator_results=operator_results,
                                research_results=research_results,
                                inferred_queries=inferred_queries,
                                client_application=user_scope.client_app,
                                conversation_id=conversation_id,
                                query_images=uploaded_images,
                                train_of_thought=train_of_thought,
                                raw_query_files=raw_query_files,
                                generated_images=generated_images,
                                generated_mermaidjs_diagram=generated_mermaidjs_diagram,
                                user_message_time=user_message_time,
                                tracer=tracer,
                            )
                        )
            except Exception as e:
                logger.error(f"Error in disconnect monitor: {e}")
        elif isinstance(request_obj, WebSocket):
            while request_obj.client_state == WebSocketState.CONNECTED and not cancellation_event.is_set():
                await asyncio.sleep(1)

                # Check if any interrupt query is received
                if interrupt_query := get_message_from_queue(parent_interrupt_queue):
                    if interrupt_query == ChatEvent.END_EVENT.value:
                        cancellation_event.set()
                        logger.debug(f"Chat cancelled by user {user} via interrupt queue.")
                    elif interrupt_query == ChatEvent.INTERRUPT.value:
                        cancellation_event.set()
                        logger.debug("Chat interrupted.")
                    else:
                        # Pass the interrupt query to child tasks
                        logger.info(f"Continuing chat with the new instruction: {interrupt_query}")
                        await child_interrupt_queue.put(interrupt_query)
                        # Append the interrupt query to the main query
                        q += f"\n\n{interrupt_query}"
                        defiltered_query += f"\n\n{defilter_query(interrupt_query)}"

            logger.debug(f"WebSocket disconnected or chat cancelled by user {user} from {common.client} client.")
            if conversation and cancellation_event.is_set():
                await asyncio.shield(
                    save_to_conversation_log(
                        q,
                        chat_response="",
                        user=user,
                        compiled_references=compiled_references,
                        online_results=online_results,
                        code_results=code_results,
                        operator_results=operator_results,
                        research_results=research_results,
                        inferred_queries=inferred_queries,
                        client_application=user_scope.client_app,
                        conversation_id=conversation_id,
                        query_images=uploaded_images,
                        train_of_thought=train_of_thought,
                        raw_query_files=raw_query_files,
                        generated_images=generated_images,
                        generated_mermaidjs_diagram=generated_mermaidjs_diagram,
                        user_message_time=user_message_time,
                        tracer=tracer,
                    )
                )

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
                    and isinstance(train_of_thought[-1]["data"], str)
                    and isinstance(data, str)
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
                yield ChatEvent.END_EVENT.value
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
            request=request_obj,
            telemetry_type="api",
            api="chat",
            client=common.client,
            user_agent=headers.get("user-agent"),
            host=headers.get("host"),
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
    if q.startswith("/automated_task"):
        q = q.replace("/automated_task", "").lstrip()
        cmds_to_rate_limit += [ConversationCommand.AutomatedTask]

    # Extract conversation command from query
    conversation_commands = [get_conversation_command(query=q)]

    conversation = await ConversationAdapters.aget_conversation_by_user(
        user,
        client_application=user_scope.client_app,
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
    chat_history = conversation.messages

    # If interrupted message in DB
    if last_message := await conversation.pop_message(interrupted=True):
        # Populate context from interrupted message
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
        logger.info(f"Loaded interrupted partial context from conversation {conversation_id}.")

    if conversation_commands == [ConversationCommand.Default]:
        try:
            chosen_io = await aget_data_sources_and_output_format(
                q,
                chat_history,
                user=user,
                query_images=uploaded_images,
                agent=agent,
                query_files=attached_file_context,
                tracer=tracer,
            )
        except ValueError as e:
            logger.error(f"Error getting data sources and output format: {e}. Falling back to default.")
            chosen_io = {"sources": [ConversationCommand.General], "output": ConversationCommand.Text}

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
            await conversation_command_rate_limiter.update_and_check_if_valid(request_obj, cmd)
            q = q.replace(f"/{cmd.value}", "").strip()
        except HTTPException as e:
            async for result in send_llm_response(str(e.detail), tracer.get("usage")):
                yield result
            return

    defiltered_query = defilter_query(q)

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
            query_files=attached_file_context,
            cancellation_event=cancellation_event,
            interrupt_queue=child_interrupt_queue,
            abort_message=ChatEvent.END_EVENT.value,
            tracer=tracer,
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
            logger.debug(f"Researched Results: {''.join(r.summarizedResult or '' for r in research_results)}")

    # Gather Context
    ## Gather Document References
    if ConversationCommand.Notes in conversation_commands:
        try:
            async for result in search_documents(
                q,
                (n or 7),
                d,
                user,
                chat_history,
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
            error_message = f"Error searching knowledge base: {e}. Attempting to respond without document references."
            logger.error(error_message, exc_info=True)
            async for result in send_event(
                ChatEvent.STATUS, "Document search failed. I'll try respond without document references"
            ):
                yield result

        if not is_none_or_empty(compiled_references):
            distinct_headings = set([d.get("compiled").split("\n")[0] for d in compiled_references if "compiled" in d])
            distinct_files = set([d["file"] for d in compiled_references])
            # Strip only leading # from headings
            headings_str = "\n- " + "\n- ".join(distinct_headings).replace("#", "")
            async for result in send_event(
                ChatEvent.STATUS,
                f"**Found {len(distinct_headings)} Notes Across {len(distinct_files)} Files**: {headings_str}",
            ):
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

    ## Run Code
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
            program_execution_context.append("Failed to run code")
            logger.warning(
                f"Failed to use code tool: {e}. Attempting to respond without code results",
                exc_info=True,
            )

    ## Operate Computer
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
                interrupt_queue=child_interrupt_queue,
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
            async for result in send_event(ChatEvent.STATUS, "Failed to generate image"):
                yield result
        else:
            generated_images.append(generated_image)

            generated_asset_results["images"] = {
                "description": improved_image_prompt,
            }

            async for result in send_event(
                ChatEvent.GENERATED_ASSETS,
                {
                    "images": [generated_image],
                },
            ):
                yield result

    if ConversationCommand.Diagram in conversation_commands:
        async for result in send_event(ChatEvent.STATUS, "Creating diagram"):
            yield result

        inferred_queries = []
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
                    generated_mermaidjs_diagram = mermaidjs_diagram_description

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
                        prompts.failed_diagram_generation.format(attempted_diagram=better_diagram_description_prompt)
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
    async for result in send_event(ChatEvent.STATUS, "**Generating a well-informed response**"):
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
        program_execution_context,
        generated_asset_results,
        is_subscribed,
        tracer,
    )

    full_response = ""
    message_start = True
    async for item in llm_response:
        # Should not happen with async generator. Skip.
        if item is None or not isinstance(item, ResponseWithThought):
            logger.warning(f"Unexpected item type in LLM response: {type(item)}. Skipping.")
            continue
        if cancellation_event.is_set():
            break
        message = item.text
        full_response += message if message else ""
        if item.thought:
            async for result in send_event(ChatEvent.THOUGHT, item.thought):
                yield result
            continue
        # Start sending response
        elif message_start:
            message_start = False
            async for result in send_event(ChatEvent.START_LLM_RESPONSE, ""):
                yield result

        try:
            async for result in send_event(ChatEvent.MESSAGE, message):
                yield result
        except Exception as e:
            if not cancellation_event.is_set():
                logger.warning(f"Error during streaming. Stopping send: {e}")
            break

    # Check if the user has disconnected
    if cancellation_event.is_set():
        logger.debug(f"Stopping LLM response to user {user} on {common.client} client.")
        # Cancel the disconnect monitor task if it is still running
        await cancel_disconnect_monitor()
        return

    # Save conversation once finish streaming
    asyncio.create_task(
        save_to_conversation_log(
            q,
            chat_response=full_response,
            user=user,
            compiled_references=compiled_references,
            online_results=online_results,
            code_results=code_results,
            operator_results=operator_results,
            research_results=research_results,
            inferred_queries=inferred_queries,
            client_application=user_scope.client_app,
            conversation_id=str(conversation.id),
            query_images=uploaded_images,
            train_of_thought=train_of_thought,
            raw_query_files=raw_query_files,
            generated_images=generated_images,
            generated_mermaidjs_diagram=generated_mermaidjs_diagram,
            tracer=tracer,
        )
    )

    # Signal end of LLM response after the loop finishes
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


@api_chat.websocket("/ws")
@requires(["authenticated"])
async def chat_ws(
    websocket: WebSocket,
    common: CommonQueryParams,
):
    # Validate WebSocket Origin
    origin = websocket.headers.get("origin")
    if not origin or URL(origin).hostname not in ALLOWED_HOSTS:
        await websocket.close(code=1008, reason="Origin not allowed")
        return

    # Limit open websocket connections per user
    user = websocket.scope["user"].object
    connection_manager = WebSocketConnectionManager(trial_user_max_connections=5, subscribed_user_max_connections=10)
    connection_id = str(uuid.uuid4())

    if not await connection_manager.can_connect(websocket):
        await websocket.close(code=1008, reason="Connection limit exceeded")
        logger.info(f"WebSocket connection rejected for user {user.id}: connection limit exceeded")
        return

    await websocket.accept()

    # Note new websocket connection for the user
    await connection_manager.register_connection(user, connection_id)

    # Initialize rate limiters
    rate_limiter_per_minute = ApiUserRateLimiter(requests=20, subscribed_requests=20, window=60, slug="chat_minute")
    rate_limiter_per_day = ApiUserRateLimiter(
        requests=100, subscribed_requests=600, window=60 * 60 * 24, slug="chat_day"
    )
    image_rate_limiter = ApiImageRateLimiter(max_images=10, max_combined_size_mb=20)

    # Shared interrupt queue for communicating interrupts to ongoing research
    interrupt_queue: asyncio.Queue = asyncio.Queue(maxsize=10)
    current_task = None

    try:
        while True:
            data = await websocket.receive_json()

            # Check if this is an interrupt message
            if data.get("type") == "interrupt":
                if current_task and not current_task.done():
                    # Send interrupt signal to the ongoing task
                    await interrupt_queue.put(data.get("query") or ChatEvent.END_EVENT.value)
                    logger.info(
                        f"Interrupt signal sent to ongoing task for user {websocket.scope['user'].object.id} with query: {data.get('query')}"
                    )
                    if data.get("query"):
                        ack_type = "interrupt_message_acknowledged"
                        await websocket.send_text(json.dumps({"type": ack_type}))
                    else:
                        ack_type = "interrupt_acknowledged"
                        await websocket.send_text(json.dumps({"type": ack_type}))
                else:
                    ack_type = "interrupt_acknowledged"
                    await websocket.send_text(json.dumps({"type": ack_type}))
                    logger.info(f"No ongoing task to interrupt for user {websocket.scope['user'].object.id}")
                continue

            # Handle regular chat messages - ensure data has required fields
            if "q" not in data:
                await websocket.send_text(json.dumps({"error": "Missing required field 'q' in chat message"}))
                continue

            body = ChatRequestBody(**data)

            # Apply rate limiting manually
            try:
                await rate_limiter_per_minute.check_websocket(websocket)
                await rate_limiter_per_day.check_websocket(websocket)
                image_rate_limiter.check_websocket(websocket, body)
            except HTTPException as e:
                await websocket.send_text(json.dumps({"error": e.detail}))
                continue

            # Cancel any ongoing task before starting a new one
            if current_task and not current_task.done():
                current_task.cancel()
                try:
                    await current_task
                except asyncio.CancelledError:
                    pass

            # Create a new task for processing the chat request
            current_task = asyncio.create_task(process_chat_request(websocket, body, common, interrupt_queue))

    except WebSocketDisconnect:
        logger.info(f"WebSocket disconnected for user {websocket.scope['user'].object.id}")
        if current_task and not current_task.done():
            interrupt_queue.put_nowait(ChatEvent.INTERRUPT.value)
    except Exception as e:
        logger.error(f"Error in websocket chat: {e}", exc_info=True)
        if current_task and not current_task.done():
            current_task.cancel()
        await websocket.close(code=1011, reason="Internal Server Error")
    finally:
        # Always unregister the connection on disconnect
        await connection_manager.unregister_connection(user, connection_id)


async def process_chat_request(
    websocket: WebSocket,
    body: ChatRequestBody,
    common: CommonQueryParams,
    interrupt_queue: asyncio.Queue,
):
    """Process a single chat request with interrupt support"""

    # Server-side message buffering for better streaming performance
    @dataclass
    class MessageBuffer:
        """Buffer for managing streamed chat messages with timing control."""

        content: str = ""
        timeout: Optional[asyncio.Task] = None
        last_flush: float = 0.0

        def __post_init__(self):
            """Initialize last_flush with current time if not provided."""
            if self.last_flush == 0.0:
                self.last_flush = time.perf_counter()

    message_buffer = MessageBuffer()
    thought_buffer = MessageBuffer()
    BUFFER_FLUSH_INTERVAL = 0.1  # 100ms buffer interval
    BUFFER_MAX_SIZE = 512  # Flush if buffer reaches this size

    async def flush_message_buffer():
        """Flush the accumulated message buffer to the client"""
        nonlocal message_buffer
        if message_buffer.content:
            buffered_content = message_buffer.content
            message_buffer.content = ""
            message_buffer.last_flush = time.perf_counter()
            if message_buffer.timeout:
                message_buffer.timeout.cancel()
                message_buffer.timeout = None
            yield buffered_content

    async def flush_thought_buffer():
        """Flush the accumulated thought buffer to the client"""
        nonlocal thought_buffer
        if thought_buffer.content:
            thought_event = json.dumps({"type": ChatEvent.THOUGHT.value, "data": thought_buffer.content})
            thought_buffer.content = ""
            thought_buffer.last_flush = time.perf_counter()
            if thought_buffer.timeout:
                thought_buffer.timeout.cancel()
                thought_buffer.timeout = None
            yield thought_event

    try:
        # Since we are using websockets, we can ignore the stream parameter and always stream
        response_iterator = event_generator(
            body,
            websocket.scope["user"],
            common,
            websocket.headers,
            websocket,
            interrupt_queue,
        )
        async for event in response_iterator:
            if event.startswith("{") and event.endswith("}"):
                evt_json = json.loads(event)
                if evt_json["type"] == ChatEvent.END_LLM_RESPONSE.value:
                    # Flush remaining buffer content on end llm response event
                    chunks = "".join([chunk async for chunk in flush_message_buffer()])
                    await websocket.send_text(chunks)
                    await websocket.send_text(ChatEvent.END_EVENT.value)
                elif evt_json["type"] == ChatEvent.THOUGHT.value:
                    # Buffer THOUGHT events for better streaming performance
                    thought_buffer.content += str(evt_json.get("data", ""))

                    # Flush if buffer is too large or enough time has passed
                    current_time = time.perf_counter()
                    should_flush_time = (current_time - thought_buffer.last_flush) >= BUFFER_FLUSH_INTERVAL
                    should_flush_size = len(thought_buffer.content) >= BUFFER_MAX_SIZE

                    if should_flush_size or should_flush_time:
                        thought_event = "".join([chunk async for chunk in flush_thought_buffer()])
                        await websocket.send_text(thought_event)
                        await websocket.send_text(ChatEvent.END_EVENT.value)
                    else:
                        # Cancel any previous timeout tasks to reset the flush timer
                        if thought_buffer.timeout:
                            thought_buffer.timeout.cancel()

                        async def delayed_thought_flush():
                            """Flush thought buffer if no new messages arrive within debounce interval."""
                            await asyncio.sleep(BUFFER_FLUSH_INTERVAL)
                            # Check if there's still content to flush
                            thought_chunks = "".join([chunk async for chunk in flush_thought_buffer()])
                            if thought_chunks:
                                thought_event = "".join([chunk async for chunk in flush_thought_buffer()])
                                await websocket.send_text(thought_event)
                                await websocket.send_text(ChatEvent.END_EVENT.value)

                        # Flush buffer if no new thoughts arrive within debounce interval
                        thought_buffer.timeout = asyncio.create_task(delayed_thought_flush())
                    continue
                await websocket.send_text(event)
                await websocket.send_text(ChatEvent.END_EVENT.value)
            elif event != ChatEvent.END_EVENT.value:
                # Buffer MESSAGE events for better streaming performance
                message_buffer.content += str(event)

                # Flush if buffer is too large or enough time has passed
                current_time = time.perf_counter()
                should_flush_time = (current_time - message_buffer.last_flush) >= BUFFER_FLUSH_INTERVAL
                should_flush_size = len(message_buffer.content) >= BUFFER_MAX_SIZE

                if should_flush_size or should_flush_time:
                    chunks = "".join([chunk async for chunk in flush_message_buffer()])
                    await websocket.send_text(chunks)
                    await websocket.send_text(ChatEvent.END_EVENT.value)
                else:
                    # Cancel any previous timeout tasks to reset the flush timer
                    if message_buffer.timeout:
                        message_buffer.timeout.cancel()

                    async def delayed_flush():
                        """Flush message buffer if no new messages arrive within debounce interval."""
                        await asyncio.sleep(BUFFER_FLUSH_INTERVAL)
                        # Check if there's still content to flush
                        chunks = "".join([chunk async for chunk in flush_message_buffer()])
                        await websocket.send_text(chunks)
                        await websocket.send_text(ChatEvent.END_EVENT.value)

                    # Flush buffer if no new messages arrive within debounce interval
                    message_buffer.timeout = asyncio.create_task(delayed_flush())
    except asyncio.CancelledError:
        logger.debug(f"Chat request cancelled for user {websocket.scope['user'].object.id}")
        raise
    except Exception as e:
        await websocket.send_text(json.dumps({"error": "Internal server error"}))
        logger.error(f"Error processing chat request: {e}", exc_info=True)
        raise


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
    response_iterator = event_generator(
        body,
        request.user,
        common,
        request.headers,
        request,
    )

    # Stream Text Response
    if body.stream:
        return StreamingResponse(response_iterator, media_type="text/plain")
    # Non-Streaming Text Response
    else:
        response_data = await read_chat_stream(response_iterator)
        return Response(content=json.dumps(response_data), media_type="application/json", status_code=200)
