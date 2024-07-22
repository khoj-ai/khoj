import asyncio
import json
import logging
from datetime import datetime
from functools import partial
from typing import Any, Dict, List, Optional
from urllib.parse import unquote

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Depends, HTTPException, Request
from fastapi.requests import Request
from fastapi.responses import Response, StreamingResponse
from starlette.authentication import requires

from khoj.app.settings import ALLOWED_HOSTS
from khoj.database.adapters import (
    ConversationAdapters,
    DataStoreAdapters,
    EntryAdapters,
    FileObjectAdapters,
    PublicConversationAdapters,
    aget_user_name,
)
from khoj.database.models import KhojUser
from khoj.processor.conversation.prompts import (
    help_message,
    no_entries_found,
    no_notes_found,
)
from khoj.processor.conversation.utils import save_to_conversation_log
from khoj.processor.speech.text_to_speech import generate_text_to_speech
from khoj.processor.tools.online_search import read_webpages, search_online
from khoj.routers.api import extract_references_and_questions
from khoj.routers.helpers import (
    ApiUserRateLimiter,
    CommonQueryParams,
    CommonQueryParamsClass,
    ConversationCommandRateLimiter,
    agenerate_chat_response,
    aget_relevant_information_sources,
    aget_relevant_output_modes,
    construct_automation_created_message,
    create_automation,
    extract_relevant_summary,
    get_conversation_command,
    is_query_empty,
    is_ready_to_chat,
    text_to_image,
    update_telemetry_state,
    validate_conversation_config,
)
from khoj.utils import state
from khoj.utils.helpers import (
    AsyncIteratorWrapper,
    ConversationCommand,
    command_descriptions,
    get_device,
    is_none_or_empty,
)
from khoj.utils.rawconfig import FilterRequest, LocationData

# Initialize Router
logger = logging.getLogger(__name__)
conversation_command_rate_limiter = ConversationCommandRateLimiter(
    trial_rate_limit=2, subscribed_rate_limit=100, slug="command"
)


api_chat = APIRouter()

from pydantic import BaseModel

from khoj.routers.email import send_query_feedback


@api_chat.get("/conversation/file-filters/{conversation_id}", response_class=Response)
@requires(["authenticated"])
def get_file_filter(request: Request, conversation_id: str) -> Response:
    conversation = ConversationAdapters.get_conversation_by_user(
        request.user.object, conversation_id=int(conversation_id)
    )
    if not conversation:
        return Response(content=json.dumps({"status": "error", "message": "Conversation not found"}), status_code=404)

    # get all files from "computer"
    file_list = EntryAdapters.get_all_filenames_by_source(request.user.object, "computer")
    file_filters = []
    for file in conversation.file_filters:
        if file in file_list:
            file_filters.append(file)
    return Response(content=json.dumps(file_filters), media_type="application/json", status_code=200)


class FactCheckerStoreDataFormat(BaseModel):
    factToVerify: str
    response: str
    references: Any
    childReferences: List[Any]
    runId: str
    modelUsed: Dict[str, Any]


class FactCheckerStoreData(BaseModel):
    runId: str
    storeData: FactCheckerStoreDataFormat


@api_chat.post("/store/factchecker", response_class=Response)
@requires(["authenticated"])
async def store_factchecker(request: Request, common: CommonQueryParams, data: FactCheckerStoreData):
    user = request.user.object

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="store_factchecker",
        **common.__dict__,
    )
    fact_checker_key = f"factchecker_{data.runId}"
    await DataStoreAdapters.astore_data(data.storeData.model_dump_json(), fact_checker_key, user, private=False)
    return Response(content=json.dumps({"status": "ok"}), media_type="application/json", status_code=200)


@api_chat.get("/store/factchecker", response_class=Response)
async def get_factchecker(request: Request, common: CommonQueryParams, runId: str):
    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="read_factchecker",
        **common.__dict__,
    )

    fact_checker_key = f"factchecker_{runId}"

    data = await DataStoreAdapters.aretrieve_public_data(fact_checker_key)
    if data is None:
        return Response(status_code=404)
    return Response(content=json.dumps(data.value), media_type="application/json", status_code=200)


@api_chat.post("/conversation/file-filters", response_class=Response)
@requires(["authenticated"])
def add_file_filter(request: Request, filter: FilterRequest):
    try:
        conversation = ConversationAdapters.get_conversation_by_user(
            request.user.object, conversation_id=int(filter.conversation_id)
        )
        file_list = EntryAdapters.get_all_filenames_by_source(request.user.object, "computer")
        if filter.filename in file_list and filter.filename not in conversation.file_filters:
            conversation.file_filters.append(filter.filename)
            conversation.save()
        # remove files from conversation.file_filters that are not in file_list
        conversation.file_filters = [file for file in conversation.file_filters if file in file_list]
        conversation.save()
        return Response(content=json.dumps(conversation.file_filters), media_type="application/json", status_code=200)
    except Exception as e:
        logger.error(f"Error adding file filter {filter.filename}: {e}", exc_info=True)
        raise HTTPException(status_code=422, detail=str(e))


@api_chat.delete("/conversation/file-filters", response_class=Response)
@requires(["authenticated"])
def remove_file_filter(request: Request, filter: FilterRequest) -> Response:
    conversation = ConversationAdapters.get_conversation_by_user(
        request.user.object, conversation_id=int(filter.conversation_id)
    )
    if filter.filename in conversation.file_filters:
        conversation.file_filters.remove(filter.filename)
    conversation.save()
    # remove files from conversation.file_filters that are not in file_list
    file_list = EntryAdapters.get_all_filenames_by_source(request.user.object, "computer")
    conversation.file_filters = [file for file in conversation.file_filters if file in file_list]
    conversation.save()
    return Response(content=json.dumps(conversation.file_filters), media_type="application/json", status_code=200)


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
@requires(["authenticated", "premium"])
async def text_to_speech(
    request: Request,
    common: CommonQueryParams,
    text: str,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=20, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=300, window=60 * 60 * 24, slug="chat_day")
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
    conversation_id: Optional[int] = None,
    n: Optional[int] = None,
):
    user = request.user.object
    validate_conversation_config()

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
        agent_metadata = {
            "slug": conversation.agent.slug,
            "name": conversation.agent.name,
            "avatar": conversation.agent.avatar,
            "isCreator": conversation.agent.creator == user,
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
        agent_metadata = {
            "slug": conversation.agent.slug,
            "name": conversation.agent.name,
            "avatar": conversation.agent.avatar,
            "isCreator": conversation.agent.creator == user,
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
        api="public_conversation_history",
        **common.__dict__,
    )

    return {"status": "ok", "response": meta_log}


@api_chat.delete("/history")
@requires(["authenticated"])
async def clear_chat_history(
    request: Request,
    common: CommonQueryParams,
    conversation_id: Optional[int] = None,
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
    ConversationAdapters.create_conversation_from_public_conversation(
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

    return Response(status_code=200, content=json.dumps({"status": "ok", "next_url": redirect_uri}))


@api_chat.post("/share")
@requires(["authenticated"])
def duplicate_chat_history_public_conversation(
    request: Request,
    common: CommonQueryParams,
    conversation_id: int,
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
):
    user = request.user.object

    # Load Conversation Sessions
    sessions = ConversationAdapters.get_conversation_sessions(user, request.user.client_app).values_list(
        "id", "slug", "title"
    )

    session_values = [{"conversation_id": session[0], "slug": session[2] or session[1]} for session in sessions]

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

    response = {"conversation_id": conversation.id}

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
@requires(["authenticated"])
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
    conversation_id: Optional[int] = None,
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


@api_chat.get("/stream")
async def stream_chat(
    request: Request,
    common: CommonQueryParams,
    q: str,
    n: int = 7,
    d: float = 0.18,
    title: Optional[str] = None,
    conversation_id: Optional[int] = None,
    city: Optional[str] = None,
    region: Optional[str] = None,
    country: Optional[str] = None,
    timezone: Optional[str] = None,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=60, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=600, window=60 * 60 * 24, slug="chat_day")
    ),
):
    async def event_generator(q: str):
        connection_alive = True
        user: KhojUser = request.user.object
        q = unquote(q)

        async def send_event(event_type: str, data: str):
            nonlocal connection_alive
            if not connection_alive or await request.is_disconnected():
                connection_alive = False
                logger.warn(f"User {user} disconnected from {common.client} client")
                return
            try:
                if event_type == "message":
                    yield data
                else:
                    yield json.dumps({"type": event_type, "data": data})
            except asyncio.CancelledError:
                connection_alive = False
                logger.warn(f"User {user} disconnected from {common.client} client")
                return
            except Exception as e:
                connection_alive = False
                logger.error(f"Failed to stream chat API response to {user} on {common.client}: {e}", exc_info=True)
                return

        async def send_llm_response(response: str):
            async for result in send_event("start_llm_response", ""):
                yield result
            async for result in send_event("message", response):
                yield result
            async for result in send_event("end_llm_response", ""):
                yield result

        conversation = await ConversationAdapters.aget_conversation_by_user(
            user, client_application=request.user.client_app, conversation_id=conversation_id, title=title
        )
        if not conversation:
            async for result in send_llm_response(f"No Conversation id: {conversation_id}  not found"):
                yield result

        await is_ready_to_chat(user)

        user_name = await aget_user_name(user)
        location = None
        if city or region or country:
            location = LocationData(city=city, region=region, country=country)

        if is_query_empty(q):
            async for result in send_llm_response("Please ask your query to get started."):
                yield result
            return

        user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        conversation_commands = [get_conversation_command(query=q, any_references=True)]

        async for result in send_event("status", f"**👀 Understanding Query**: {q}"):
            yield result

        meta_log = conversation.conversation_log
        is_automated_task = conversation_commands == [ConversationCommand.AutomatedTask]

        if conversation_commands == [ConversationCommand.Default] or is_automated_task:
            conversation_commands = await aget_relevant_information_sources(q, meta_log, is_automated_task)
            conversation_commands_str = ", ".join([cmd.value for cmd in conversation_commands])
            async for result in send_event(
                "status", f"**🗃️ Chose Data Sources to Search:** {conversation_commands_str}"
            ):
                yield result

            mode = await aget_relevant_output_modes(q, meta_log, is_automated_task)
            async for result in send_event("status", f"**🧑🏾‍💻 Decided Response Mode:** {mode.value}"):
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
            if len(file_filters) == 0:
                response_log = "No files selected for summarization. Please add files using the section on the left."
                async for result in send_llm_response(response_log):
                    yield result
            elif len(file_filters) > 1:
                response_log = "Only one file can be selected for summarization."
                async for result in send_llm_response(response_log):
                    yield result
            else:
                try:
                    file_object = await FileObjectAdapters.async_get_file_objects_by_name(user, file_filters[0])
                    if len(file_object) == 0:
                        response_log = "Sorry, we couldn't find the full text of this file. Please re-upload the document and try again."
                        async for result in send_llm_response(response_log):
                            yield result
                        return
                    contextual_data = " ".join([file.raw_text for file in file_object])
                    if not q:
                        q = "Create a general summary of the file"
                    async for result in send_event(
                        "status", f"**🧑🏾‍💻 Constructing Summary Using:** {file_object[0].file_name}"
                    ):
                        yield result

                    response = await extract_relevant_summary(q, contextual_data)
                    response_log = str(response)
                    async for result in send_llm_response(response_log):
                        yield result
                except Exception as e:
                    response_log = "Error summarizing file."
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
            )
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="chat",
                metadata={"conversation_command": conversation_commands[0].value},
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
            )
            common = CommonQueryParamsClass(
                client=request.user.client_app,
                user_agent=request.headers.get("user-agent"),
                host=request.headers.get("host"),
            )
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="chat",
                **common.__dict__,
            )
            async for result in send_llm_response(llm_response):
                yield result
            return

        compiled_references, inferred_queries, defiltered_query = [], [], None
        async for result in extract_references_and_questions(
            request,
            meta_log,
            q,
            (n or 7),
            (d or 0.18),
            conversation_id,
            conversation_commands,
            location,
            partial(send_event, "status"),
        ):
            if isinstance(result, dict) and "status" in result:
                yield result["status"]
            else:
                compiled_references.extend(result[0])
                inferred_queries.extend(result[1])
                defiltered_query = result[2]

        if not is_none_or_empty(compiled_references):
            headings = "\n- " + "\n- ".join(set([c.get("compiled", c).split("\n")[0] for c in compiled_references]))
            async for result in send_event("status", f"**📜 Found Relevant Notes**: {headings}"):
                yield result

        online_results: Dict = dict()

        if conversation_commands == [ConversationCommand.Notes] and not await EntryAdapters.auser_has_entries(user):
            async for result in send_llm_response(f"{no_entries_found.format()}"):
                yield result
            return

        if ConversationCommand.Notes in conversation_commands and is_none_or_empty(compiled_references):
            conversation_commands.remove(ConversationCommand.Notes)

        if ConversationCommand.Online in conversation_commands:
            try:
                async for result in search_online(
                    defiltered_query, meta_log, location, partial(send_event, "status"), custom_filters
                ):
                    if isinstance(result, dict) and "status" in result:
                        yield result["status"]
                    else:
                        online_results = result
            except ValueError as e:
                error_message = f"Error searching online: {e}. Attempting to respond without online results"
                logger.warning(error_message)
                async for result in send_llm_response(error_message):
                    yield result
                return

        if ConversationCommand.Webpage in conversation_commands:
            try:
                async for result in read_webpages(defiltered_query, meta_log, location, partial(send_event, "status")):
                    if isinstance(result, dict) and "status" in result:
                        yield result["status"]
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
                async for result in send_event("status", f"**📚 Read web pages**: {webpages}"):
                    yield result
            except ValueError as e:
                logger.warning(
                    f"Error directly reading webpages: {e}. Attempting to respond without online results",
                    exc_info=True,
                )

        if ConversationCommand.Image in conversation_commands:
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="chat",
                metadata={"conversation_command": conversation_commands[0].value},
            )
            async for result in text_to_image(
                q,
                user,
                meta_log,
                location_data=location,
                references=compiled_references,
                online_results=online_results,
                send_status_func=partial(send_event, "status"),
            ):
                if isinstance(result, dict) and "status" in result:
                    yield result["status"]
                else:
                    image, status_code, improved_image_prompt, intent_type = result

            if image is None or status_code != 200:
                content_obj = {
                    "content-type": "application/json",
                    "intentType": intent_type,
                    "detail": improved_image_prompt,
                    "image": image,
                }
                async for result in send_llm_response(json.dumps(content_obj)):
                    yield result
                return

            await sync_to_async(save_to_conversation_log)(
                q,
                image,
                user,
                meta_log,
                user_message_time,
                intent_type=intent_type,
                inferred_queries=[improved_image_prompt],
                client_application=request.user.client_app,
                conversation_id=conversation_id,
                compiled_references=compiled_references,
                online_results=online_results,
            )
            content_obj = {
                "content-type": "application/json",
                "intentType": intent_type,
                "context": compiled_references,
                "online_results": online_results,
                "inferredQueries": [improved_image_prompt],
                "image": image,
            }
            async for result in send_llm_response(json.dumps(content_obj)):
                yield result
            return

        async for result in send_event(
            "references", json.dumps({"context": compiled_references, "online_results": online_results})
        ):
            yield result

        async for result in send_event("status", f"**💭 Generating a well-informed response**"):
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
        )

        chat_metadata["agent"] = conversation.agent.slug if conversation.agent else None

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="chat",
            metadata=chat_metadata,
        )
        iterator = AsyncIteratorWrapper(llm_response)

        async for result in send_event("start_llm_response", ""):
            yield result

        continue_stream = True
        async for item in iterator:
            if item is None:
                async for result in send_event("end_llm_response", ""):
                    yield result
                logger.debug("Finished streaming response")
                return
            if not connection_alive or not continue_stream:
                continue
            # Stop streaming after compiled references section of response starts
            # References are being processed via the references event rather than the message event
            if "### compiled references:" in item:
                continue_stream = False
                item = item.split("### compiled references:")[0]
            try:
                async for result in send_event("message", f"{item}"):
                    yield result
            except Exception as e:
                continue_stream = False
                logger.info(f"User {user} disconnected. Emitting rest of responses to clear thread: {e}")

    return StreamingResponse(event_generator(q), media_type="text/plain")


@api_chat.get("", response_class=Response)
@requires(["authenticated"])
async def chat(
    request: Request,
    common: CommonQueryParams,
    q: str,
    n: Optional[int] = 5,
    d: Optional[float] = 0.22,
    stream: Optional[bool] = False,
    title: Optional[str] = None,
    conversation_id: Optional[int] = None,
    city: Optional[str] = None,
    region: Optional[str] = None,
    country: Optional[str] = None,
    timezone: Optional[str] = None,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=60, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=600, window=60 * 60 * 24, slug="chat_day")
    ),
) -> Response:
    user: KhojUser = request.user.object
    q = unquote(q)
    if is_query_empty(q):
        return Response(
            content="It seems like your query is incomplete. Could you please provide more details or specify what you need help with?",
            media_type="text/plain",
            status_code=400,
        )
    user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    logger.info(f"Chat request by {user.username}: {q}")

    await is_ready_to_chat(user)
    conversation_commands = [get_conversation_command(query=q, any_references=True)]

    _custom_filters = []
    if conversation_commands == [ConversationCommand.Help]:
        help_str = "/" + ConversationCommand.Help
        if q.strip() == help_str:
            conversation_config = await ConversationAdapters.aget_user_conversation_config(user)
            if conversation_config == None:
                conversation_config = await ConversationAdapters.aget_default_conversation_config()
            model_type = conversation_config.model_type
            formatted_help = help_message.format(model=model_type, version=state.khoj_version, device=get_device())
            return StreamingResponse(iter([formatted_help]), media_type="text/event-stream", status_code=200)
        # Adding specification to search online specifically on khoj.dev pages.
        _custom_filters.append("site:khoj.dev")
        conversation_commands.append(ConversationCommand.Online)

    conversation = await ConversationAdapters.aget_conversation_by_user(
        user, request.user.client_app, conversation_id, title
    )
    conversation_id = conversation.id if conversation else None

    if not conversation:
        return Response(
            content=f"No conversation found with requested id, title", media_type="text/plain", status_code=400
        )
    else:
        meta_log = conversation.conversation_log

    if ConversationCommand.Summarize in conversation_commands:
        file_filters = conversation.file_filters
        llm_response = ""
        if len(file_filters) == 0:
            llm_response = "No files selected for summarization. Please add files using the section on the left."
        elif len(file_filters) > 1:
            llm_response = "Only one file can be selected for summarization."
        else:
            try:
                file_object = await FileObjectAdapters.async_get_file_objects_by_name(user, file_filters[0])
                if len(file_object) == 0:
                    llm_response = "Sorry, we couldn't find the full text of this file. Please re-upload the document and try again."
                    return StreamingResponse(content=llm_response, media_type="text/event-stream", status_code=200)
                contextual_data = " ".join([file.raw_text for file in file_object])
                summarizeStr = "/" + ConversationCommand.Summarize
                if q.strip() == summarizeStr:
                    q = "Create a general summary of the file"
                response = await extract_relevant_summary(q, contextual_data)
                llm_response = str(response)
            except Exception as e:
                logger.error(f"Error summarizing file for {user.email}: {e}")
                llm_response = "Error summarizing file."
        await sync_to_async(save_to_conversation_log)(
            q,
            llm_response,
            user,
            conversation.conversation_log,
            user_message_time,
            intent_type="summarize",
            client_application=request.user.client_app,
            conversation_id=conversation_id,
        )
        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="chat",
            metadata={"conversation_command": conversation_commands[0].value},
            **common.__dict__,
        )
        return StreamingResponse(content=llm_response, media_type="text/event-stream", status_code=200)

    is_automated_task = conversation_commands == [ConversationCommand.AutomatedTask]

    if conversation_commands == [ConversationCommand.Default] or is_automated_task:
        conversation_commands = await aget_relevant_information_sources(q, meta_log, is_automated_task)
        mode = await aget_relevant_output_modes(q, meta_log, is_automated_task)
        if mode not in conversation_commands:
            conversation_commands.append(mode)

    for cmd in conversation_commands:
        await conversation_command_rate_limiter.update_and_check_if_valid(request, cmd)
        q = q.replace(f"/{cmd.value}", "").strip()

    location = None

    if city or region or country:
        location = LocationData(city=city, region=region, country=country)

    user_name = await aget_user_name(user)

    if ConversationCommand.Automation in conversation_commands:
        try:
            automation, crontime, query_to_run, subject = await create_automation(
                q, timezone, user, request.url, meta_log
            )
        except Exception as e:
            logger.error(f"Error creating automation {q} for {user.email}: {e}", exc_info=True)
            return Response(
                content=f"Unable to create automation. Ensure the automation doesn't already exist.",
                media_type="text/plain",
                status_code=500,
            )

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
        )

        if stream:
            return StreamingResponse(llm_response, media_type="text/event-stream", status_code=200)
        else:
            return Response(content=llm_response, media_type="text/plain", status_code=200)

    compiled_references, inferred_queries, defiltered_query = await extract_references_and_questions(
        request, meta_log, q, (n or 5), (d or math.inf), conversation_id, conversation_commands, location
    )
    online_results: Dict[str, Dict] = {}

    if conversation_commands == [ConversationCommand.Notes] and not await EntryAdapters.auser_has_entries(user):
        no_entries_found_format = no_entries_found.format()
        if stream:
            return StreamingResponse(iter([no_entries_found_format]), media_type="text/event-stream", status_code=200)
        else:
            response_obj = {"response": no_entries_found_format}
            return Response(content=json.dumps(response_obj), media_type="text/plain", status_code=200)

    if conversation_commands == [ConversationCommand.Notes] and is_none_or_empty(compiled_references):
        no_notes_found_format = no_notes_found.format()
        if stream:
            return StreamingResponse(iter([no_notes_found_format]), media_type="text/event-stream", status_code=200)
        else:
            response_obj = {"response": no_notes_found_format}
            return Response(content=json.dumps(response_obj), media_type="text/plain", status_code=200)

    if ConversationCommand.Notes in conversation_commands and is_none_or_empty(compiled_references):
        conversation_commands.remove(ConversationCommand.Notes)

    if ConversationCommand.Online in conversation_commands:
        try:
            online_results = await search_online(defiltered_query, meta_log, location, custom_filters=_custom_filters)
        except ValueError as e:
            logger.warning(f"Error searching online: {e}. Attempting to respond without online results")

    if ConversationCommand.Webpage in conversation_commands:
        try:
            online_results = await read_webpages(defiltered_query, meta_log, location)
        except ValueError as e:
            logger.warning(
                f"Error directly reading webpages: {e}. Attempting to respond without online results", exc_info=True
            )

    if ConversationCommand.Image in conversation_commands:
        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="chat",
            metadata={"conversation_command": conversation_commands[0].value},
            **common.__dict__,
        )
        image, status_code, improved_image_prompt, intent_type = await text_to_image(
            q, user, meta_log, location_data=location, references=compiled_references, online_results=online_results
        )
        if image is None:
            content_obj = {"image": image, "intentType": intent_type, "detail": improved_image_prompt}
            return Response(content=json.dumps(content_obj), media_type="application/json", status_code=status_code)

        await sync_to_async(save_to_conversation_log)(
            q,
            image,
            user,
            meta_log,
            user_message_time,
            intent_type=intent_type,
            inferred_queries=[improved_image_prompt],
            client_application=request.user.client_app,
            conversation_id=conversation.id,
            compiled_references=compiled_references,
            online_results=online_results,
        )
        content_obj = {"image": image, "intentType": intent_type, "inferredQueries": [improved_image_prompt], "context": compiled_references, "online_results": online_results}  # type: ignore
        return Response(content=json.dumps(content_obj), media_type="application/json", status_code=status_code)

    # Get the (streamed) chat response from the LLM of choice.
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
        conversation.id,
        location,
        user_name,
    )

    cmd_set = set([cmd.value for cmd in conversation_commands])
    chat_metadata["conversation_command"] = cmd_set
    chat_metadata["agent"] = conversation.agent.slug if conversation.agent else None

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat",
        metadata=chat_metadata,
        **common.__dict__,
    )

    if llm_response is None:
        return Response(content=llm_response, media_type="text/plain", status_code=500)

    if stream:
        return StreamingResponse(llm_response, media_type="text/event-stream", status_code=200)

    iterator = AsyncIteratorWrapper(llm_response)

    # Get the full response from the generator if the stream is not requested.
    aggregated_gpt_response = ""
    async for item in iterator:
        if item is None:
            break
        aggregated_gpt_response += item

    actual_response = aggregated_gpt_response.split("### compiled references:")[0]

    response_obj = {
        "response": actual_response,
        "inferredQueries": inferred_queries,
        "context": compiled_references,
        "online_results": online_results,
    }

    return Response(content=json.dumps(response_obj), media_type="application/json", status_code=200)
