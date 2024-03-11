import json
import logging
import math
from typing import Dict, Optional
from urllib.parse import unquote

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Depends, Request
from fastapi.requests import Request
from fastapi.responses import Response, StreamingResponse
from starlette.authentication import requires

from khoj.database.adapters import ConversationAdapters, EntryAdapters, aget_user_name
from khoj.database.models import KhojUser
from khoj.processor.conversation.prompts import help_message, no_entries_found
from khoj.processor.conversation.utils import save_to_conversation_log
from khoj.processor.tools.online_search import search_online
from khoj.routers.api import extract_references_and_questions
from khoj.routers.helpers import (
    ApiUserRateLimiter,
    CommonQueryParams,
    ConversationCommandRateLimiter,
    agenerate_chat_response,
    aget_relevant_information_sources,
    aget_relevant_output_modes,
    get_conversation_command,
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
from khoj.utils.rawconfig import LocationData

# Initialize Router
logger = logging.getLogger(__name__)
conversation_command_rate_limiter = ConversationCommandRateLimiter(
    trial_rate_limit=2, subscribed_rate_limit=100, slug="command"
)


api_chat = APIRouter()


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

    meta_log = conversation.conversation_log
    meta_log.update(
        {"conversation_id": conversation.id, "slug": conversation.title if conversation.title else conversation.slug}
    )

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat_history",
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
):
    user = request.user.object

    # Create new Conversation Session
    conversation = await ConversationAdapters.acreate_conversation_session(user, request.user.client_app)

    response = {"conversation_id": conversation.id}

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="create_chat_sessions",
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


@api_chat.get("", response_class=Response)
@requires(["authenticated"])
async def chat(
    request: Request,
    common: CommonQueryParams,
    q: str,
    n: Optional[int] = 5,
    d: Optional[float] = 0.18,
    stream: Optional[bool] = False,
    slug: Optional[str] = None,
    conversation_id: Optional[int] = None,
    city: Optional[str] = None,
    region: Optional[str] = None,
    country: Optional[str] = None,
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=60, window=60, slug="chat_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=5, subscribed_requests=600, window=60 * 60 * 24, slug="chat_day")
    ),
) -> Response:
    user: KhojUser = request.user.object
    q = unquote(q)

    await is_ready_to_chat(user)
    conversation_commands = [get_conversation_command(query=q, any_references=True)]

    if conversation_commands == [ConversationCommand.Help]:
        conversation_config = await ConversationAdapters.aget_user_conversation_config(user)
        if conversation_config == None:
            conversation_config = await ConversationAdapters.aget_default_conversation_config()
        model_type = conversation_config.model_type
        formatted_help = help_message.format(model=model_type, version=state.khoj_version, device=get_device())
        return StreamingResponse(iter([formatted_help]), media_type="text/event-stream", status_code=200)

    meta_log = (
        await ConversationAdapters.aget_conversation_by_user(user, request.user.client_app, conversation_id, slug)
    ).conversation_log

    if conversation_commands == [ConversationCommand.Default]:
        conversation_commands = await aget_relevant_information_sources(q, meta_log)
        mode = await aget_relevant_output_modes(q, meta_log)
        if mode not in conversation_commands:
            conversation_commands.append(mode)

    for cmd in conversation_commands:
        await conversation_command_rate_limiter.update_and_check_if_valid(request, cmd)
        q = q.replace(f"/{cmd.value}", "").strip()

    location = None

    if city or region or country:
        location = LocationData(city=city, region=region, country=country)

    user_name = await aget_user_name(user)

    compiled_references, inferred_queries, defiltered_query = await extract_references_and_questions(
        request, common, meta_log, q, (n or 5), (d or math.inf), conversation_commands, location
    )
    online_results: Dict = dict()

    if conversation_commands == [ConversationCommand.Notes] and not await EntryAdapters.auser_has_entries(user):
        no_entries_found_format = no_entries_found.format()
        if stream:
            return StreamingResponse(iter([no_entries_found_format]), media_type="text/event-stream", status_code=200)
        else:
            response_obj = {"response": no_entries_found_format}
            return Response(content=json.dumps(response_obj), media_type="text/plain", status_code=200)

    if ConversationCommand.Notes in conversation_commands and is_none_or_empty(compiled_references):
        conversation_commands.remove(ConversationCommand.Notes)

    if ConversationCommand.Online in conversation_commands:
        try:
            online_results = await search_online(defiltered_query, meta_log, location)
        except ValueError as e:
            return StreamingResponse(
                iter(["Please set your SERPER_DEV_API_KEY to get started with online searches 🌐"]),
                media_type="text/event-stream",
                status_code=200,
            )

    if ConversationCommand.Image in conversation_commands:
        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="chat",
            metadata={"conversation_command": conversation_commands[0].value},
            **common.__dict__,
        )
        intent_type = "text-to-image"
        image, status_code, improved_image_prompt, image_url = await text_to_image(
            q, user, meta_log, location_data=location, references=compiled_references, online_results=online_results
        )
        if image is None:
            content_obj = {"image": image, "intentType": intent_type, "detail": improved_image_prompt}
            return Response(content=json.dumps(content_obj), media_type="application/json", status_code=status_code)

        if image_url:
            intent_type = "text-to-image2"
            image = image_url
        await sync_to_async(save_to_conversation_log)(
            q,
            image,
            user,
            meta_log,
            intent_type=intent_type,
            inferred_queries=[improved_image_prompt],
            client_application=request.user.client_app,
            conversation_id=conversation_id,
            compiled_references=compiled_references,
            online_results=online_results,
        )
        content_obj = {"image": image, "intentType": intent_type, "inferredQueries": [improved_image_prompt], "context": compiled_references, "online_results": online_results}  # type: ignore
        return Response(content=json.dumps(content_obj), media_type="application/json", status_code=status_code)

    # Get the (streamed) chat response from the LLM of choice.
    llm_response, chat_metadata = await agenerate_chat_response(
        defiltered_query,
        meta_log,
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

    cmd_set = set([cmd.value for cmd in conversation_commands])
    chat_metadata["conversation_command"] = cmd_set

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

    response_obj = {"response": actual_response, "context": compiled_references}

    return Response(content=json.dumps(response_obj), media_type="application/json", status_code=200)
