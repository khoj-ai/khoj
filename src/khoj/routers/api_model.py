import json
import logging
from typing import Dict, Optional, Union

from fastapi import APIRouter, HTTPException, Request
from fastapi.requests import Request
from fastapi.responses import Response
from starlette.authentication import has_required_scope, requires

from khoj.database import adapters
from khoj.database.adapters import ConversationAdapters, EntryAdapters
from khoj.routers.helpers import update_telemetry_state

api_model = APIRouter()
logger = logging.getLogger(__name__)


@api_model.get("/chat/options", response_model=Dict[str, Union[str, int]])
def get_chat_model_options(
    request: Request,
    client: Optional[str] = None,
):
    conversation_options = ConversationAdapters.get_conversation_processor_options().all()

    all_conversation_options = list()
    for conversation_option in conversation_options:
        all_conversation_options.append({"chat_model": conversation_option.chat_model, "id": conversation_option.id})

    return Response(content=json.dumps(all_conversation_options), media_type="application/json", status_code=200)


@api_model.get("/chat")
@requires(["authenticated"])
def get_user_chat_model(
    request: Request,
    client: Optional[str] = None,
):
    user = request.user.object

    chat_model = ConversationAdapters.get_conversation_config(user)

    if chat_model is None:
        chat_model = ConversationAdapters.get_default_conversation_config()

    return Response(status_code=200, content=json.dumps({"id": chat_model.id, "chat_model": chat_model.chat_model}))


@api_model.post("/chat", status_code=200)
@requires(["authenticated", "premium"])
async def update_chat_model(
    request: Request,
    id: str,
    client: Optional[str] = None,
):
    user = request.user.object

    new_config = await ConversationAdapters.aset_user_conversation_processor(user, int(id))

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_conversation_chat_model",
        client=client,
        metadata={"processor_conversation_type": "conversation"},
    )

    if new_config is None:
        return {"status": "error", "message": "Model not found"}

    return {"status": "ok"}


@api_model.post("/voice", status_code=200)
@requires(["authenticated", "premium"])
async def update_voice_model(
    request: Request,
    id: str,
    client: Optional[str] = None,
):
    user = request.user.object

    new_config = await ConversationAdapters.aset_user_voice_model(user, id)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_voice_model",
        client=client,
    )

    if new_config is None:
        return Response(status_code=404, content=json.dumps({"status": "error", "message": "Model not found"}))

    return Response(status_code=202, content=json.dumps({"status": "ok"}))


@api_model.post("/search", status_code=200)
@requires(["authenticated"])
async def update_search_model(
    request: Request,
    id: str,
    client: Optional[str] = None,
):
    user = request.user.object

    prev_config = await adapters.aget_user_search_model(user)
    new_config = await adapters.aset_user_search_model(user, int(id))

    if prev_config and int(id) != prev_config.id and new_config:
        await EntryAdapters.adelete_all_entries(user)

    if not prev_config:
        # If the use was just using the default config, delete all the entries and set the new config.
        await EntryAdapters.adelete_all_entries(user)

    if new_config is None:
        return {"status": "error", "message": "Model not found"}
    else:
        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_search_model",
            client=client,
            metadata={"search_model": new_config.setting.name},
        )

    return {"status": "ok"}


@api_model.post("/paint", status_code=200)
@requires(["authenticated"])
async def update_paint_model(
    request: Request,
    id: str,
    client: Optional[str] = None,
):
    user = request.user.object
    subscribed = has_required_scope(request, ["premium"])

    if not subscribed:
        raise HTTPException(status_code=403, detail="User is not subscribed to premium")

    new_config = await ConversationAdapters.aset_user_text_to_image_model(user, int(id))

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_paint_model",
        client=client,
        metadata={"paint_model": new_config.setting.model_name},
    )

    if new_config is None:
        return {"status": "error", "message": "Model not found"}

    return {"status": "ok"}
