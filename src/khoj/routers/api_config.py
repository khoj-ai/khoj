import json
import logging
import math
from typing import Dict, List, Optional, Union

from asgiref.sync import sync_to_async
from fastapi import APIRouter, HTTPException, Request
from fastapi.requests import Request
from fastapi.responses import Response
from starlette.authentication import has_required_scope, requires

from khoj.database import adapters
from khoj.database.adapters import ConversationAdapters, EntryAdapters
from khoj.database.models import Entry as DbEntry
from khoj.database.models import (
    GithubConfig,
    KhojUser,
    LocalMarkdownConfig,
    LocalOrgConfig,
    LocalPdfConfig,
    LocalPlaintextConfig,
    NotionConfig,
    Subscription,
)
from khoj.routers.helpers import CommonQueryParams, update_telemetry_state
from khoj.utils import constants, state
from khoj.utils.rawconfig import (
    FullConfig,
    GithubContentConfig,
    NotionContentConfig,
    SearchConfig,
)
from khoj.utils.state import SearchType

api_config = APIRouter()
logger = logging.getLogger(__name__)


def map_config_to_object(content_source: str):
    if content_source == DbEntry.EntrySource.GITHUB:
        return GithubConfig
    if content_source == DbEntry.EntrySource.NOTION:
        return NotionConfig
    if content_source == DbEntry.EntrySource.COMPUTER:
        return "Computer"


async def map_config_to_db(config: FullConfig, user: KhojUser):
    if config.content_type:
        if config.content_type.org:
            await LocalOrgConfig.objects.filter(user=user).adelete()
            await LocalOrgConfig.objects.acreate(
                input_files=config.content_type.org.input_files,
                input_filter=config.content_type.org.input_filter,
                index_heading_entries=config.content_type.org.index_heading_entries,
                user=user,
            )
        if config.content_type.markdown:
            await LocalMarkdownConfig.objects.filter(user=user).adelete()
            await LocalMarkdownConfig.objects.acreate(
                input_files=config.content_type.markdown.input_files,
                input_filter=config.content_type.markdown.input_filter,
                index_heading_entries=config.content_type.markdown.index_heading_entries,
                user=user,
            )
        if config.content_type.pdf:
            await LocalPdfConfig.objects.filter(user=user).adelete()
            await LocalPdfConfig.objects.acreate(
                input_files=config.content_type.pdf.input_files,
                input_filter=config.content_type.pdf.input_filter,
                index_heading_entries=config.content_type.pdf.index_heading_entries,
                user=user,
            )
        if config.content_type.plaintext:
            await LocalPlaintextConfig.objects.filter(user=user).adelete()
            await LocalPlaintextConfig.objects.acreate(
                input_files=config.content_type.plaintext.input_files,
                input_filter=config.content_type.plaintext.input_filter,
                index_heading_entries=config.content_type.plaintext.index_heading_entries,
                user=user,
            )
        if config.content_type.github:
            await adapters.set_user_github_config(
                user=user,
                pat_token=config.content_type.github.pat_token,
                repos=config.content_type.github.repos,
            )
        if config.content_type.notion:
            await adapters.set_notion_config(
                user=user,
                token=config.content_type.notion.token,
            )


def _initialize_config():
    if state.config is None:
        state.config = FullConfig()
        state.config.search_type = SearchConfig.model_validate(constants.default_config["search-type"])


@api_config.post("/data/content-source/github", status_code=200)
@requires(["authenticated"])
async def set_content_config_github_data(
    request: Request,
    updated_config: Union[GithubContentConfig, None],
    client: Optional[str] = None,
):
    _initialize_config()

    user = request.user.object

    try:
        await adapters.set_user_github_config(
            user=user,
            pat_token=updated_config.pat_token,
            repos=updated_config.repos,
        )
    except Exception as e:
        logger.error(e, exc_info=True)
        raise HTTPException(status_code=500, detail="Failed to set Github config")

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_content_config",
        client=client,
        metadata={"content_type": "github"},
    )

    return {"status": "ok"}


@api_config.post("/data/content-source/notion", status_code=200)
@requires(["authenticated"])
async def set_content_config_notion_data(
    request: Request,
    updated_config: Union[NotionContentConfig, None],
    client: Optional[str] = None,
):
    _initialize_config()

    user = request.user.object

    try:
        await adapters.set_notion_config(
            user=user,
            token=updated_config.token,
        )
    except Exception as e:
        logger.error(e, exc_info=True)
        raise HTTPException(status_code=500, detail="Failed to set Github config")

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="set_content_config",
        client=client,
        metadata={"content_type": "notion"},
    )

    return {"status": "ok"}


@api_config.delete("/data/content-source/{content_source}", status_code=200)
@requires(["authenticated"])
async def remove_content_source_data(
    request: Request,
    content_source: str,
    client: Optional[str] = None,
):
    user = request.user.object

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="delete_content_config",
        client=client,
        metadata={"content_source": content_source},
    )

    content_object = map_config_to_object(content_source)
    if content_object is None:
        raise ValueError(f"Invalid content source: {content_source}")
    elif content_object != "Computer":
        await content_object.objects.filter(user=user).adelete()
    await sync_to_async(EntryAdapters.delete_all_entries)(user, content_source)

    enabled_content = await sync_to_async(EntryAdapters.get_unique_file_types)(user)
    return {"status": "ok"}


@api_config.delete("/data/file", status_code=200)
@requires(["authenticated"])
async def remove_file_data(
    request: Request,
    filename: str,
    client: Optional[str] = None,
):
    user = request.user.object

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="delete_file",
        client=client,
    )

    await EntryAdapters.adelete_entry_by_file(user, filename)

    return {"status": "ok"}


@api_config.get("/data/{content_source}", response_model=List[str])
@requires(["authenticated"])
async def get_all_filenames(
    request: Request,
    content_source: str,
    client: Optional[str] = None,
):
    user = request.user.object

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="get_all_filenames",
        client=client,
    )

    return await sync_to_async(list)(EntryAdapters.get_all_filenames_by_source(user, content_source))  # type: ignore[call-arg]


@api_config.get("/data/conversation/model/options", response_model=Dict[str, Union[str, int]])
def get_chat_model_options(
    request: Request,
    client: Optional[str] = None,
):
    conversation_options = ConversationAdapters.get_conversation_processor_options().all()

    all_conversation_options = list()
    for conversation_option in conversation_options:
        all_conversation_options.append({"chat_model": conversation_option.chat_model, "id": conversation_option.id})

    return Response(content=json.dumps(all_conversation_options), media_type="application/json", status_code=200)


@api_config.get("/data/conversation/model")
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


@api_config.post("/data/conversation/model", status_code=200)
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


@api_config.post("/data/voice/model", status_code=200)
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


@api_config.post("/data/search/model", status_code=200)
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


@api_config.post("/data/paint/model", status_code=200)
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


@api_config.get("/index/size", response_model=Dict[str, int])
@requires(["authenticated"])
async def get_indexed_data_size(request: Request, common: CommonQueryParams):
    user = request.user.object
    indexed_data_size_in_mb = await sync_to_async(EntryAdapters.get_size_of_indexed_data_in_mb)(user)
    return Response(
        content=json.dumps({"indexed_data_size_in_mb": math.ceil(indexed_data_size_in_mb)}),
        media_type="application/json",
        status_code=200,
    )


@api_config.post("/user/name", status_code=200)
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


@api_config.get("/types", response_model=List[str])
@requires(["authenticated"])
def get_config_types(
    request: Request,
):
    user = request.user.object
    enabled_file_types = EntryAdapters.get_unique_file_types(user)
    configured_content_types = list(enabled_file_types)

    if state.config and state.config.content_type:
        for ctype in state.config.content_type.model_dump(exclude_none=True):
            configured_content_types.append(ctype)

    return [
        search_type.value
        for search_type in SearchType
        if (search_type.value in configured_content_types) or search_type == SearchType.All
    ]
