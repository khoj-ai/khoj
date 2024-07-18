import asyncio
import logging
from typing import Dict, Optional, Union

from fastapi import APIRouter, Depends, Header, Request, Response, UploadFile
from pydantic import BaseModel
from starlette.authentication import requires

from khoj.routers.helpers import (
    ApiIndexedDataLimiter,
    configure_content,
    update_telemetry_state,
)
from khoj.utils import constants, state
from khoj.utils.config import SearchModels
from khoj.utils.helpers import get_file_type
from khoj.utils.rawconfig import ContentConfig, FullConfig, SearchConfig
from khoj.utils.yaml import save_config_to_file_updated_state

logger = logging.getLogger(__name__)

api_content = APIRouter()


class File(BaseModel):
    path: str
    content: Union[str, bytes]


class IndexBatchRequest(BaseModel):
    files: list[File]


class IndexerInput(BaseModel):
    org: Optional[dict[str, str]] = None
    markdown: Optional[dict[str, str]] = None
    pdf: Optional[dict[str, bytes]] = None
    plaintext: Optional[dict[str, str]] = None
    image: Optional[dict[str, bytes]] = None
    docx: Optional[dict[str, bytes]] = None


@api_content.put("")
@requires(["authenticated"])
async def put_content(
    request: Request,
    files: list[UploadFile],
    t: Optional[Union[state.SearchType, str]] = state.SearchType.All,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
    indexed_data_limiter: ApiIndexedDataLimiter = Depends(
        ApiIndexedDataLimiter(
            incoming_entries_size_limit=10,
            subscribed_incoming_entries_size_limit=25,
            total_entries_size_limit=10,
            subscribed_total_entries_size_limit=100,
        )
    ),
):
    return await indexer(request, files, t, True, client, user_agent, referer, host)


@api_content.patch("")
@requires(["authenticated"])
async def patch_content(
    request: Request,
    files: list[UploadFile],
    t: Optional[Union[state.SearchType, str]] = state.SearchType.All,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
    indexed_data_limiter: ApiIndexedDataLimiter = Depends(
        ApiIndexedDataLimiter(
            incoming_entries_size_limit=10,
            subscribed_incoming_entries_size_limit=25,
            total_entries_size_limit=10,
            subscribed_total_entries_size_limit=100,
        )
    ),
):
    return await indexer(request, files, t, False, client, user_agent, referer, host)


async def indexer(
    request: Request,
    files: list[UploadFile],
    t: Optional[Union[state.SearchType, str]] = state.SearchType.All,
    regenerate: bool = False,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    user = request.user.object
    method = "regenerate" if regenerate else "sync"
    index_files: Dict[str, Dict[str, str]] = {
        "org": {},
        "markdown": {},
        "pdf": {},
        "plaintext": {},
        "image": {},
        "docx": {},
    }
    try:
        logger.info(f"📬 Updating content index via API call by {client} client")
        for file in files:
            file_content = file.file.read()
            file_type, encoding = get_file_type(file.content_type, file_content)
            if file_type in index_files:
                index_files[file_type][file.filename] = file_content.decode(encoding) if encoding else file_content
            else:
                logger.warning(f"Skipped indexing unsupported file type sent by {client} client: {file.filename}")

        indexer_input = IndexerInput(
            org=index_files["org"],
            markdown=index_files["markdown"],
            pdf=index_files["pdf"],
            plaintext=index_files["plaintext"],
            image=index_files["image"],
            docx=index_files["docx"],
        )

        if state.config == None:
            logger.info("📬 Initializing content index on first run.")
            default_full_config = FullConfig(
                content_type=None,
                search_type=SearchConfig.model_validate(constants.default_config["search-type"]),
                processor=None,
            )
            state.config = default_full_config
            default_content_config = ContentConfig(
                org=None,
                markdown=None,
                pdf=None,
                docx=None,
                image=None,
                github=None,
                notion=None,
                plaintext=None,
            )
            state.config.content_type = default_content_config
            save_config_to_file_updated_state()
            configure_search(state.search_models, state.config.search_type)

        # Extract required fields from config
        loop = asyncio.get_event_loop()
        success = await loop.run_in_executor(
            None,
            configure_content,
            indexer_input.model_dump(),
            regenerate,
            t,
            user,
        )
        if not success:
            raise RuntimeError(f"Failed to {method} {t} data sent by {client} client into content index")
        logger.info(f"Finished {method} {t} data sent by {client} client into content index")
    except Exception as e:
        logger.error(f"Failed to {method} {t} data sent by {client} client into content index: {e}", exc_info=True)
        logger.error(
            f"🚨 Failed to {method} {t} data sent by {client} client into content index: {e}",
            exc_info=True,
        )
        return Response(content="Failed", status_code=500)

    indexing_metadata = {
        "num_org": len(index_files["org"]),
        "num_markdown": len(index_files["markdown"]),
        "num_pdf": len(index_files["pdf"]),
        "num_plaintext": len(index_files["plaintext"]),
        "num_image": len(index_files["image"]),
        "num_docx": len(index_files["docx"]),
    }

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="index/update",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
        metadata=indexing_metadata,
    )

    logger.info(f"📪 Content index updated via API call by {client} client")

    indexed_filenames = ",".join(file for ctype in index_files for file in index_files[ctype]) or ""
    return Response(content=indexed_filenames, status_code=200)


def configure_search(search_models: SearchModels, search_config: Optional[SearchConfig]) -> Optional[SearchModels]:
    # Run Validation Checks
    if search_models is None:
        search_models = SearchModels()

    return search_models
