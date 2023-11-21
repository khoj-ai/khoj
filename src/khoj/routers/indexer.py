import asyncio
import logging
from typing import Dict, Optional, Union

from fastapi import APIRouter, Header, Request, Response, UploadFile
from pydantic import BaseModel
from starlette.authentication import requires

from khoj.database.models import GithubConfig, KhojUser, NotionConfig
from khoj.processor.github.github_to_entries import GithubToEntries
from khoj.processor.markdown.markdown_to_entries import MarkdownToEntries
from khoj.processor.notion.notion_to_entries import NotionToEntries
from khoj.processor.org_mode.org_to_entries import OrgToEntries
from khoj.processor.pdf.pdf_to_entries import PdfToEntries
from khoj.processor.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.routers.helpers import update_telemetry_state
from khoj.search_type import image_search, text_search
from khoj.utils import constants, state
from khoj.utils.config import ContentIndex, SearchModels
from khoj.utils.helpers import LRU, get_file_type
from khoj.utils.rawconfig import ContentConfig, FullConfig, SearchConfig
from khoj.utils.yaml import save_config_to_file_updated_state

logger = logging.getLogger(__name__)

indexer = APIRouter()


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


@indexer.post("/update")
@requires(["authenticated"])
async def update(
    request: Request,
    files: list[UploadFile],
    force: bool = False,
    t: Optional[Union[state.SearchType, str]] = state.SearchType.All,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    user = request.user.object
    try:
        logger.info(f"📬 Updating content index via API call by {client} client")
        org_files: Dict[str, str] = {}
        markdown_files: Dict[str, str] = {}
        pdf_files: Dict[str, bytes] = {}
        plaintext_files: Dict[str, str] = {}

        for file in files:
            file_type, encoding = get_file_type(file.content_type)
            dict_to_update = None
            if file_type == "org":
                dict_to_update = org_files
            elif file_type == "markdown":
                dict_to_update = markdown_files
            elif file_type == "pdf":
                dict_to_update = pdf_files  # type: ignore
            elif file_type == "plaintext":
                dict_to_update = plaintext_files

            if dict_to_update is not None:
                dict_to_update[file.filename] = (
                    file.file.read().decode("utf-8") if encoding == "utf-8" else file.file.read()  # type: ignore
                )
            else:
                logger.warning(f"Skipped indexing unsupported file type sent by {client} client: {file.filename}")

        indexer_input = IndexerInput(
            org=org_files,
            markdown=markdown_files,
            pdf=pdf_files,
            plaintext=plaintext_files,
        )

        if state.config == None:
            logger.info("📬 Initializing content index on first run.")
            default_full_config = FullConfig(
                content_type=None,
                search_type=SearchConfig.parse_obj(constants.default_config["search-type"]),
                processor=None,
            )
            state.config = default_full_config
            default_content_config = ContentConfig(
                org=None,
                markdown=None,
                pdf=None,
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
        state.content_index, success = await loop.run_in_executor(
            None,
            configure_content,
            state.content_index,
            state.config.content_type,
            indexer_input.dict(),
            state.search_models,
            force,
            t,
            False,
            user,
        )
        if not success:
            raise RuntimeError("Failed to update content index")
        logger.info(f"Finished processing batch indexing request")
    except Exception as e:
        logger.error(f"Failed to process batch indexing request: {e}", exc_info=True)
        logger.error(
            f"🚨 Failed to {force} update {t} content index triggered via API call by {client} client: {e}",
            exc_info=True,
        )
        return Response(content="Failed", status_code=500)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="index/update",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

    logger.info(f"📪 Content index updated via API call by {client} client")

    return Response(content="OK", status_code=200)


def configure_search(search_models: SearchModels, search_config: Optional[SearchConfig]) -> Optional[SearchModels]:
    # Run Validation Checks
    if search_models is None:
        search_models = SearchModels()

    if search_config and search_config.image:
        logger.info("🔍 🌄 Setting up image search model")
        search_models.image_search = image_search.initialize_model(search_config.image)

    return search_models


def configure_content(
    content_index: Optional[ContentIndex],
    content_config: Optional[ContentConfig],
    files: Optional[dict[str, dict[str, str]]],
    search_models: SearchModels,
    regenerate: bool = False,
    t: Optional[state.SearchType] = state.SearchType.All,
    full_corpus: bool = True,
    user: KhojUser = None,
) -> tuple[Optional[ContentIndex], bool]:
    content_index = ContentIndex()

    success = True
    if t == None:
        t = state.SearchType.All

    if t is not None and t in [type.value for type in state.SearchType]:
        t = state.SearchType(t)

    if t is not None and not t.value in [type.value for type in state.SearchType]:
        logger.warning(f"🚨 Invalid search type: {t}")
        return None, False

    search_type = t.value if t else None

    if files is None:
        logger.warning(f"🚨 No files to process for {search_type} search.")
        return None, True

    try:
        # Initialize Org Notes Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Org.value) and files["org"]:
            logger.info("🦄 Setting up search for orgmode notes")
            # Extract Entries, Generate Notes Embeddings
            text_search.setup(
                OrgToEntries,
                files.get("org"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )
    except Exception as e:
        logger.error(f"🚨 Failed to setup org: {e}", exc_info=True)
        success = False

    try:
        # Initialize Markdown Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Markdown.value) and files[
            "markdown"
        ]:
            logger.info("💎 Setting up search for markdown notes")
            # Extract Entries, Generate Markdown Embeddings
            text_search.setup(
                MarkdownToEntries,
                files.get("markdown"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup markdown: {e}", exc_info=True)
        success = False

    try:
        # Initialize PDF Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Pdf.value) and files["pdf"]:
            logger.info("🖨️ Setting up search for pdf")
            # Extract Entries, Generate PDF Embeddings
            text_search.setup(
                PdfToEntries,
                files.get("pdf"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup PDF: {e}", exc_info=True)
        success = False

    try:
        # Initialize Plaintext Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Plaintext.value) and files[
            "plaintext"
        ]:
            logger.info("📄 Setting up search for plaintext")
            # Extract Entries, Generate Plaintext Embeddings
            text_search.setup(
                PlaintextToEntries,
                files.get("plaintext"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup plaintext: {e}", exc_info=True)
        success = False

    try:
        # Initialize Image Search
        if (
            (search_type == state.SearchType.All.value or search_type == state.SearchType.Image.value)
            and content_config
            and content_config.image
            and search_models.image_search
        ):
            logger.info("🌄 Setting up search for images")
            # Extract Entries, Generate Image Embeddings
            content_index.image = image_search.setup(
                content_config.image, search_models.image_search.image_encoder, regenerate=regenerate
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup images: {e}", exc_info=True)
        success = False

    try:
        github_config = GithubConfig.objects.filter(user=user).prefetch_related("githubrepoconfig").first()
        if (
            search_type == state.SearchType.All.value or search_type == state.SearchType.Github.value
        ) and github_config is not None:
            logger.info("🐙 Setting up search for github")
            # Extract Entries, Generate Github Embeddings
            text_search.setup(
                GithubToEntries,
                None,
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
                config=github_config,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup GitHub: {e}", exc_info=True)
        success = False

    try:
        # Initialize Notion Search
        notion_config = NotionConfig.objects.filter(user=user).first()
        if (
            search_type == state.SearchType.All.value or search_type == state.SearchType.Notion.value
        ) and notion_config:
            logger.info("🔌 Setting up search for notion")
            text_search.setup(
                NotionToEntries,
                None,
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
                config=notion_config,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup Notion: {e}", exc_info=True)
        success = False

    # Invalidate Query Cache
    if user:
        state.query_cache[user.uuid] = LRU()

    return content_index, success


def load_content(
    content_config: Optional[ContentConfig],
    content_index: Optional[ContentIndex],
    search_models: SearchModels,
):
    if content_config is None:
        logger.warning("🚨 No Content configuration available.")
        return None
    if content_index is None:
        content_index = ContentIndex()

    if content_config.image:
        logger.info("🌄 Loading images")
        content_index.image = image_search.setup(
            content_config.image, search_models.image_search.image_encoder, regenerate=False
        )
    return content_index
