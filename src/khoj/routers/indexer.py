# Standard Packages
import logging
from typing import Optional, Union, Dict
import asyncio

# External Packages
from fastapi import APIRouter, HTTPException, Header, Request, Response, UploadFile
from pydantic import BaseModel
from khoj.routers.helpers import update_telemetry_state

# Internal Packages
from khoj.utils import state, constants
from khoj.processor.markdown.markdown_to_jsonl import MarkdownToJsonl
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from khoj.processor.pdf.pdf_to_jsonl import PdfToJsonl
from khoj.processor.github.github_to_jsonl import GithubToJsonl
from khoj.processor.notion.notion_to_jsonl import NotionToJsonl
from khoj.processor.plaintext.plaintext_to_jsonl import PlaintextToJsonl
from khoj.search_type import text_search, image_search
from khoj.utils.yaml import save_config_to_file_updated_state
from khoj.utils.config import SearchModels
from khoj.utils.helpers import LRU, get_file_type
from khoj.utils.rawconfig import (
    ContentConfig,
    FullConfig,
    SearchConfig,
)
from khoj.utils.config import (
    ContentIndex,
    SearchModels,
)
from database.models import (
    KhojUser,
    GithubConfig,
    NotionConfig,
)

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
async def update(
    request: Request,
    files: list[UploadFile],
    x_api_key: str = Header(None),
    force: bool = False,
    t: Optional[Union[state.SearchType, str]] = None,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    user = request.user.object if request.user.is_authenticated else None
    if x_api_key != "secret":
        raise HTTPException(status_code=401, detail="Invalid API Key")
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
                    file.file.read().decode("utf-8") if encoding == "utf-8" else file.file.read()
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
        state.content_index = await loop.run_in_executor(
            None,
            configure_content,
            state.content_index,
            state.config.content_type,
            indexer_input.dict(),
            state.search_models,
            regenerate=force,
            search_type=t,
            full_corpus=False,
            user=user,
        )
        logger.info(f"Finished processing batch indexing request")
    except Exception as e:
        logger.error(f"Failed to process batch indexing request: {e}", exc_info=True)
        logger.error(
            f"🚨 Failed to {force} update {t} content index triggered via API call by {client} client: {e}",
            exc_info=True,
        )

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="index/update",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

    return Response(content="OK", status_code=200)


def configure_search(search_models: SearchModels, search_config: Optional[SearchConfig]) -> Optional[SearchModels]:
    # Run Validation Checks
    if search_config is None:
        logger.warning("🚨 No Search configuration available.")
        return None
    if search_models is None:
        search_models = SearchModels()

    if search_config.image:
        logger.info("🔍 🌄 Setting up image search model")
        search_models.image_search = image_search.initialize_model(search_config.image)

    return search_models


def configure_content(
    content_index: Optional[ContentIndex],
    content_config: Optional[ContentConfig],
    files: Optional[dict[str, dict[str, str]]],
    search_models: SearchModels,
    regenerate: bool = False,
    t: Optional[Union[state.SearchType, str]] = None,
    full_corpus: bool = True,
    user: KhojUser = None,
) -> Optional[ContentIndex]:
    content_index = ContentIndex()

    if t in [type.value for type in state.SearchType]:
        t = state.SearchType(t).value

    assert type(t) == str or t == None, f"Invalid search type: {t}"

    if files is None:
        logger.warning(f"🚨 No files to process for {t} search.")
        return None

    try:
        # Initialize Org Notes Search
        if (t == None or t == state.SearchType.Org.value) and files["org"]:
            logger.info("🦄 Setting up search for orgmode notes")
            # Extract Entries, Generate Notes Embeddings
            text_search.setup(
                OrgToJsonl,
                files.get("org"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )
    except Exception as e:
        logger.error(f"🚨 Failed to setup org: {e}", exc_info=True)

    try:
        # Initialize Markdown Search
        if (t == None or t == state.SearchType.Markdown.value) and files["markdown"]:
            logger.info("💎 Setting up search for markdown notes")
            # Extract Entries, Generate Markdown Embeddings
            text_search.setup(
                MarkdownToJsonl,
                files.get("markdown"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup markdown: {e}", exc_info=True)

    try:
        # Initialize PDF Search
        if (t == None or t == state.SearchType.Pdf.value) and files["pdf"]:
            logger.info("🖨️ Setting up search for pdf")
            # Extract Entries, Generate PDF Embeddings
            text_search.setup(
                PdfToJsonl,
                files.get("pdf"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup PDF: {e}", exc_info=True)

    try:
        # Initialize Plaintext Search
        if (t == None or t == state.SearchType.Plaintext.value) and files["plaintext"]:
            logger.info("📄 Setting up search for plaintext")
            # Extract Entries, Generate Plaintext Embeddings
            text_search.setup(
                PlaintextToJsonl,
                files.get("plaintext"),
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup plaintext: {e}", exc_info=True)

    try:
        # Initialize Image Search
        if (
            (t == None or t == state.SearchType.Image.value)
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

    try:
        github_config = GithubConfig.objects.filter(user=user).prefetch_related("githubrepoconfig").first()
        if (t == None or t == state.SearchType.Github.value) and github_config is not None:
            logger.info("🐙 Setting up search for github")
            # Extract Entries, Generate Github Embeddings
            text_search.setup(
                GithubToJsonl,
                None,
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
                config=github_config,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup GitHub: {e}", exc_info=True)

    try:
        # Initialize Notion Search
        notion_config = NotionConfig.objects.filter(user=user).first()
        if (t == None or t in state.SearchType.Notion.value) and notion_config:
            logger.info("🔌 Setting up search for notion")
            text_search.setup(
                NotionToJsonl,
                None,
                regenerate=regenerate,
                full_corpus=full_corpus,
                user=user,
                config=notion_config,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup GitHub: {e}", exc_info=True)

    # Invalidate Query Cache
    if user:
        state.query_cache[user.uuid] = LRU()

    return content_index


def load_content(
    content_config: Optional[ContentConfig],
    content_index: Optional[ContentIndex],
    search_models: SearchModels,
):
    logger.info(f"Loading content from existing embeddings...")
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
