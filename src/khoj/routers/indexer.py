# Standard Packages
import logging
from typing import Optional

# External Packages
from fastapi import APIRouter, HTTPException, Header, Request, Body, Response
from pydantic import BaseModel

# Internal Packages
from khoj.utils import state
from khoj.processor.jsonl.jsonl_to_jsonl import JsonlToJsonl
from khoj.processor.markdown.markdown_to_jsonl import MarkdownToJsonl
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from khoj.processor.pdf.pdf_to_jsonl import PdfToJsonl
from khoj.processor.github.github_to_jsonl import GithubToJsonl
from khoj.processor.notion.notion_to_jsonl import NotionToJsonl
from khoj.processor.plaintext.plaintext_to_jsonl import PlaintextToJsonl
from khoj.utils.rawconfig import ContentConfig
from khoj.search_type import text_search, image_search
from khoj.utils.config import SearchModels
from khoj.utils.helpers import LRU
from khoj.utils.rawconfig import (
    ContentConfig,
)
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.utils.config import (
    ContentIndex,
    SearchModels,
)

logger = logging.getLogger(__name__)

indexer = APIRouter()


class IndexBatchRequest(BaseModel):
    org: Optional[dict[str, str]]
    pdf: Optional[dict[str, str]]
    plaintext: Optional[dict[str, str]]
    markdown: Optional[dict[str, str]]


@indexer.post("/batch")
async def index_batch(
    request: Request,
    x_api_key: str = Header(None),
    regenerate: bool = False,
    search_type: Optional[state.SearchType] = None,
):
    if x_api_key != "secret":
        raise HTTPException(status_code=401, detail="Invalid API Key")
    state.config_lock.acquire()
    try:
        logger.info(f"Received batch indexing request")
        index_batch_request = ""
        async for chunk in request.stream():
            index_batch_request += chunk.decode()
        index_batch_request = IndexBatchRequest.parse_raw(index_batch_request)
        logger.info(f"Received batch indexing request size: {len(index_batch_request.dict())}")

        # Extract required fields from config
        state.content_index = configure_content(
            state.content_index,
            state.config.content_type,
            index_batch_request.dict(),
            state.search_models,
            regenerate=regenerate,
            t=search_type,
        )

    except Exception as e:
        logger.error(f"Failed to process batch indexing request: {e}")
    finally:
        state.config_lock.release()
    return Response(content="OK", status_code=200)


def configure_content(
    content_index: Optional[ContentIndex],
    content_config: Optional[ContentConfig],
    files: Optional[dict[str, str]],
    search_models: SearchModels,
    regenerate: bool,
    t: Optional[state.SearchType] = None,
) -> Optional[ContentIndex]:
    # Run Validation Checks
    if content_config is None:
        logger.warning("🚨 No Content configuration available.")
        return None
    if content_index is None:
        content_index = ContentIndex()

    if files is None:
        logger.warning(f"🚨 No files to process for {t.value} search.")
        return None

    try:
        # Initialize Org Notes Search
        if (
            (t == None or t.value == state.SearchType.Org.value)
            and content_config.org
            and search_models.text_search
            and files["org"]
        ):
            logger.info("🦄 Setting up search for orgmode notes")
            # Extract Entries, Generate Notes Embeddings
            content_index.org = text_search.setup(
                OrgToJsonl,
                files["org"],
                content_config.org,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize Markdown Search
        if (
            (t == None or t.value == state.SearchType.Markdown.value)
            and content_config.markdown
            and search_models.text_search
            and files["markdown"]
        ):
            logger.info("💎 Setting up search for markdown notes")
            # Extract Entries, Generate Markdown Embeddings
            content_index.markdown = text_search.setup(
                MarkdownToJsonl,
                files["markdown"],
                content_config.markdown,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize PDF Search
        if (
            (t == None or t.value == state.SearchType.Pdf.value)
            and content_config.pdf
            and search_models.text_search
            and files["pdf"]
        ):
            logger.info("🖨️ Setting up search for pdf")
            # Extract Entries, Generate PDF Embeddings
            content_index.pdf = text_search.setup(
                PdfToJsonl,
                files["pdf"],
                content_config.pdf,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize Plaintext Search
        if (
            (t == None or t.value == state.SearchType.Plaintext.value)
            and content_config.plaintext
            and search_models.text_search
            and files["plaintext"]
        ):
            logger.info("📄 Setting up search for plaintext")
            # Extract Entries, Generate Plaintext Embeddings
            content_index.plaintext = text_search.setup(
                PlaintextToJsonl,
                files["plaintext"],
                content_config.plaintext,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize Image Search
        if (
            (t == None or t.value == state.SearchType.Image.value)
            and content_config.image
            and search_models.image_search
        ):
            logger.info("🌄 Setting up search for images")
            # Extract Entries, Generate Image Embeddings
            content_index.image = image_search.setup(
                content_config.image, search_models.image_search.image_encoder, regenerate=regenerate
            )

        if (
            (t == None or t.value == state.SearchType.Github.value)
            and content_config.github
            and search_models.text_search
        ):
            logger.info("🐙 Setting up search for github")
            # Extract Entries, Generate Github Embeddings
            content_index.github = text_search.setup(
                GithubToJsonl,
                None,
                content_config.github,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize Notion Search
        if (
            (t == None or t.value in state.SearchType.Notion.value)
            and content_config.notion
            and search_models.text_search
        ):
            logger.info("🔌 Setting up search for notion")
            content_index.notion = text_search.setup(
                NotionToJsonl,
                None,
                content_config.notion,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize External Plugin Search
        if (t == None or t in state.SearchType) and content_config.plugins and search_models.text_search:
            logger.info("🔌 Setting up search for plugins")
            content_index.plugins = {}
            for plugin_type, plugin_config in content_config.plugins.items():
                content_index.plugins[plugin_type] = text_search.setup(
                    JsonlToJsonl,
                    None,
                    plugin_config,
                    search_models.text_search.bi_encoder,
                    regenerate=regenerate,
                    filters=[DateFilter(), WordFilter(), FileFilter()],
                )

    except Exception as e:
        logger.error(f"🚨 Failed to setup search: {e}", exc_info=True)
        raise e

    # Invalidate Query Cache
    state.query_cache = LRU()

    return content_index
