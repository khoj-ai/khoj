# Standard Packages
import logging
import sys
from typing import Optional, Union, Dict

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
from khoj.utils.rawconfig import ContentConfig, TextContentConfig
from khoj.search_type import text_search, image_search
from khoj.utils.config import SearchModels
from khoj.utils.constants import default_config
from khoj.utils.helpers import LRU, get_file_type
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


@indexer.post("/batch")
async def index_batch(
    request: Request,
    x_api_key: str = Header(None),
    regenerate: bool = False,
    search_type: Optional[Union[state.SearchType, str]] = None,
):
    if x_api_key != "secret":
        raise HTTPException(status_code=401, detail="Invalid API Key")
    state.config_lock.acquire()
    try:
        logger.info(f"Received batch indexing request")
        index_batch_request_acc = b""
        async for chunk in request.stream():
            index_batch_request_acc += chunk
        data_bytes = sys.getsizeof(index_batch_request_acc)
        unit = "KB"
        data_size = data_bytes / 1024
        if data_size > 1000:
            unit = "MB"
            data_size = data_size / 1024
        if data_size > 1000:
            unit = "GB"
            data_size = data_size / 1024
        data_size_metric = f"{data_size:.2f} {unit}"
        logger.info(f"Received {data_size_metric} of data")
        index_batch_request = IndexBatchRequest.parse_raw(index_batch_request_acc)
        logger.info(f"Received {len(index_batch_request.files)} files")

        org_files: Dict[str, str] = {}
        markdown_files: Dict[str, str] = {}
        pdf_files: Dict[str, str] = {}
        plaintext_files: Dict[str, str] = {}

        for file in index_batch_request.files:
            file_type = get_file_type(file.path)
            dict_to_update = None
            if file_type == "org":
                dict_to_update = org_files
            elif file_type == "markdown":
                dict_to_update = markdown_files
            elif file_type == "pdf":
                dict_to_update = pdf_files
            elif file_type == "plaintext":
                dict_to_update = plaintext_files

            if dict_to_update is not None:
                dict_to_update[file.path] = file.content
            else:
                logger.info(f"Skipping unsupported streamed file: {file.path}")

        indexer_input = IndexerInput(
            org=org_files,
            markdown=markdown_files,
            pdf=pdf_files,
            plaintext=plaintext_files,
        )

        # Extract required fields from config
        state.content_index = configure_content(
            state.content_index,
            state.config.content_type,
            indexer_input.dict(),
            state.search_models,
            regenerate=regenerate,
            t=search_type,
            full_corpus=False,
        )

    except Exception as e:
        logger.error(f"Failed to process batch indexing request: {e}", exc_info=True)
    finally:
        state.config_lock.release()
    return Response(content="OK", status_code=200)


def configure_content(
    content_index: Optional[ContentIndex],
    content_config: Optional[ContentConfig],
    files: Optional[dict[str, dict[str, str]]],
    search_models: SearchModels,
    regenerate: bool = False,
    t: Optional[Union[state.SearchType, str]] = None,
    full_corpus: bool = True,
) -> Optional[ContentIndex]:
    def has_valid_text_config(config: TextContentConfig):
        return config.input_files or config.input_filter

    # Run Validation Checks
    if content_config is None:
        logger.warning("🚨 No Content configuration available.")
        return None
    if content_index is None:
        content_index = ContentIndex()

    if t in [type.value for type in state.SearchType]:
        t = state.SearchType(t).value

    assert type(t) == str or t == None, f"Invalid search type: {t}"

    if files is None:
        logger.warning(f"🚨 No files to process for {t} search.")
        return None

    try:
        # Initialize Org Notes Search
        if (
            (t == None or t == state.SearchType.Org.value)
            and ((content_config.org and has_valid_text_config(content_config.org)) or files["org"])
            and search_models.text_search
        ):
            if content_config.org == None:
                logger.info("🦄 No configuration for orgmode notes. Using default configuration.")
                default_configuration = default_config["content-type"]["org"]  # type: ignore
                content_config.org = TextContentConfig(
                    compressed_jsonl=default_configuration["compressed-jsonl"],
                    embeddings_file=default_configuration["embeddings-file"],
                )

            logger.info("🦄 Setting up search for orgmode notes")
            # Extract Entries, Generate Notes Embeddings
            content_index.org = text_search.setup(
                OrgToJsonl,
                files.get("org"),
                content_config.org,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
                full_corpus=full_corpus,
            )
    except Exception as e:
        logger.error(f"🚨 Failed to setup org: {e}", exc_info=True)

    try:
        # Initialize Markdown Search
        if (
            (t == None or t == state.SearchType.Markdown.value)
            and ((content_config.markdown and has_valid_text_config(content_config.markdown)) or files["markdown"])
            and search_models.text_search
            and files["markdown"]
        ):
            if content_config.markdown == None:
                logger.info("💎 No configuration for markdown notes. Using default configuration.")
                default_configuration = default_config["content-type"]["markdown"]  # type: ignore
                content_config.markdown = TextContentConfig(
                    compressed_jsonl=default_configuration["compressed-jsonl"],
                    embeddings_file=default_configuration["embeddings-file"],
                )

            logger.info("💎 Setting up search for markdown notes")
            # Extract Entries, Generate Markdown Embeddings
            content_index.markdown = text_search.setup(
                MarkdownToJsonl,
                files.get("markdown"),
                content_config.markdown,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
                full_corpus=full_corpus,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup markdown: {e}", exc_info=True)

    try:
        # Initialize PDF Search
        if (
            (t == None or t == state.SearchType.Pdf.value)
            and ((content_config.pdf and has_valid_text_config(content_config.pdf)) or files["pdf"])
            and search_models.text_search
            and files["pdf"]
        ):
            if content_config.pdf == None:
                logger.info("🖨️ No configuration for pdf notes. Using default configuration.")
                default_configuration = default_config["content-type"]["pdf"]  # type: ignore
                content_config.pdf = TextContentConfig(
                    compressed_jsonl=default_configuration["compressed-jsonl"],
                    embeddings_file=default_configuration["embeddings-file"],
                )

            logger.info("🖨️ Setting up search for pdf")
            # Extract Entries, Generate PDF Embeddings
            content_index.pdf = text_search.setup(
                PdfToJsonl,
                files.get("pdf"),
                content_config.pdf,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
                full_corpus=full_corpus,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup PDF: {e}", exc_info=True)

    try:
        # Initialize Plaintext Search
        if (
            (t == None or t == state.SearchType.Plaintext.value)
            and ((content_config.plaintext and has_valid_text_config(content_config.plaintext)) or files["plaintext"])
            and search_models.text_search
            and files["plaintext"]
        ):
            if content_config.plaintext == None:
                logger.info("📄 No configuration for plaintext notes. Using default configuration.")
                default_configuration = default_config["content-type"]["plaintext"]  # type: ignore
                content_config.plaintext = TextContentConfig(
                    compressed_jsonl=default_configuration["compressed-jsonl"],
                    embeddings_file=default_configuration["embeddings-file"],
                )

            logger.info("📄 Setting up search for plaintext")
            # Extract Entries, Generate Plaintext Embeddings
            content_index.plaintext = text_search.setup(
                PlaintextToJsonl,
                files.get("plaintext"),
                content_config.plaintext,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
                full_corpus=full_corpus,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup plaintext: {e}", exc_info=True)

    try:
        # Initialize Image Search
        if (t == None or t == state.SearchType.Image.value) and content_config.image and search_models.image_search:
            logger.info("🌄 Setting up search for images")
            # Extract Entries, Generate Image Embeddings
            content_index.image = image_search.setup(
                content_config.image, search_models.image_search.image_encoder, regenerate=regenerate
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup images: {e}", exc_info=True)

    try:
        if (t == None or t == state.SearchType.Github.value) and content_config.github and search_models.text_search:
            logger.info("🐙 Setting up search for github")
            # Extract Entries, Generate Github Embeddings
            content_index.github = text_search.setup(
                GithubToJsonl,
                None,
                content_config.github,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
                full_corpus=full_corpus,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup GitHub: {e}", exc_info=True)

    try:
        # Initialize Notion Search
        if (t == None or t in state.SearchType.Notion.value) and content_config.notion and search_models.text_search:
            logger.info("🔌 Setting up search for notion")
            content_index.notion = text_search.setup(
                NotionToJsonl,
                None,
                content_config.notion,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
                full_corpus=full_corpus,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup GitHub: {e}", exc_info=True)

    try:
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
                    full_corpus=full_corpus,
                )

    except Exception as e:
        logger.error(f"🚨 Failed to setup Plugin: {e}", exc_info=True)

    # Invalidate Query Cache
    state.query_cache = LRU()

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

    if content_config.org:
        logger.info("🦄 Loading orgmode notes")
        content_index.org = text_search.load(content_config.org, filters=[DateFilter(), WordFilter(), FileFilter()])
    if content_config.markdown:
        logger.info("💎 Loading markdown notes")
        content_index.markdown = text_search.load(
            content_config.markdown, filters=[DateFilter(), WordFilter(), FileFilter()]
        )
    if content_config.pdf:
        logger.info("🖨️ Loading pdf")
        content_index.pdf = text_search.load(content_config.pdf, filters=[DateFilter(), WordFilter(), FileFilter()])
    if content_config.plaintext:
        logger.info("📄 Loading plaintext")
        content_index.plaintext = text_search.load(
            content_config.plaintext, filters=[DateFilter(), WordFilter(), FileFilter()]
        )
    if content_config.image:
        logger.info("🌄 Loading images")
        content_index.image = image_search.setup(
            content_config.image, search_models.image_search.image_encoder, regenerate=False
        )
    if content_config.github:
        logger.info("🐙 Loading github")
        content_index.github = text_search.load(
            content_config.github, filters=[DateFilter(), WordFilter(), FileFilter()]
        )
    if content_config.notion:
        logger.info("🔌 Loading notion")
        content_index.notion = text_search.load(
            content_config.notion, filters=[DateFilter(), WordFilter(), FileFilter()]
        )
    if content_config.plugins:
        logger.info("🔌 Loading plugins")
        content_index.plugins = {}
        for plugin_type, plugin_config in content_config.plugins.items():
            content_index.plugins[plugin_type] = text_search.load(
                plugin_config, filters=[DateFilter(), WordFilter(), FileFilter()]
            )

    state.query_cache = LRU()
    return content_index
