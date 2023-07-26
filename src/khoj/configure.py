# Standard Packages
import sys
import logging
import json
from enum import Enum
from typing import Optional
import requests

# External Packages
import schedule
from fastapi.staticfiles import StaticFiles

# Internal Packages
from khoj.processor.jsonl.jsonl_to_jsonl import JsonlToJsonl
from khoj.processor.markdown.markdown_to_jsonl import MarkdownToJsonl
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from khoj.processor.pdf.pdf_to_jsonl import PdfToJsonl
from khoj.processor.github.github_to_jsonl import GithubToJsonl
from khoj.processor.notion.notion_to_jsonl import NotionToJsonl
from khoj.search_type import image_search, text_search
from khoj.utils import constants, state
from khoj.utils.config import (
    ContentIndex,
    SearchType,
    SearchModels,
    ProcessorConfigModel,
    ConversationProcessorConfigModel,
)
from khoj.utils.helpers import LRU, resolve_absolute_path, merge_dicts
from khoj.utils.rawconfig import FullConfig, ProcessorConfig, SearchConfig, ContentConfig, ConversationProcessorConfig
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.search_filter.file_filter import FileFilter


logger = logging.getLogger(__name__)


def initialize_server(config: Optional[FullConfig], regenerate: bool, required=False):
    if config is None and required:
        logger.error(
            f"🚨 Exiting as Khoj is not configured.\nConfigure it via http://localhost:42110/config or by editing {state.config_file}."
        )
        sys.exit(1)
    elif config is None:
        logger.warning(
            f"🚨 Khoj is not configured.\nConfigure it via http://localhost:42110/config, plugins or by editing {state.config_file}."
        )
        return None

    try:
        configure_server(config, regenerate)
    except Exception as e:
        logger.error(f"🚨 Failed to configure server on app load: {e}", exc_info=True)


def configure_server(config: FullConfig, regenerate: bool, search_type: Optional[SearchType] = None):
    # Update Config
    state.config = config

    # Initialize Processor from Config
    try:
        state.config_lock.acquire()
        state.processor_config = configure_processor(state.config.processor)
    except Exception as e:
        logger.error(f"🚨 Failed to configure processor", exc_info=True)
        raise e
    finally:
        state.config_lock.release()

    # Initialize Search Models from Config
    try:
        state.config_lock.acquire()
        state.SearchType = configure_search_types(state.config)
        state.search_models = configure_search(state.search_models, state.config.search_type)
    except Exception as e:
        logger.error(f"🚨 Failed to configure search models", exc_info=True)
        raise e
    finally:
        state.config_lock.release()

    # Initialize Content from Config
    if state.search_models:
        try:
            state.config_lock.acquire()
            state.content_index = configure_content(
                state.content_index, state.config.content_type, state.search_models, regenerate, search_type
            )
        except Exception as e:
            logger.error(f"🚨 Failed to index content", exc_info=True)
            raise e
        finally:
            state.config_lock.release()


def configure_routes(app):
    # Import APIs here to setup search types before while configuring server
    from khoj.routers.api import api
    from khoj.routers.api_beta import api_beta
    from khoj.routers.web_client import web_client

    app.mount("/static", StaticFiles(directory=constants.web_directory), name="static")
    app.include_router(api, prefix="/api")
    app.include_router(api_beta, prefix="/api/beta")
    app.include_router(web_client)


if not state.demo:

    @schedule.repeat(schedule.every(61).minutes)
    def update_search_index():
        try:
            state.config_lock.acquire()
            state.content_index = configure_content(
                state.content_index, state.config.content_type, state.search_models, regenerate=False
            )
            logger.info("📬 Content index updated via Scheduler")
        except Exception as e:
            logger.error(f"🚨 Error updating content index via Scheduler: {e}", exc_info=True)
        finally:
            state.config_lock.release()


def configure_search_types(config: FullConfig):
    # Extract core search types
    core_search_types = {e.name: e.value for e in SearchType}
    # Extract configured plugin search types
    plugin_search_types = {}
    if config.content_type and config.content_type.plugins:
        plugin_search_types = {plugin_type: plugin_type for plugin_type in config.content_type.plugins.keys()}

    # Dynamically generate search type enum by merging core search types with configured plugin search types
    return Enum("SearchType", merge_dicts(core_search_types, plugin_search_types))


def configure_search(search_models: SearchModels, search_config: Optional[SearchConfig]) -> Optional[SearchModels]:
    # Run Validation Checks
    if search_config is None:
        logger.warning("🚨 No Search configuration available.")
        return None
    if search_models is None:
        search_models = SearchModels()

    # Initialize Search Models
    if search_config.asymmetric:
        logger.info("🔍 📜 Setting up text search model")
        search_models.text_search = text_search.initialize_model(search_config.asymmetric)

    if search_config.image:
        logger.info("🔍 🌄 Setting up image search model")
        search_models.image_search = image_search.initialize_model(search_config.image)

    return search_models


def configure_content(
    content_index: Optional[ContentIndex],
    content_config: Optional[ContentConfig],
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

    try:
        # Initialize Org Notes Search
        if (t == state.SearchType.Org or t == None) and content_config.org and search_models.text_search:
            logger.info("🦄 Setting up search for orgmode notes")
            # Extract Entries, Generate Notes Embeddings
            content_index.org = text_search.setup(
                OrgToJsonl,
                content_config.org,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize Markdown Search
        if (t == state.SearchType.Markdown or t == None) and content_config.markdown and search_models.text_search:
            logger.info("💎 Setting up search for markdown notes")
            # Extract Entries, Generate Markdown Embeddings
            content_index.markdown = text_search.setup(
                MarkdownToJsonl,
                content_config.markdown,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize PDF Search
        if (t == state.SearchType.Pdf or t == None) and content_config.pdf and search_models.text_search:
            logger.info("🖨️ Setting up search for pdf")
            # Extract Entries, Generate PDF Embeddings
            content_index.pdf = text_search.setup(
                PdfToJsonl,
                content_config.pdf,
                search_models.text_search.bi_encoder,
                regenerate=regenerate,
                filters=[DateFilter(), WordFilter(), FileFilter()],
            )

        # Initialize Image Search
        if (t == state.SearchType.Image or t == None) and content_config.image and search_models.image_search:
            logger.info("🌄 Setting up search for images")
            # Extract Entries, Generate Image Embeddings
            content_index.image = image_search.setup(
                content_config.image, search_models.image_search.image_encoder, regenerate=regenerate
            )

        if (t == state.SearchType.Github or t == None) and content_config.github and search_models.text_search:
            logger.info("🐙 Setting up search for github")
            # Extract Entries, Generate Github Embeddings
            content_index.github = text_search.setup(
                GithubToJsonl,
                content_config.github,
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
                    plugin_config,
                    search_models.text_search.bi_encoder,
                    regenerate=regenerate,
                    filters=[DateFilter(), WordFilter(), FileFilter()],
                )

        # Initialize Notion Search
        if (t == None or t in state.SearchType) and content_config.notion and search_models.text_search:
            logger.info("🔌 Setting up search for notion")
            content_index.notion = text_search.setup(
                NotionToJsonl,
                content_config.notion,
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


def configure_processor(
    processor_config: Optional[ProcessorConfig], state_processor_config: Optional[ProcessorConfigModel] = None
):
    if not processor_config:
        logger.warning("🚨 No Processor configuration available.")
        return None

    processor = ProcessorConfigModel()

    # Initialize Conversation Processor
    logger.info("💬 Setting up conversation processor")
    processor.conversation = configure_conversation_processor(processor_config, state_processor_config)

    return processor


def configure_conversation_processor(
    processor_config: Optional[ProcessorConfig], state_processor_config: Optional[ProcessorConfigModel] = None
):
    if (
        not processor_config
        or not processor_config.conversation
        or not processor_config.conversation.conversation_logfile
    ):
        default_config = constants.default_config
        default_conversation_logfile = resolve_absolute_path(
            default_config["processor"]["conversation"]["conversation-logfile"]  # type: ignore
        )
        conversation_logfile = resolve_absolute_path(default_conversation_logfile)
        conversation_config = processor_config.conversation if processor_config else None
        conversation_processor = ConversationProcessorConfigModel(
            conversation_config=ConversationProcessorConfig(
                conversation_logfile=conversation_logfile,
                openai=(conversation_config.openai if (conversation_config is not None) else None),
                enable_offline_chat=(
                    conversation_config.enable_offline_chat if (conversation_config is not None) else False
                ),
            )
        )
    else:
        conversation_processor = ConversationProcessorConfigModel(
            conversation_config=processor_config.conversation,
        )
        conversation_logfile = resolve_absolute_path(conversation_processor.conversation_logfile)

    # Load Conversation Logs from Disk
    if state_processor_config and state_processor_config.conversation and state_processor_config.conversation.meta_log:
        conversation_processor.meta_log = state_processor_config.conversation.meta_log
        conversation_processor.chat_session = state_processor_config.conversation.chat_session
        logger.debug(f"Loaded conversation logs from state")
        return conversation_processor

    if conversation_logfile.is_file():
        # Load Metadata Logs from Conversation Logfile
        with conversation_logfile.open("r") as f:
            conversation_processor.meta_log = json.load(f)
        logger.debug(f"Loaded conversation logs from {conversation_logfile}")
    else:
        # Initialize Conversation Logs
        conversation_processor.meta_log = {}
        conversation_processor.chat_session = []

    return conversation_processor


@schedule.repeat(schedule.every(17).minutes)
def save_chat_session():
    # No need to create empty log file
    if not (
        state.processor_config
        and state.processor_config.conversation
        and state.processor_config.conversation.meta_log
        and state.processor_config.conversation.chat_session
    ):
        return

    # Summarize Conversation Logs for this Session
    conversation_log = state.processor_config.conversation.meta_log
    session = {
        "session-start": conversation_log.get("session", [{"session-end": 0}])[-1]["session-end"],
        "session-end": len(conversation_log["chat"]),
    }
    if "session" in conversation_log:
        conversation_log["session"].append(session)
    else:
        conversation_log["session"] = [session]

    # Save Conversation Metadata Logs to Disk
    conversation_logfile = resolve_absolute_path(state.processor_config.conversation.conversation_logfile)
    conversation_logfile.parent.mkdir(parents=True, exist_ok=True)  # create conversation directory if doesn't exist
    with open(conversation_logfile, "w+", encoding="utf-8") as logfile:
        json.dump(conversation_log, logfile, indent=2)

    state.processor_config.conversation.chat_session = []
    logger.info("📩 Saved current chat session to conversation logs")


@schedule.repeat(schedule.every(59).minutes)
def upload_telemetry():
    if not state.config or not state.config.app or not state.config.app.should_log_telemetry or not state.telemetry:
        message = "📡 No telemetry to upload" if not state.telemetry else "📡 Telemetry logging disabled"
        logger.debug(message)
        return

    try:
        logger.debug(f"📡 Upload usage telemetry to {constants.telemetry_server}:\n{state.telemetry}")
        for log in state.telemetry:
            for field in log:
                # Check if the value for the field is JSON serializable
                try:
                    json.dumps(log[field])
                except TypeError:
                    log[field] = str(log[field])
        requests.post(constants.telemetry_server, json=state.telemetry)
    except Exception as e:
        logger.error(f"📡 Error uploading telemetry: {e}", exc_info=True)
    else:
        state.telemetry = []
