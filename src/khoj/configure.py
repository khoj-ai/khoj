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
from khoj.utils import constants, state
from khoj.utils.config import (
    SearchType,
    ProcessorConfigModel,
    ConversationProcessorConfigModel,
)
from khoj.utils.helpers import resolve_absolute_path, merge_dicts
from khoj.utils.fs_syncer import collect_files
from khoj.utils.rawconfig import FullConfig, OfflineChatProcessorConfig, ProcessorConfig, ConversationProcessorConfig
from khoj.routers.indexer import configure_content, load_content, configure_search


logger = logging.getLogger(__name__)


def initialize_server(config: Optional[FullConfig], required=False):
    if config is None and required:
        logger.error(
            f"🚨 Exiting as Khoj is not configured.\nConfigure it via http://{state.host}:{state.port}/config or by editing {state.config_file}."
        )
        sys.exit(1)
    elif config is None:
        logger.warning(
            f"🚨 Khoj is not configured.\nConfigure it via http://{state.host}:{state.port}/config, plugins or by editing {state.config_file}."
        )
        return None

    try:
        configure_server(config, init=True)
    except Exception as e:
        logger.error(f"🚨 Failed to configure server on app load: {e}", exc_info=True)


def configure_server(
    config: FullConfig, regenerate: bool = False, search_type: Optional[SearchType] = None, init=False
):
    # Update Config
    state.config = config

    # Initialize Processor from Config
    try:
        state.processor_config = configure_processor(state.config.processor)
    except Exception as e:
        logger.error(f"🚨 Failed to configure processor", exc_info=True)
        raise e

    # Initialize Search Models from Config and initialize content
    try:
        state.config_lock.acquire()
        state.SearchType = configure_search_types(state.config)
        state.search_models = configure_search(state.search_models, state.config.search_type)
        initialize_content(regenerate, search_type, init)
    except Exception as e:
        logger.error(f"🚨 Failed to configure search models", exc_info=True)
        raise e
    finally:
        state.config_lock.release()


def initialize_content(regenerate: bool, search_type: Optional[SearchType] = None, init=False):
    # Initialize Content from Config
    if state.search_models:
        try:
            if init:
                logger.info("📬 Initializing content index...")
                state.content_index = load_content(state.config.content_type, state.content_index, state.search_models)
            else:
                logger.info("📬 Updating content index...")
                all_files = collect_files(state.config.content_type)
                state.content_index = configure_content(
                    state.content_index,
                    state.config.content_type,
                    all_files,
                    state.search_models,
                    regenerate,
                    search_type,
                )
        except Exception as e:
            logger.error(f"🚨 Failed to index content", exc_info=True)
            raise e


def configure_routes(app):
    # Import APIs here to setup search types before while configuring server
    from khoj.routers.api import api
    from khoj.routers.api_beta import api_beta
    from khoj.routers.web_client import web_client
    from khoj.routers.indexer import indexer

    app.mount("/static", StaticFiles(directory=constants.web_directory), name="static")
    app.include_router(api, prefix="/api")
    app.include_router(api_beta, prefix="/api/beta")
    app.include_router(indexer, prefix="/api/v1/index")
    app.include_router(web_client)


if not state.demo:

    @schedule.repeat(schedule.every(61).minutes)
    def update_search_index():
        try:
            logger.info("📬 Updating content index via Scheduler")
            all_files = collect_files(state.config.content_type)
            state.content_index = configure_content(
                state.content_index, state.config.content_type, all_files, state.search_models
            )
            logger.info("📪 Content index updated via Scheduler")
        except Exception as e:
            logger.error(f"🚨 Error updating content index via Scheduler: {e}", exc_info=True)


def configure_search_types(config: FullConfig):
    # Extract core search types
    core_search_types = {e.name: e.value for e in SearchType}
    # Extract configured plugin search types
    plugin_search_types = {}
    if config.content_type and config.content_type.plugins:
        plugin_search_types = {plugin_type: plugin_type for plugin_type in config.content_type.plugins.keys()}

    # Dynamically generate search type enum by merging core search types with configured plugin search types
    return Enum("SearchType", merge_dicts(core_search_types, plugin_search_types))


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
                offline_chat=conversation_config.offline_chat if conversation_config else OfflineChatProcessorConfig(),
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
