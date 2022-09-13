# System Packages
import sys
import logging

# External Packages
import json

# Internal Packages
from src.processor.ledger.beancount_to_jsonl import beancount_to_jsonl
from src.processor.markdown.markdown_to_jsonl import markdown_to_jsonl
from src.processor.org_mode.org_to_jsonl import org_to_jsonl
from src.search_type import image_search, text_search
from src.utils.config import SearchType, SearchModels, ProcessorConfigModel, ConversationProcessorConfigModel
from src.utils import state
from src.utils.helpers import LRU, resolve_absolute_path
from src.utils.rawconfig import FullConfig, ProcessorConfig
from src.search_filter.date_filter import DateFilter
from src.search_filter.word_filter import WordFilter
from src.search_filter.file_filter import FileFilter


logger = logging.getLogger(__name__)


def configure_server(args, required=False):
    if args.config is None:
        if required:
            print('Exiting as Khoj is not configured. Configure the application to use it.')
            sys.exit(1)
        else:
            return
    else:
        state.config = args.config

    # Initialize the search model from Config
    state.model = configure_search(state.model, state.config, args.regenerate)

    # Initialize Processor from Config
    state.processor_config = configure_processor(args.config.processor)


def configure_search(model: SearchModels, config: FullConfig, regenerate: bool, t: SearchType = None):
    # Initialize Org Notes Search
    if (t == SearchType.Org or t == None) and config.content_type.org:
        # Extract Entries, Generate Notes Embeddings
        model.orgmode_search = text_search.setup(
            org_to_jsonl,
            config.content_type.org,
            search_config=config.search_type.asymmetric,
            regenerate=regenerate,
            filters=[DateFilter(), WordFilter(), FileFilter()])

    # Initialize Org Music Search
    if (t == SearchType.Music or t == None) and config.content_type.music:
        # Extract Entries, Generate Music Embeddings
        model.music_search = text_search.setup(
            org_to_jsonl,
            config.content_type.music,
            search_config=config.search_type.asymmetric,
            regenerate=regenerate,
            filters=[DateFilter(), WordFilter()])

    # Initialize Markdown Search
    if (t == SearchType.Markdown or t == None) and config.content_type.markdown:
        # Extract Entries, Generate Markdown Embeddings
        model.markdown_search = text_search.setup(
            markdown_to_jsonl,
            config.content_type.markdown,
            search_config=config.search_type.asymmetric,
            regenerate=regenerate,
            filters=[DateFilter(), WordFilter(), FileFilter()])

    # Initialize Ledger Search
    if (t == SearchType.Ledger or t == None) and config.content_type.ledger:
        # Extract Entries, Generate Ledger Embeddings
        model.ledger_search = text_search.setup(
            beancount_to_jsonl,
            config.content_type.ledger,
            search_config=config.search_type.symmetric,
            regenerate=regenerate,
            filters=[DateFilter(), WordFilter(), FileFilter()])

    # Initialize Image Search
    if (t == SearchType.Image or t == None) and config.content_type.image:
        # Extract Entries, Generate Image Embeddings
        model.image_search = image_search.setup(
            config.content_type.image,
            search_config=config.search_type.image,
            regenerate=regenerate)

    # Invalidate Query Cache
    state.query_cache = LRU()

    return model


def configure_processor(processor_config: ProcessorConfig):
    if not processor_config:
        return

    processor = ProcessorConfigModel()

    # Initialize Conversation Processor
    if processor_config.conversation:
        processor.conversation = configure_conversation_processor(processor_config.conversation)

    return processor


def configure_conversation_processor(conversation_processor_config):
    conversation_processor = ConversationProcessorConfigModel(conversation_processor_config)
    conversation_logfile = resolve_absolute_path(conversation_processor.conversation_logfile)

    if conversation_logfile.is_file():
        # Load Metadata Logs from Conversation Logfile
        with conversation_logfile.open('r') as f:
            conversation_processor.meta_log = json.load(f)
        logger.info('Conversation logs loaded from disk.')
    else:
        # Initialize Conversation Logs
        conversation_processor.meta_log = {}
        conversation_processor.chat_session = ""

    return conversation_processor
