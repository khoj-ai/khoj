# Standard Packages
import sys

# External Packages
import torch

# Internal Packages
from src.processor.ledger.beancount_to_jsonl import beancount_to_jsonl
from src.processor.markdown.markdown_to_jsonl import markdown_to_jsonl
from src.processor.org_mode.org_to_jsonl import org_to_jsonl
from src.search_type import image_search, text_search
from src.utils.config import SearchType, SearchModels, ProcessorConfigModel, ConversationProcessorConfigModel
from src.utils.cli import cli
from src.utils import constants
from src.utils.helpers import get_absolute_path
from src.utils.rawconfig import FullConfig


def initialize_server(cmd_args):
    # Load config from CLI
    args = cli(cmd_args)

    # Stores the file path to the config file.
    constants.config_file = args.config_file

    # Store the raw config data.
    constants.config = args.config

    # Store the verbose flag
    constants.verbose = args.verbose

    # Initialize the search model from Config
    constants.model = initialize_search(constants.model, args.config, args.regenerate, device=constants.device, verbose=constants.verbose)

    # Initialize Processor from Config
    constants.processor_config = initialize_processor(args.config, verbose=constants.verbose)

    return args.host, args.port, args.socket


def initialize_search(model: SearchModels, config: FullConfig, regenerate: bool, t: SearchType = None, device=torch.device("cpu"), verbose: int = 0):
    # Initialize Org Notes Search
    if (t == SearchType.Org or t == None) and config.content_type.org:
        # Extract Entries, Generate Notes Embeddings
        model.orgmode_search = text_search.setup(org_to_jsonl, config.content_type.org, search_config=config.search_type.asymmetric, regenerate=regenerate, device=device, verbose=verbose)

    # Initialize Org Music Search
    if (t == SearchType.Music or t == None) and config.content_type.music:
        # Extract Entries, Generate Music Embeddings
        model.music_search = text_search.setup(org_to_jsonl, config.content_type.music, search_config=config.search_type.asymmetric, regenerate=regenerate, device=device, verbose=verbose)

    # Initialize Markdown Search
    if (t == SearchType.Markdown or t == None) and config.content_type.markdown:
        # Extract Entries, Generate Markdown Embeddings
        model.markdown_search = text_search.setup(markdown_to_jsonl, config.content_type.markdown, search_config=config.search_type.asymmetric, regenerate=regenerate, device=device, verbose=verbose)

    # Initialize Ledger Search
    if (t == SearchType.Ledger or t == None) and config.content_type.ledger:
        # Extract Entries, Generate Ledger Embeddings
        model.ledger_search = text_search.setup(beancount_to_jsonl, config.content_type.ledger, search_config=config.search_type.symmetric, regenerate=regenerate, verbose=verbose)

    # Initialize Image Search
    if (t == SearchType.Image or t == None) and config.content_type.image:
        # Extract Entries, Generate Image Embeddings
        model.image_search = image_search.setup(config.content_type.image, search_config=config.search_type.image, regenerate=regenerate, verbose=verbose)

    return model


def initialize_processor(config: FullConfig, verbose: int):
    if not config.processor:
        return

    processor_config = ProcessorConfigModel()

    # Initialize Conversation Processor
    processor_config.conversation = ConversationProcessorConfigModel(config.processor.conversation, verbose)

    conversation_logfile = processor_config.conversation.conversation_logfile
    if processor_config.conversation.verbose:
        print('INFO:\tLoading conversation logs from disk...')

    if conversation_logfile.expanduser().absolute().is_file():
        # Load Metadata Logs from Conversation Logfile
        with open(get_absolute_path(conversation_logfile), 'r') as f:
            processor_config.conversation.meta_log = json.load(f)

        print('INFO:\tConversation logs loaded from disk.')
    else:
        # Initialize Conversation Logs
        processor_config.conversation.meta_log = {}
        processor_config.conversation.chat_session = ""

    return processor_config

