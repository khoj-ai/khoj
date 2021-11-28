# System Packages
from enum import Enum
from dataclasses import dataclass
from pathlib import Path

# Internal Packages
from src.utils.helpers import get_from_dict


class SearchType(str, Enum):
    Notes = "notes"
    Ledger = "ledger"
    Music = "music"
    Image = "image"


class TextSearchModel():
    def __init__(self, entries, corpus_embeddings, bi_encoder, cross_encoder, top_k, verbose):
        self.entries = entries
        self.corpus_embeddings = corpus_embeddings
        self.bi_encoder = bi_encoder
        self.cross_encoder = cross_encoder
        self.top_k = top_k
        self.verbose = verbose


class ImageSearchModel():
    def __init__(self, image_names, image_embeddings, image_metadata_embeddings, image_encoder, verbose):
        self.image_encoder = image_encoder
        self.image_names = image_names
        self.image_embeddings = image_embeddings
        self.image_metadata_embeddings = image_metadata_embeddings
        self.image_encoder = image_encoder
        self.verbose = verbose


@dataclass
class SearchModels():
    notes_search: TextSearchModel = None
    ledger_search: TextSearchModel = None
    music_search: TextSearchModel = None
    image_search: ImageSearchModel = None


class TextSearchConfig():
    def __init__(self, input_files, input_filter, compressed_jsonl, embeddings_file, verbose):
        self.input_files = input_files
        self.input_filter = input_filter
        self.compressed_jsonl = Path(compressed_jsonl)
        self.embeddings_file = Path(embeddings_file)
        self.verbose = verbose


    def create_from_dictionary(config, key_tree, verbose):
        text_config = get_from_dict(config, *key_tree)
        search_enabled = text_config and ('input-files' in text_config or 'input-filter' in text_config)
        if not search_enabled:
            return None

        return TextSearchConfig(
            input_files = text_config['input-files'],
            input_filter = text_config['input-filter'],
            compressed_jsonl = Path(text_config['compressed-jsonl']),
            embeddings_file = Path(text_config['embeddings-file']),
            verbose = verbose)


class ImageSearchConfig():
    def __init__(self, input_directory, embeddings_file, batch_size, use_xmp_metadata, verbose):
        self.input_directory = input_directory
        self.embeddings_file = Path(embeddings_file)
        self.batch_size = batch_size
        self.use_xmp_metadata = use_xmp_metadata
        self.verbose = verbose

    def create_from_dictionary(config, key_tree, verbose):
        image_config = get_from_dict(config, *key_tree)
        search_enabled = image_config and 'input-directory' in image_config
        if not search_enabled:
            return None

        return ImageSearchConfig(
            input_directory = Path(image_config['input-directory']),
            embeddings_file = Path(image_config['embeddings-file']),
            batch_size = image_config['batch-size'],
            use_xmp_metadata = {'yes': True, 'no': False}[image_config['use-xmp-metadata']],
            verbose = verbose)


@dataclass
class SearchConfig():
    notes: TextSearchConfig = None
    ledger: TextSearchConfig = None
    music: TextSearchConfig = None
    image: ImageSearchConfig = None


class ConversationProcessorConfig():
    def __init__(self, conversation_logfile, conversation_history, openai_api_key, verbose):
        self.openai_api_key = openai_api_key
        self.conversation_logfile = conversation_logfile
        self.conversation_history = conversation_history
        self.verbose = verbose

    def create_from_dictionary(config, key_tree, verbose):
        conversation_config = get_from_dict(config, *key_tree)
        if not conversation_config:
            return None

        return ConversationProcessorConfig(
            openai_api_key = conversation_config['openai-api-key'],
            conversation_history = '',
            conversation_logfile = Path(conversation_config['conversation-logfile']),
            verbose = verbose)


@dataclass
class ProcessorConfig():
    conversation: ConversationProcessorConfig = None
