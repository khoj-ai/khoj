# System Packages
from enum import Enum
from dataclasses import dataclass
from pathlib import Path

# Internal Packages
from src.utils.helpers import get_from_dict

from src.utils.rawconfig import TextSearchConfigTest, ImageSearchConfigTest, ProcessorConversationConfig


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
    def __init__(self, text_search_config: TextSearchConfigTest, verbose: bool):
        self.input_files = text_search_config.input_files
        self.input_filter = text_search_config.input_filter
        self.compressed_jsonl = Path(text_search_config.compressed_jsonl)
        self.embeddings_file = Path(text_search_config.embeddings_file)
        self.verbose = verbose


class ImageSearchConfig():
    def __init__(self, image_search_config: ImageSearchConfigTest, verbose):
        self.input_directory = Path(image_search_config.input_directory)
        self.embeddings_file = Path(image_search_config.embeddings_file)
        self.batch_size = image_search_config.batch_size
        self.use_xmp_metadata = image_search_config.use_xmp_metadata
        self.verbose = verbose


@dataclass
class SearchConfig():
    notes: TextSearchConfig = None
    ledger: TextSearchConfig = None
    music: TextSearchConfig = None
    image: ImageSearchConfig = None


class ConversationProcessorConfig():
    def __init__(self, processor_config: ProcessorConversationConfig, verbose: bool):
        self.openai_api_key = processor_config.open_api_key
        self.conversation_logfile = Path(processor_config.conversation_logfile)
        self.chat_log = ''
        self.meta_log = []
        self.conversation_history = Path(processor_config.conversation_history)
        self.verbose = verbose

@dataclass
class ProcessorConfig():
    conversation: ConversationProcessorConfig = None
