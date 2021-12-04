# System Packages
from enum import Enum
from dataclasses import dataclass
from pathlib import Path

# Internal Packages
from src.utils.rawconfig import ProcessorConversationConfigModel


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


class ConversationProcessorConfigDTO():
    def __init__(self, processor_config: ProcessorConversationConfigModel, verbose: bool):
        self.openai_api_key = processor_config.open_api_key
        self.conversation_logfile = Path(processor_config.conversation_logfile)
        self.chat_log = ''
        self.meta_log = []
        self.conversation_history = Path(processor_config.conversation_history)
        self.verbose = verbose

@dataclass
class ProcessorConfig():
    conversation: ConversationProcessorConfigDTO = None
