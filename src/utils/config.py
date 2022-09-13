# System Packages
from enum import Enum
from dataclasses import dataclass
from pathlib import Path

# Internal Packages
from src.utils.rawconfig import ConversationProcessorConfig
from src.search_filter.base_filter import BaseFilter


class SearchType(str, Enum):
    Org = "org"
    Ledger = "ledger"
    Music = "music"
    Markdown = "markdown"
    Image = "image"


class ProcessorType(str, Enum):
    Conversation = "conversation"


class TextSearchModel():
    def __init__(self, entries, corpus_embeddings, bi_encoder, cross_encoder, filters: list[BaseFilter], top_k):
        self.entries = entries
        self.corpus_embeddings = corpus_embeddings
        self.bi_encoder = bi_encoder
        self.cross_encoder = cross_encoder
        self.filters = filters
        self.top_k = top_k


class ImageSearchModel():
    def __init__(self, image_names, image_embeddings, image_metadata_embeddings, image_encoder):
        self.image_encoder = image_encoder
        self.image_names = image_names
        self.image_embeddings = image_embeddings
        self.image_metadata_embeddings = image_metadata_embeddings
        self.image_encoder = image_encoder


@dataclass
class SearchModels():
    orgmode_search: TextSearchModel = None
    ledger_search: TextSearchModel = None
    music_search: TextSearchModel = None
    markdown_search: TextSearchModel = None
    image_search: ImageSearchModel = None


class ConversationProcessorConfigModel():
    def __init__(self, processor_config: ConversationProcessorConfig):
        self.openai_api_key = processor_config.openai_api_key
        self.conversation_logfile = Path(processor_config.conversation_logfile)
        self.chat_session = ''
        self.meta_log: dict = {}


@dataclass
class ProcessorConfigModel():
    conversation: ConversationProcessorConfigModel = None
