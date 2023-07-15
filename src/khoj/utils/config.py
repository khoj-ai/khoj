# System Packages
from __future__ import annotations  # to avoid quoting type hints
from enum import Enum
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Optional, Union

# External Packages
import torch

# Internal Packages
if TYPE_CHECKING:
    from sentence_transformers import CrossEncoder
    from khoj.search_filter.base_filter import BaseFilter
    from khoj.utils.models import BaseEncoder
    from khoj.utils.rawconfig import ConversationProcessorConfig, Entry


class SearchType(str, Enum):
    All = "all"
    Org = "org"
    Markdown = "markdown"
    Image = "image"
    Pdf = "pdf"
    Github = "github"
    Notion = "notion"


class ProcessorType(str, Enum):
    Conversation = "conversation"


@dataclass
class TextContent:
    entries: List[Entry]
    corpus_embeddings: torch.Tensor
    filters: List[BaseFilter]


@dataclass
class ImageContent:
    image_names: List[str]
    image_embeddings: torch.Tensor
    image_metadata_embeddings: torch.Tensor


@dataclass
class TextSearchModel:
    bi_encoder: BaseEncoder
    cross_encoder: Optional[CrossEncoder] = None
    top_k: Optional[int] = 15


@dataclass
class ImageSearchModel:
    image_encoder: BaseEncoder


@dataclass
class ContentIndex:
    org: Optional[TextContent] = None
    markdown: Optional[TextContent] = None
    pdf: Optional[TextContent] = None
    github: Optional[TextContent] = None
    notion: Optional[TextContent] = None
    image: Optional[ImageContent] = None
    plugins: Optional[Dict[str, TextContent]] = None


@dataclass
class SearchModels:
    text_search: Optional[TextSearchModel] = None
    image_search: Optional[ImageSearchModel] = None
    plugin_search: Optional[Dict[str, TextSearchModel]] = None


class ConversationProcessorConfigModel:
    def __init__(self, processor_config: ConversationProcessorConfig):
        self.openai_api_key = processor_config.openai_api_key
        self.model = processor_config.model
        self.chat_model = processor_config.chat_model
        self.conversation_logfile = Path(processor_config.conversation_logfile)
        self.chat_session: List[str] = []
        self.meta_log: dict = {}


@dataclass
class ProcessorConfigModel:
    conversation: Union[ConversationProcessorConfigModel, None] = None
