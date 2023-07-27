# System Packages
from __future__ import annotations  # to avoid quoting type hints

from enum import Enum
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Optional, Union, Any
from khoj.processor.conversation.gpt4all.utils import download_model

# External Packages
import torch

# Internal Packages
if TYPE_CHECKING:
    from sentence_transformers import CrossEncoder
    from khoj.search_filter.base_filter import BaseFilter
    from khoj.utils.models import BaseEncoder
    from khoj.utils.rawconfig import ConversationProcessorConfig, Entry, OpenAIProcessorConfig


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


@dataclass
class GPT4AllProcessorConfig:
    chat_model: Optional[str] = "llama-2-7b-chat.ggmlv3.q4_K_S.bin"
    loaded_model: Union[Any, None] = None


class ConversationProcessorConfigModel:
    def __init__(
        self,
        conversation_config: ConversationProcessorConfig,
    ):
        self.openai_model = conversation_config.openai
        self.gpt4all_model = GPT4AllProcessorConfig()
        self.enable_offline_chat = conversation_config.enable_offline_chat
        self.conversation_logfile = Path(conversation_config.conversation_logfile)
        self.chat_session: List[str] = []
        self.meta_log: dict = {}

        if not self.openai_model and self.enable_offline_chat:
            self.gpt4all_model.loaded_model = download_model(self.gpt4all_model.chat_model)
        else:
            self.gpt4all_model.loaded_model = None


@dataclass
class ProcessorConfigModel:
    conversation: Union[ConversationProcessorConfigModel, None] = None
