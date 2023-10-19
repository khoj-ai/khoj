# System Packages
from __future__ import annotations  # to avoid quoting type hints

from enum import Enum
import logging

from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Dict, List, Optional, Union, Any
from khoj.processor.conversation.gpt4all.utils import download_model

# External Packages
import torch

from khoj.utils.rawconfig import OfflineChatProcessorConfig

logger = logging.getLogger(__name__)

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
    Plaintext = "plaintext"


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
    plaintext: Optional[TextContent] = None
    plugins: Optional[Dict[str, TextContent]] = None


@dataclass
class SearchModels:
    text_search: Optional[TextSearchModel] = None
    image_search: Optional[ImageSearchModel] = None
    plugin_search: Optional[Dict[str, TextSearchModel]] = None


@dataclass
class GPT4AllProcessorConfig:
    loaded_model: Union[Any, None] = None


class ConversationProcessorConfigModel:
    def __init__(
        self,
        conversation_config: ConversationProcessorConfig,
    ):
        self.openai_model = conversation_config.openai
        self.gpt4all_model = GPT4AllProcessorConfig()
        self.offline_chat = conversation_config.offline_chat or OfflineChatProcessorConfig()
        self.max_prompt_size = conversation_config.max_prompt_size
        self.tokenizer = conversation_config.tokenizer
        self.conversation_logfile = Path(conversation_config.conversation_logfile)
        self.chat_session: List[str] = []
        self.meta_log: dict = {}

        if self.offline_chat.enable_offline_chat:
            try:
                self.gpt4all_model.loaded_model = download_model(self.offline_chat.chat_model)
            except Exception as e:
                self.offline_chat.enable_offline_chat = False
                self.gpt4all_model.loaded_model = None
                logger.error(f"Error while loading offline chat model: {e}", exc_info=True)
        else:
            self.gpt4all_model.loaded_model = None


@dataclass
class ProcessorConfigModel:
    conversation: Union[ConversationProcessorConfigModel, None] = None
