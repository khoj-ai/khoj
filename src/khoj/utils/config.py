# System Packages
from __future__ import annotations  # to avoid quoting type hints

import logging
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Any, List, Optional, Union

import torch

from khoj.processor.conversation.offline.utils import download_model

logger = logging.getLogger(__name__)


if TYPE_CHECKING:
    from sentence_transformers import CrossEncoder

    from khoj.utils.models import BaseEncoder


class SearchType(str, Enum):
    All = "all"
    Org = "org"
    Markdown = "markdown"
    Image = "image"
    Pdf = "pdf"
    Github = "github"
    Notion = "notion"
    Plaintext = "plaintext"
    Docx = "docx"


class ProcessorType(str, Enum):
    Conversation = "conversation"


@dataclass
class TextContent:
    enabled: bool


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
class SearchModels:
    text_search: Optional[TextSearchModel] = None


@dataclass
class OfflineChatProcessorConfig:
    loaded_model: Union[Any, None] = None


class OfflineChatProcessorModel:
    def __init__(self, chat_model: str = "NousResearch/Hermes-2-Pro-Mistral-7B-GGUF", max_tokens: int = None):
        self.chat_model = chat_model
        self.loaded_model = None
        try:
            self.loaded_model = download_model(self.chat_model, max_tokens=max_tokens)
        except ValueError as e:
            self.loaded_model = None
            logger.error(f"Error while loading offline chat model: {e}", exc_info=True)
            raise e
