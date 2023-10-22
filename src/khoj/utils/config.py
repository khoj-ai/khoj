# System Packages
from __future__ import annotations  # to avoid quoting type hints

from enum import Enum
import logging

from dataclasses import dataclass
from typing import TYPE_CHECKING, List, Optional, Union, Any

# External Packages
import torch

# Internal Packages
from khoj.processor.conversation.gpt4all.utils import download_model


logger = logging.getLogger(__name__)

# Internal Packages
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
class ContentIndex:
    image: Optional[ImageContent] = None


@dataclass
class SearchModels:
    text_search: Optional[TextSearchModel] = None
    image_search: Optional[ImageSearchModel] = None


@dataclass
class GPT4AllProcessorConfig:
    loaded_model: Union[Any, None] = None


class GPT4AllProcessorModel:
    def __init__(
        self,
        chat_model: str = "llama-2-7b-chat.ggmlv3.q4_0.bin",
    ):
        self.chat_model = chat_model
        self.loaded_model = None
        try:
            self.loaded_model = download_model(self.chat_model)
        except ValueError as e:
            self.loaded_model = None
            logger.error(f"Error while loading offline chat model: {e}", exc_info=True)
            raise e
