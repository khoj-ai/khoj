# System Packages
from __future__ import annotations  # to avoid quoting type hints

import logging
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Any, List, Optional, Union

import torch

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
