# System Packages
from __future__ import annotations  # to avoid quoting type hints

from enum import Enum


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
