# System Packages
from pathlib import Path
from typing import List, Optional

# External Packages
from pydantic import BaseModel

# Internal Packages
from src.utils.helpers import to_snake_case_from_dash

class ConfigBaseModel(BaseModel):
    class Config:
        alias_generator = to_snake_case_from_dash
        allow_population_by_field_name = True

class SearchConfig(ConfigBaseModel):
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class TextSearchConfig(ConfigBaseModel):
    compressed_jsonl: Optional[Path]
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ImageSearchConfig(ConfigBaseModel):
    use_xmp_metadata: Optional[str]
    batch_size: Optional[int]
    input_directory: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ContentType(ConfigBaseModel):
    org: Optional[TextSearchConfig]
    ledger: Optional[TextSearchConfig]
    image: Optional[ImageSearchConfig]
    music: Optional[TextSearchConfig]

class AsymmetricConfig(ConfigBaseModel):
    encoder: Optional[str]
    cross_encoder: Optional[str]

class ImageSearchTypeConfig(ConfigBaseModel):
    encoder: Optional[str]

class SearchTypeConfig(ConfigBaseModel):
    asymmetric: Optional[AsymmetricConfig]
    image: Optional[ImageSearchTypeConfig]

class ProcessorConversationConfig(ConfigBaseModel):
    open_api_key: Optional[str]
    conversation_logfile: Optional[str]
    conversation_history: Optional[str]

class ProcessorConfig(ConfigBaseModel):
    conversation: Optional[ProcessorConversationConfig]

class FullConfig(ConfigBaseModel):
    content_type: Optional[ContentType]
    search_type: Optional[SearchTypeConfig]
    processor: Optional[ProcessorConfig]
