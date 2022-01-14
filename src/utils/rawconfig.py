# System Packages
from pathlib import Path
from typing import List, Optional

# External Packages
from pydantic import BaseModel

# Internal Packages
from src.utils.helpers import to_snake_case_from_dash

class ConfigBase(BaseModel):
    class Config:
        alias_generator = to_snake_case_from_dash
        allow_population_by_field_name = True

class SearchConfig(ConfigBase):
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class TextSearchConfig(ConfigBase):
    compressed_jsonl: Optional[Path]
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ImageSearchConfig(ConfigBase):
    use_xmp_metadata: Optional[str]
    batch_size: Optional[int]
    input_directory: Optional[Path]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ContentTypeConfig(ConfigBase):
    org: Optional[TextSearchConfig]
    ledger: Optional[TextSearchConfig]
    image: Optional[ImageSearchConfig]
    music: Optional[TextSearchConfig]

class SymmetricConfig(ConfigBase):
    encoder: Optional[str]
    cross_encoder: Optional[str]
    model_directory: Optional[Path]

class AsymmetricConfig(ConfigBase):
    encoder: Optional[str]
    cross_encoder: Optional[str]
    model_directory: Optional[Path]

class ImageSearchTypeConfig(ConfigBase):
    encoder: Optional[str]

class SearchTypeConfig(ConfigBase):
    asymmetric: Optional[AsymmetricConfig]
    symmetric: Optional[SymmetricConfig]
    image: Optional[ImageSearchTypeConfig]

class ConversationProcessorConfig(ConfigBase):
    openai_api_key: Optional[str]
    conversation_logfile: Optional[str]

class ProcessorConfigModel(ConfigBase):
    conversation: Optional[ConversationProcessorConfig]

class FullConfig(ConfigBase):
    content_type: Optional[ContentTypeConfig]
    search_type: Optional[SearchTypeConfig]
    processor: Optional[ProcessorConfigModel]
