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

class SearchConfigTest(ConfigBaseModel):
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class TextSearchConfigTest(ConfigBaseModel):
    compressed_jsonl: Optional[Path]
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ImageSearchConfigTest(ConfigBaseModel):
    use_xmp_metadata: Optional[str]
    batch_size: Optional[int]
    input_directory: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ContentType(ConfigBaseModel):
    org: Optional[TextSearchConfigTest]
    ledger: Optional[TextSearchConfigTest]
    image: Optional[ImageSearchConfigTest]
    music: Optional[TextSearchConfigTest]

class AsymmetricConfig(ConfigBaseModel):
    encoder: Optional[str]
    cross_encoder: Optional[str]

class ImageSearchTypeConfig(ConfigBaseModel):
    encoder: Optional[str]

class SearchTypeConfigTest(ConfigBaseModel):
    asymmetric: Optional[AsymmetricConfig]
    image: Optional[ImageSearchTypeConfig]

class ProcessorConversationConfig(ConfigBaseModel):
    open_api_key: Optional[str]
    conversation_logfile: Optional[str]
    conversation_history: Optional[str]

class ProcessorConfigTest(ConfigBaseModel):
    conversation: Optional[ProcessorConversationConfig]

class FullConfig(ConfigBaseModel):
    content_type: Optional[ContentType]
    search_type: Optional[SearchTypeConfigTest]
    processor: Optional[ProcessorConfigTest]
