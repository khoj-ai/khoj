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

class SearchConfigModel(ConfigBaseModel):
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class TextSearchConfigModel(ConfigBaseModel):
    compressed_jsonl: Optional[Path]
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ImageSearchConfigModel(ConfigBaseModel):
    use_xmp_metadata: Optional[str]
    batch_size: Optional[int]
    input_directory: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ContentTypeModel(ConfigBaseModel):
    org: Optional[TextSearchConfigModel]
    ledger: Optional[TextSearchConfigModel]
    image: Optional[ImageSearchConfigModel]
    music: Optional[TextSearchConfigModel]

class AsymmetricConfigModel(ConfigBaseModel):
    encoder: Optional[str]
    cross_encoder: Optional[str]

class ImageSearchTypeConfigModel(ConfigBaseModel):
    encoder: Optional[str]

class SearchTypeConfigModel(ConfigBaseModel):
    asymmetric: Optional[AsymmetricConfigModel]
    image: Optional[ImageSearchTypeConfigModel]

class ProcessorConversationConfigModel(ConfigBaseModel):
    open_api_key: Optional[str]
    conversation_logfile: Optional[str]
    conversation_history: Optional[str]

class ProcessorConfigModel(ConfigBaseModel):
    conversation: Optional[ProcessorConversationConfigModel]

class FullConfigModel(ConfigBaseModel):
    content_type: Optional[ContentTypeModel]
    search_type: Optional[SearchTypeConfigModel]
    processor: Optional[ProcessorConfigModel]
