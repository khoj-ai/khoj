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

class TextContentConfig(ConfigBase):
    compressed_jsonl: Optional[Path]
    input_files: Optional[List[str]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ImageContentConfig(ConfigBase):
    use_xmp_metadata: Optional[bool]
    batch_size: Optional[int]
    input_directories: Optional[List[Path]]
    input_filter: Optional[str]
    embeddings_file: Optional[Path]

class ContentConfig(ConfigBase):
    org: Optional[TextContentConfig]
    ledger: Optional[TextContentConfig]
    image: Optional[ImageContentConfig]
    music: Optional[TextContentConfig]
    markdown: Optional[TextContentConfig]

class TextSearchConfig(ConfigBase):
    encoder: Optional[str]
    cross_encoder: Optional[str]
    model_directory: Optional[Path]

class ImageSearchConfig(ConfigBase):
    encoder: Optional[str]
    model_directory: Optional[Path]

class SearchConfig(ConfigBase):
    asymmetric: Optional[TextSearchConfig]
    symmetric: Optional[TextSearchConfig]
    image: Optional[ImageSearchConfig]

class ConversationProcessorConfig(ConfigBase):
    openai_api_key: Optional[str]
    conversation_logfile: Optional[str]

class ProcessorConfig(ConfigBase):
    conversation: Optional[ConversationProcessorConfig]

class FullConfig(ConfigBase):
    content_type: Optional[ContentConfig]
    search_type: Optional[SearchConfig]
    processor: Optional[ProcessorConfig]
