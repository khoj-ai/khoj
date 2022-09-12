# System Packages
from pathlib import Path
from typing import List, Optional

# External Packages
from pydantic import BaseModel, validator

# Internal Packages
from src.utils.helpers import to_snake_case_from_dash, is_none_or_empty

class ConfigBase(BaseModel):
    class Config:
        alias_generator = to_snake_case_from_dash
        allow_population_by_field_name = True

class TextContentConfig(ConfigBase):
    input_files: Optional[List[Path]]
    input_filter: Optional[List[str]]
    compressed_jsonl: Path
    embeddings_file: Path
    index_heading_entries: Optional[bool] = False

    @validator('input_filter')
    def input_filter_or_files_required(cls, input_filter, values, **kwargs):
        if is_none_or_empty(input_filter) and ('input_files' not in values or values["input_files"] is None):
            raise ValueError("Either input_filter or input_files required in all content-type.<text_search> section of Khoj config file")
        return input_filter

class ImageContentConfig(ConfigBase):
    input_directories: Optional[List[Path]]
    input_filter: Optional[List[str]]
    embeddings_file: Path
    use_xmp_metadata: bool
    batch_size: int

    @validator('input_filter')
    def input_filter_or_directories_required(cls, input_filter, values, **kwargs):
        if is_none_or_empty(input_filter) and ('input_directories' not in values or values["input_directories"] is None):
            raise ValueError("Either input_filter or input_directories required in all content-type.image section of Khoj config file")
        return input_filter

class ContentConfig(ConfigBase):
    org: Optional[TextContentConfig]
    ledger: Optional[TextContentConfig]
    image: Optional[ImageContentConfig]
    music: Optional[TextContentConfig]
    markdown: Optional[TextContentConfig]

class TextSearchConfig(ConfigBase):
    encoder: str
    cross_encoder: str
    model_directory: Optional[Path]

class ImageSearchConfig(ConfigBase):
    encoder: str
    model_directory: Optional[Path]

class SearchConfig(ConfigBase):
    asymmetric: Optional[TextSearchConfig]
    symmetric: Optional[TextSearchConfig]
    image: Optional[ImageSearchConfig]

class ConversationProcessorConfig(ConfigBase):
    openai_api_key: str
    conversation_logfile: Path

class ProcessorConfig(ConfigBase):
    conversation: Optional[ConversationProcessorConfig]

class FullConfig(ConfigBase):
    content_type: Optional[ContentConfig]
    search_type: Optional[SearchConfig]
    processor: Optional[ProcessorConfig]
