# System Packages
import json
from pathlib import Path
from typing import List, Dict, Optional, Union, Any

# External Packages
from pydantic import BaseModel, validator

# Internal Packages
from khoj.utils.helpers import to_snake_case_from_dash, is_none_or_empty


class ConfigBase(BaseModel):
    class Config:
        alias_generator = to_snake_case_from_dash
        allow_population_by_field_name = True

    def __getitem__(self, item):
        return getattr(self, item)

    def __setitem__(self, key, value):
        return setattr(self, key, value)


class TextConfigBase(ConfigBase):
    compressed_jsonl: Path
    embeddings_file: Path


class TextContentConfig(TextConfigBase):
    input_files: Optional[List[Path]]
    input_filter: Optional[List[str]]
    index_heading_entries: Optional[bool] = False

    @validator("input_filter")
    def input_filter_or_files_required(cls, input_filter, values, **kwargs):
        if is_none_or_empty(input_filter) and ("input_files" not in values or values["input_files"] is None):
            raise ValueError(
                "Either input_filter or input_files required in all content-type.<text_search> section of Khoj config file"
            )
        return input_filter


class GithubRepoConfig(ConfigBase):
    name: str
    owner: str
    branch: Optional[str] = "master"


class GithubContentConfig(TextConfigBase):
    pat_token: str
    repos: List[GithubRepoConfig]


class NotionContentConfig(TextConfigBase):
    token: str


class ImageContentConfig(ConfigBase):
    input_directories: Optional[List[Path]]
    input_filter: Optional[List[str]]
    embeddings_file: Path
    use_xmp_metadata: bool
    batch_size: int

    @validator("input_filter")
    def input_filter_or_directories_required(cls, input_filter, values, **kwargs):
        if is_none_or_empty(input_filter) and (
            "input_directories" not in values or values["input_directories"] is None
        ):
            raise ValueError(
                "Either input_filter or input_directories required in all content-type.image section of Khoj config file"
            )
        return input_filter


class ContentConfig(ConfigBase):
    org: Optional[TextContentConfig]
    image: Optional[ImageContentConfig]
    markdown: Optional[TextContentConfig]
    pdf: Optional[TextContentConfig]
    github: Optional[GithubContentConfig]
    plugins: Optional[Dict[str, TextContentConfig]]
    notion: Optional[NotionContentConfig]


class TextSearchConfig(ConfigBase):
    encoder: str
    cross_encoder: str
    encoder_type: Optional[str]
    model_directory: Optional[Path]


class ImageSearchConfig(ConfigBase):
    encoder: str
    encoder_type: Optional[str]
    model_directory: Optional[Path]


class SearchConfig(ConfigBase):
    asymmetric: Optional[TextSearchConfig]
    symmetric: Optional[TextSearchConfig]
    image: Optional[ImageSearchConfig]


class OpenAIProcessorConfig(ConfigBase):
    api_key: str
    chat_model: Optional[str] = "gpt-3.5-turbo"


class ConversationProcessorConfig(ConfigBase):
    conversation_logfile: Path
    openai: Optional[OpenAIProcessorConfig]
    enable_offline_chat: Optional[bool] = False


class ProcessorConfig(ConfigBase):
    conversation: Optional[ConversationProcessorConfig]


class AppConfig(ConfigBase):
    should_log_telemetry: bool


class FullConfig(ConfigBase):
    content_type: Optional[ContentConfig] = None
    search_type: Optional[SearchConfig] = None
    processor: Optional[ProcessorConfig] = None
    app: Optional[AppConfig] = AppConfig(should_log_telemetry=True)
    version: Optional[str] = None


class SearchResponse(ConfigBase):
    entry: str
    score: str
    additional: Optional[dict]


class Entry:
    raw: str
    compiled: str
    heading: Optional[str]
    file: Optional[str]

    def __init__(
        self, raw: str = None, compiled: str = None, heading: Optional[str] = None, file: Optional[str] = None
    ):
        self.raw = raw
        self.compiled = compiled
        self.heading = heading
        self.file = file

    def to_json(self) -> str:
        return json.dumps(self.__dict__, ensure_ascii=False)

    def __repr__(self) -> str:
        return self.__dict__.__repr__()

    @classmethod
    def from_dict(cls, dictionary: dict):
        return cls(
            raw=dictionary["raw"],
            compiled=dictionary["compiled"],
            file=dictionary.get("file", None),
            heading=dictionary.get("heading", None),
        )
