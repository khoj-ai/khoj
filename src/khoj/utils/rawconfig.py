# System Packages
import json
import uuid
from typing import List, Optional

from pydantic import BaseModel

from khoj.utils.helpers import to_snake_case_from_dash


class ConfigBase(BaseModel):
    class Config:
        alias_generator = to_snake_case_from_dash
        populate_by_name = True

    def __getitem__(self, item):
        return getattr(self, item)

    def __setitem__(self, key, value):
        return setattr(self, key, value)


class LocationData(BaseModel):
    city: Optional[str]
    region: Optional[str]
    country: Optional[str]
    country_code: Optional[str]

    def __str__(self):
        parts = []
        if self.city:
            parts.append(self.city)
        if self.region:
            parts.append(self.region)
        if self.country:
            parts.append(self.country)
        return ", ".join(parts)


class FileFilterRequest(BaseModel):
    filename: str
    conversation_id: str


class FilesFilterRequest(BaseModel):
    filenames: List[str]
    conversation_id: str


class GithubRepoConfig(ConfigBase):
    name: str
    owner: str
    branch: Optional[str] = "master"


class GithubContentConfig(ConfigBase):
    pat_token: Optional[str] = None
    repos: List[GithubRepoConfig]


class NotionContentConfig(ConfigBase):
    token: str


class SearchResponse(ConfigBase):
    entry: str
    score: float
    cross_score: Optional[float] = None
    additional: Optional[dict] = None
    corpus_id: str


class FileData(BaseModel):
    name: str
    content: bytes
    file_type: str
    encoding: str | None = None


class FileAttachment(BaseModel):
    name: str
    content: str
    file_type: str
    size: int


class ChatRequestBody(BaseModel):
    q: str
    n: Optional[int] = 7
    d: Optional[float] = None
    stream: Optional[bool] = False
    title: Optional[str] = None
    conversation_id: Optional[str] = None
    turn_id: Optional[str] = None
    city: Optional[str] = None
    region: Optional[str] = None
    country: Optional[str] = None
    country_code: Optional[str] = None
    timezone: Optional[str] = None
    images: Optional[list[str]] = None
    files: Optional[list[FileAttachment]] = []
    create_new: Optional[bool] = False


class Entry:
    raw: str
    compiled: str
    heading: Optional[str]
    file: Optional[str]
    uri: Optional[str] = None
    corpus_id: str

    def __init__(
        self,
        raw: str = None,
        compiled: str = None,
        heading: Optional[str] = None,
        file: Optional[str] = None,
        uri: Optional[str] = None,
        corpus_id: uuid.UUID = None,
    ):
        self.raw = raw
        self.compiled = compiled
        self.heading = heading
        self.file = file
        self.corpus_id = str(corpus_id)
        if uri:
            self.uri = uri
        elif file and (file.startswith("http") or file.startswith("file://")):
            self.uri = file
        elif file:
            self.uri = f"file://{file}"
        else:
            self.uri = None

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
            corpus_id=dictionary.get("corpus_id", None),
            uri=dictionary.get("uri", None),
        )
