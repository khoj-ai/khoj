# System Packages
from enum import Enum
from dataclasses import dataclass


class SearchType(str, Enum):
    Notes = "notes"
    Ledger = "ledger"
    Music = "music"
    Image = "image"


@dataclass
class SearchSettings():
    notes_search_enabled: bool = False
    ledger_search_enabled: bool = False
    music_search_enabled: bool = False
    image_search_enabled: bool = False


class AsymmetricSearchModel():
    def __init__(self, entries, corpus_embeddings, bi_encoder, cross_encoder, top_k):
        self.entries = entries
        self.corpus_embeddings = corpus_embeddings
        self.bi_encoder = bi_encoder
        self.cross_encoder = cross_encoder
        self.top_k = top_k


@dataclass
class SearchModels():
    notes_search: AsymmetricSearchModel = None
