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


class LedgerSearchModel():
    def __init__(self, transactions, transaction_embeddings, symmetric_encoder, symmetric_cross_encoder, top_k):
        self.transactions = transactions
        self.transaction_embeddings = transaction_embeddings
        self.symmetric_encoder = symmetric_encoder
        self.symmetric_cross_encoder = symmetric_cross_encoder
        self.top_k = top_k


class ImageSearchModel():
    def __init__(self, image_names, image_embeddings, image_metadata_embeddings, image_encoder):
        self.image_names = image_names
        self.image_embeddings = image_embeddings
        self.image_metadata_embeddings = image_metadata_embeddings
        self.image_encoder = image_encoder


@dataclass
class SearchModels():
    notes_search: AsymmetricSearchModel = None
    ledger_search: LedgerSearchModel = None
    music_search: AsymmetricSearchModel = None
    image_search: ImageSearchModel = None
