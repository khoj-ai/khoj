from enum import Enum


class SearchType(str, Enum):
    Notes = "notes"
    Ledger = "ledger"
    Music = "music"
    Image = "image"

