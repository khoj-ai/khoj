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


