# Standard Packages
from abc import ABC, abstractmethod
from typing import Iterable

# Internal Packages
from src.utils.rawconfig import TextContentConfig


class TextToJsonl(ABC):
    def __init__(self, config: TextContentConfig):
        self.config = config

    @abstractmethod
    def process(self, previous_entries: Iterable[tuple[int, dict]]=None) -> list[tuple[int, dict]]: ...
