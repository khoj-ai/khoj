# Standard Packages
from abc import ABC, abstractmethod

# Internal Packages
from src.utils.rawconfig import Entry


class BaseFilter(ABC):
    @abstractmethod
    def load(self, entries: list[Entry], *args, **kwargs): ...

    @abstractmethod
    def can_filter(self, raw_query:str) -> bool: ...

    @abstractmethod
    def apply(self, query:str, entries: list[Entry]) -> tuple[str, set[int]]: ...
