# Standard Packages
from abc import ABC, abstractmethod
from typing import List, Set, Tuple

# Internal Packages
from khoj.utils.rawconfig import Entry


class BaseFilter(ABC):
    @abstractmethod
    def load(self, entries: List[Entry], *args, **kwargs):
        ...

    @abstractmethod
    def can_filter(self, raw_query: str) -> bool:
        ...

    @abstractmethod
    def apply(self, query: str, entries: List[Entry]) -> Tuple[str, Set[int]]:
        ...

    @abstractmethod
    def defilter(self, query: str) -> str:
        ...
