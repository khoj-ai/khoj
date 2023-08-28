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
    def get_filter_terms(self, query: str) -> List[str]:
        ...

    def can_filter(self, raw_query: str) -> bool:
        return len(self.get_filter_terms(raw_query)) > 0

    @abstractmethod
    def apply(self, query: str, entries: List[Entry]) -> Tuple[str, Set[int]]:
        ...

    @abstractmethod
    def defilter(self, query: str) -> str:
        ...
