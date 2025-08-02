from abc import ABC, abstractmethod
from typing import List


class BaseFilter(ABC):
    @abstractmethod
    def get_filter_terms(self, query: str) -> List[str]: ...

    def can_filter(self, raw_query: str) -> bool:
        return len(self.get_filter_terms(raw_query)) > 0

    @abstractmethod
    def defilter(self, query: str) -> str: ...
