# Standard Packages
from abc import ABC, abstractmethod


class BaseFilter(ABC):
    @abstractmethod
    def load(self, *args, **kwargs):
        pass

    @abstractmethod
    def can_filter(self, raw_query:str) -> bool:
        pass

    @abstractmethod
    def apply(self, query:str, raw_entries:list[str]) -> tuple[str, set[int]]:
        pass