# Standard Packages
from abc import ABC, abstractmethod
from typing import List, Tuple

# External Packages
import torch


class BaseFilter(ABC):
    @abstractmethod
    def load(self, *args, **kwargs):
        pass

    @abstractmethod
    def can_filter(self, raw_query:str) -> bool:
        pass

    @abstractmethod
    def apply(self, query:str, raw_entries:List[str], raw_embeddings: torch.Tensor) -> Tuple[str, List[str], torch.Tensor]:
        pass