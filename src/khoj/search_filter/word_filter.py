import logging
import re
from collections import defaultdict
from typing import List

from khoj.search_filter.base_filter import BaseFilter
from khoj.utils.helpers import LRU

logger = logging.getLogger(__name__)


class WordFilter(BaseFilter):
    # Filter Regex
    required_regex = r'\+"([a-zA-Z0-9_-]+)" ?'
    blocked_regex = r'\-"([a-zA-Z0-9_-]+)" ?'

    def __init__(self, entry_key="raw"):
        self.entry_key = entry_key
        self.word_to_entry_index = defaultdict(set)
        self.cache = LRU()

    def get_filter_terms(self, query: str) -> List[str]:
        "Get all filter terms in query"
        required_terms = [f"+{required_term}" for required_term in re.findall(self.required_regex, query)]
        blocked_terms = [f"-{blocked_term}" for blocked_term in re.findall(self.blocked_regex, query)]
        return required_terms + blocked_terms

    def defilter(self, query: str) -> str:
        return re.sub(self.blocked_regex, "", re.sub(self.required_regex, "", query)).strip()
