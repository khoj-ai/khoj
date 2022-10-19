# Standard Packages
import re
import time
import logging
from collections import defaultdict

# Internal Packages
from src.search_filter.base_filter import BaseFilter
from src.utils.helpers import LRU


logger = logging.getLogger(__name__)


class WordFilter(BaseFilter):
    # Filter Regex
    required_regex = r'\+"([a-zA-Z0-9_-]+)" ?'
    blocked_regex = r'\-"([a-zA-Z0-9_-]+)" ?'

    def __init__(self, entry_key='raw'):
        self.entry_key = entry_key
        self.word_to_entry_index = defaultdict(set)
        self.cache = LRU()


    def load(self, entries, regenerate=False):
        start = time.time()
        self.cache = {}  # Clear cache on filter (re-)load
        entry_splitter = r',|\.| |\]|\[\(|\)|\{|\}|\<|\>|\t|\n|\:|\;|\?|\!|\(|\)|\&|\^|\$|\@|\%|\+|\=|\/|\\|\||\~|\`|\"|\''
        # Create map of words to entries they exist in
        for entry_index, entry in enumerate(entries):
            for word in re.split(entry_splitter, getattr(entry, self.entry_key).lower()):
                if word == '':
                    continue
                self.word_to_entry_index[word].add(entry_index)
        end = time.time()
        logger.debug(f"Created word filter index: {end - start} seconds")

        return self.word_to_entry_index


    def can_filter(self, raw_query):
        "Check if query contains word filters"
        required_words = re.findall(self.required_regex, raw_query)
        blocked_words = re.findall(self.blocked_regex, raw_query)

        return len(required_words) != 0 or len(blocked_words) != 0


    def apply(self, raw_query, raw_entries):
        "Find entries containing required and not blocked words specified in query"
        # Separate natural query from required, blocked words filters
        start = time.time()

        required_words = set([word.lower() for word in re.findall(self.required_regex, raw_query)])
        blocked_words = set([word.lower() for word in re.findall(self.blocked_regex, raw_query)])
        query = re.sub(self.blocked_regex, '', re.sub(self.required_regex, '', raw_query)).strip()

        end = time.time()
        logger.debug(f"Extract required, blocked filters from query: {end - start} seconds")

        if len(required_words) == 0 and len(blocked_words) == 0:
            return query, set(range(len(raw_entries)))

        # Return item from cache if exists
        cache_key = tuple(sorted(required_words)), tuple(sorted(blocked_words))
        if cache_key in self.cache:
            logger.info(f"Return word filter results from cache")
            included_entry_indices = self.cache[cache_key]
            return query, included_entry_indices

        if not self.word_to_entry_index:
            self.load(raw_entries, regenerate=False)

        start = time.time()

        # mark entries that contain all required_words for inclusion
        entries_with_all_required_words = set(range(len(raw_entries)))
        if len(required_words) > 0:
            entries_with_all_required_words = set.intersection(*[self.word_to_entry_index.get(word, set()) for word in required_words])

        # mark entries that contain any blocked_words for exclusion
        entries_with_any_blocked_words = set()
        if len(blocked_words) > 0:
            entries_with_any_blocked_words = set.union(*[self.word_to_entry_index.get(word, set()) for word in blocked_words])

        end = time.time()
        logger.debug(f"Mark entries satisfying filter: {end - start} seconds")

        # get entries satisfying inclusion and exclusion filters
        included_entry_indices = entries_with_all_required_words - entries_with_any_blocked_words

        # Cache results
        self.cache[cache_key] = included_entry_indices

        return query, included_entry_indices
