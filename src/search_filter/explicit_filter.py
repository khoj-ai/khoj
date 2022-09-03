# Standard Packages
import re
import time
import pickle
import logging

# External Packages
import torch

# Internal Packages
from src.utils.helpers import resolve_absolute_path
from src.utils.config import SearchType


logger = logging.getLogger(__name__)


class ExplicitFilter:
    # Filter Regex
    required_regex = r'\+"(\w+)" ?'
    blocked_regex = r'\-"(\w+)" ?'

    def __init__(self, filter_directory, search_type: SearchType, entry_key='raw'):
        self.filter_file = resolve_absolute_path(filter_directory / f"{search_type.name.lower()}_explicit_filter_entry_word_sets.pkl")
        self.entry_key = entry_key
        self.search_type = search_type
        self.entries_by_word_set = None
        self.cache = {}


    def load(self, entries, regenerate=False):
        if self.filter_file.exists() and not regenerate:
            start = time.time()
            with self.filter_file.open('rb') as f:
                self.entries_by_word_set = pickle.load(f)
            end = time.time()
            logger.debug(f"Load {self.search_type} entries by word set from file: {end - start} seconds")
        else:
            start = time.time()
            self.cache = {}  # Clear cache on (re-)generating entries_by_word_set
            entry_splitter = r',|\.| |\]|\[\(|\)|\{|\}|\t|\n|\:'
            self.entries_by_word_set = [set(word.lower()
                                    for word
                                    in re.split(entry_splitter, entry[self.entry_key])
                                    if word != "")
                                for entry in entries]
            with self.filter_file.open('wb') as f:
                pickle.dump(self.entries_by_word_set, f)
            end = time.time()
            logger.debug(f"Convert all {self.search_type} entries to word sets: {end - start} seconds")

        return self.entries_by_word_set


    def can_filter(self, raw_query):
        "Check if query contains explicit filters"
        # Extract explicit query portion with required, blocked words to filter from natural query
        required_words = re.findall(self.required_regex, raw_query)
        blocked_words = re.findall(self.blocked_regex, raw_query)

        return len(required_words) != 0 or len(blocked_words) != 0


    def apply(self, raw_query, entries, embeddings):
        "Find entries containing required and not blocked words specified in query"
        # Separate natural query from explicit required, blocked words filters
        start = time.time()

        required_words = set([word.lower() for word in re.findall(self.required_regex, raw_query)])
        blocked_words = set([word.lower() for word in re.findall(self.blocked_regex, raw_query)])
        query = re.sub(self.blocked_regex, '', re.sub(self.required_regex, '', raw_query))

        end = time.time()
        logger.debug(f"Extract required, blocked filters from query: {end - start} seconds")

        if len(required_words) == 0 and len(blocked_words) == 0:
            return query, raw_entries, raw_embeddings

        # Return item from cache if exists
        cache_key = tuple(sorted(required_words)), tuple(sorted(blocked_words))
        if cache_key in self.cache:
            logger.info(f"Explicit filter results from cache")
            entries, embeddings = self.cache[cache_key]
            return query, entries, embeddings

        if not self.entries_by_word_set:
            self.load(entries, regenerate=False)

        # track id of entries to exclude
        start = time.time()
        entries_to_exclude = set()

        # mark entries that do not contain all required_words for exclusion
        if len(required_words) > 0:
            for id, words_in_entry in enumerate(self.entries_by_word_set):
                if not required_words.issubset(words_in_entry):
                    entries_to_exclude.add(id)

        # mark entries that contain any blocked_words for exclusion
        if len(blocked_words) > 0:
            for id, words_in_entry in enumerate(self.entries_by_word_set):
                if words_in_entry.intersection(blocked_words):
                    entries_to_exclude.add(id)
        end = time.time()
        logger.debug(f"Mark entries not satisfying filter: {end - start} seconds")

        # delete entries (and their embeddings) marked for exclusion
        start = time.time()
        for id in sorted(list(entries_to_exclude), reverse=True):
            del entries[id]
            embeddings = torch.cat((embeddings[:id], embeddings[id+1:]))
        end = time.time()
        logger.debug(f"Delete entries not satisfying filter: {end - start} seconds")

        # Cache results
        self.cache[cache_key] = entries, embeddings

        return query, entries, embeddings
