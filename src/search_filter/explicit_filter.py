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
    def __init__(self, filter_directory, search_type: SearchType, entry_key='raw'):
        self.filter_file = resolve_absolute_path(filter_directory / f"{search_type.name.lower()}_explicit_filter_entry_word_sets.pkl")
        self.entry_key = entry_key
        self.search_type = search_type


    def load(self, entries, regenerate=False):
        if self.filter_file.exists() and not regenerate:
            start = time.time()
            with self.filter_file.open('rb') as f:
                entries_by_word_set = pickle.load(f)
            end = time.time()
            logger.debug(f"Load {self.search_type} entries by word set from file: {end - start} seconds")
        else:
            start = time.time()
            entry_splitter = r',|\.| |\]|\[\(|\)|\{|\}|\t|\n|\:'
            entries_by_word_set = [set(word.lower()
                                    for word
                                    in re.split(entry_splitter, entry[self.entry_key])
                                    if word != "")
                                for entry in entries]
            with self.filter_file.open('wb') as f:
                pickle.dump(entries_by_word_set, f)
            end = time.time()
            logger.debug(f"Convert all {self.search_type} entries to word sets: {end - start} seconds")

        return entries_by_word_set


    def can_filter(self, raw_query):
        "Check if query contains explicit filters"
        # Extract explicit query portion with required, blocked words to filter from natural query
        required_words = set([word[1:].lower() for word in raw_query.split() if word.startswith("+")])
        blocked_words = set([word[1:].lower() for word in raw_query.split() if word.startswith("-")])

        return len(required_words) != 0 or len(blocked_words) != 0


    def apply(self, raw_query, entries, embeddings):
        "Find entries containing required and not blocked words specified in query"
        # Separate natural query from explicit required, blocked words filters
        start = time.time()
        query = " ".join([word for word in raw_query.split() if not word.startswith("+") and not word.startswith("-")])
        required_words = set([word[1:].lower() for word in raw_query.split() if word.startswith("+")])
        blocked_words = set([word[1:].lower() for word in raw_query.split() if word.startswith("-")])
        end = time.time()
        logger.debug(f"Time to extract required, blocked words: {end - start} seconds")

        if len(required_words) == 0 and len(blocked_words) == 0:
            return query, entries, embeddings

        # load or generate word set for each entry
        entries_by_word_set = self.load(entries, regenerate=False)

        # track id of entries to exclude
        start = time.time()
        entries_to_exclude = set()

        # mark entries that do not contain all required_words for exclusion
        if len(required_words) > 0:
            for id, words_in_entry in enumerate(entries_by_word_set):
                if not required_words.issubset(words_in_entry):
                    entries_to_exclude.add(id)

        # mark entries that contain any blocked_words for exclusion
        if len(blocked_words) > 0:
            for id, words_in_entry in enumerate(entries_by_word_set):
                if words_in_entry.intersection(blocked_words):
                    entries_to_exclude.add(id)
        end = time.time()
        logger.debug(f"Mark entries to filter: {end - start} seconds")

        # delete entries (and their embeddings) marked for exclusion
        start = time.time()
        for id in sorted(list(entries_to_exclude), reverse=True):
            del entries[id]
            embeddings = torch.cat((embeddings[:id], embeddings[id+1:]))
        end = time.time()
        logger.debug(f"Remove entries to filter from embeddings: {end - start} seconds")

        return query, entries, embeddings
