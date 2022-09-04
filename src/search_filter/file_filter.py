# Standard Packages
import re
import fnmatch
import time
import logging

# External Packages
import torch

# Internal Packages
from src.search_filter.base_filter import BaseFilter
from src.utils.helpers import LRU


logger = logging.getLogger(__name__)


class FileFilter(BaseFilter):
    file_filter_regex = r'file:"(.+?)" ?'

    def __init__(self, entry_key='file'):
        self.entry_key = entry_key
        self.cache = LRU()

    def load(self, *args, **kwargs):
        pass

    def can_filter(self, raw_query):
        return re.search(self.file_filter_regex, raw_query) is not None

    def apply(self, raw_query, raw_entries, raw_embeddings):
        # Extract file filters from raw query
        start = time.time()
        raw_files_to_search = re.findall(self.file_filter_regex, raw_query)
        if not raw_files_to_search:
            return raw_query, raw_entries, raw_embeddings

        # Convert simple file filters with no path separator into regex
        # e.g. "file:notes.org" -> "file:.*notes.org"
        files_to_search = []
        for file in sorted(raw_files_to_search):
            if '/' not in file and '\\' not in file and '*' not in file:
                files_to_search += [f'*{file}']
            else:
                files_to_search += [file]
        end = time.time()
        logger.debug(f"Extract files_to_search from query: {end - start} seconds")

        # Return item from cache if exists
        query = re.sub(self.file_filter_regex, '', raw_query).strip()
        cache_key = tuple(files_to_search)
        if cache_key in self.cache:
            logger.info(f"Return file filter results from cache")
            entries, embeddings = self.cache[cache_key]
            return query, entries, embeddings

        # Mark entries that contain any blocked_words for exclusion
        start = time.time()

        included_entry_indices = [id for id, entry in enumerate(raw_entries) for search_file in files_to_search if fnmatch.fnmatch(entry[self.entry_key], search_file)]
        if not included_entry_indices:
            return query, [], torch.empty(0)

        end = time.time()
        logger.debug(f"Mark entries satisfying filter: {end - start} seconds")

        # Get entries (and associated embeddings) satisfying file filters
        start = time.time()

        entries = [raw_entries[id] for id in included_entry_indices]
        embeddings = torch.index_select(raw_embeddings, 0, torch.tensor(list(included_entry_indices)))

        end = time.time()
        logger.debug(f"Keep entries satisfying filter: {end - start} seconds")

        # Cache results
        self.cache[cache_key] = entries, embeddings

        return query, entries, embeddings
