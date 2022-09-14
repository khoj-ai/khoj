# Standard Packages
import re
import fnmatch
import time
import logging
from collections import defaultdict

# Internal Packages
from src.search_filter.base_filter import BaseFilter
from src.utils.helpers import LRU


logger = logging.getLogger(__name__)


class FileFilter(BaseFilter):
    file_filter_regex = r'file:"(.+?)" ?'

    def __init__(self, entry_key='file'):
        self.entry_key = entry_key
        self.file_to_entry_map = defaultdict(set)
        self.cache = LRU()

    def load(self, entries, *args, **kwargs):
        start = time.time()
        for id, entry in enumerate(entries):
            self.file_to_entry_map[entry[self.entry_key]].add(id)
        end = time.time()
        logger.debug(f"Created file filter index: {end - start} seconds")

    def can_filter(self, raw_query):
        return re.search(self.file_filter_regex, raw_query) is not None

    def apply(self, raw_query, raw_entries):
        # Extract file filters from raw query
        start = time.time()
        raw_files_to_search = re.findall(self.file_filter_regex, raw_query)
        if not raw_files_to_search:
            return raw_query, set(range(len(raw_entries)))

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
            included_entry_indices = self.cache[cache_key]
            return query, included_entry_indices

        if not self.file_to_entry_map:
            self.load(raw_entries, regenerate=False)

        # Mark entries that contain any blocked_words for exclusion
        start = time.time()

        included_entry_indices = set.union(*[self.file_to_entry_map[entry_file]
                for entry_file in self.file_to_entry_map.keys()
                for search_file in files_to_search
                if fnmatch.fnmatch(entry_file, search_file)], set())
        if not included_entry_indices:
            return query, {}

        end = time.time()
        logger.debug(f"Mark entries satisfying filter: {end - start} seconds")

        # Cache results
        self.cache[cache_key] = included_entry_indices

        return query, included_entry_indices
