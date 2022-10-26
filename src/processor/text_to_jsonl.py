# Standard Packages
from abc import ABC, abstractmethod
import hashlib
import time
import logging
from typing import Callable

# Internal Packages
from src.utils.rawconfig import Entry, TextContentConfig


logger = logging.getLogger(__name__)


class TextToJsonl(ABC):
    def __init__(self, config: TextContentConfig):
        self.config = config

    @abstractmethod
    def process(self, previous_entries: list[Entry]=None) -> list[tuple[int, Entry]]: ...

    @staticmethod
    def hash_func(key: str) -> Callable:
        return lambda entry: hashlib.md5(bytes(getattr(entry, key), encoding='utf-8')).hexdigest()

    def mark_entries_for_update(self, current_entries: list[Entry], previous_entries: list[Entry], key='compiled', logger=None) -> list[tuple[int, Entry]]:
        # Hash all current and previous entries to identify new entries
        start = time.time()
        current_entry_hashes = list(map(TextToJsonl.hash_func(key), current_entries))
        previous_entry_hashes = list(map(TextToJsonl.hash_func(key), previous_entries))
        end = time.time()
        logger.debug(f"Hash previous, current entries: {end - start} seconds")

        start = time.time()
        hash_to_current_entries = dict(zip(current_entry_hashes, current_entries))
        hash_to_previous_entries = dict(zip(previous_entry_hashes, previous_entries))

        # All entries that did not exist in the previous set are to be added
        new_entry_hashes = set(current_entry_hashes) - set(previous_entry_hashes)
        # All entries that exist in both current and previous sets are kept
        existing_entry_hashes = set(current_entry_hashes) & set(previous_entry_hashes)

        # Mark new entries with -1 id to flag for later embeddings generation
        new_entries = [
            (-1, hash_to_current_entries[entry_hash])
            for entry_hash in new_entry_hashes
        ]
        # Set id of existing entries to their previous ids to reuse their existing encoded embeddings
        existing_entries = [
            (previous_entry_hashes.index(entry_hash), hash_to_previous_entries[entry_hash])
            for entry_hash in existing_entry_hashes
        ]

        existing_entries_sorted = sorted(existing_entries, key=lambda e: e[0])
        entries_with_ids = existing_entries_sorted + new_entries
        end = time.time()
        logger.debug(f"Identify, Mark, Combine new, existing entries: {end - start} seconds")

        return entries_with_ids