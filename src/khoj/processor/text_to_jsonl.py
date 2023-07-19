# Standard Packages
from abc import ABC, abstractmethod
import hashlib
import logging
from typing import Callable, List, Tuple
from khoj.utils.helpers import timer

# Internal Packages
from khoj.utils.rawconfig import Entry, TextConfigBase


logger = logging.getLogger(__name__)


class TextToJsonl(ABC):
    def __init__(self, config: TextConfigBase):
        self.config = config

    @abstractmethod
    def process(self, previous_entries: List[Entry] = []) -> List[Tuple[int, Entry]]:
        ...

    @staticmethod
    def hash_func(key: str) -> Callable:
        return lambda entry: hashlib.md5(bytes(getattr(entry, key), encoding="utf-8")).hexdigest()

    @staticmethod
    def split_entries_by_max_tokens(
        entries: List[Entry], max_tokens: int = 256, max_word_length: int = 500
    ) -> List[Entry]:
        "Split entries if compiled entry length exceeds the max tokens supported by the ML model."
        chunked_entries: List[Entry] = []
        for entry in entries:
            # Split entry into words
            compiled_entry_words = [word for word in entry.compiled.split(" ") if word != ""]

            # Drop long words instead of having entry truncated to maintain quality of entry processed by models
            compiled_entry_words = [word for word in compiled_entry_words if len(word) <= max_word_length]

            # Split entry into chunks of max tokens
            for chunk_index in range(0, len(compiled_entry_words), max_tokens):
                compiled_entry_words_chunk = compiled_entry_words[chunk_index : chunk_index + max_tokens]
                compiled_entry_chunk = " ".join(compiled_entry_words_chunk)

                # Prepend heading to all other chunks, the first chunk already has heading from original entry
                if chunk_index > 0:
                    # Snip heading to avoid crossing max_tokens limit
                    # Keep last 100 characters of heading as entry heading more important than filename
                    snipped_heading = entry.heading[-100:]
                    compiled_entry_chunk = f"{snipped_heading}.\n{compiled_entry_chunk}"

                chunked_entries.append(
                    Entry(
                        compiled=compiled_entry_chunk,
                        raw=entry.raw,
                        heading=entry.heading,
                        file=entry.file,
                    )
                )

        return chunked_entries

    @staticmethod
    def mark_entries_for_update(
        current_entries: List[Entry], previous_entries: List[Entry], key="compiled", logger: logging.Logger = None
    ) -> List[Tuple[int, Entry]]:
        # Hash all current and previous entries to identify new entries
        with timer("Hash previous, current entries", logger):
            current_entry_hashes = list(map(TextToJsonl.hash_func(key), current_entries))
            previous_entry_hashes = list(map(TextToJsonl.hash_func(key), previous_entries))

        with timer("Identify, Mark, Combine new, existing entries", logger):
            hash_to_current_entries = dict(zip(current_entry_hashes, current_entries))
            hash_to_previous_entries = dict(zip(previous_entry_hashes, previous_entries))

            # All entries that did not exist in the previous set are to be added
            new_entry_hashes = set(current_entry_hashes) - set(previous_entry_hashes)
            # All entries that exist in both current and previous sets are kept
            existing_entry_hashes = set(current_entry_hashes) & set(previous_entry_hashes)

            # load new entries in the order in which they are processed for a stable sort
            new_entries = [
                (current_entry_hashes.index(entry_hash), hash_to_current_entries[entry_hash])
                for entry_hash in new_entry_hashes
            ]
            new_entries_sorted = sorted(new_entries, key=lambda e: e[0])
            # Mark new entries with -1 id to flag for later embeddings generation
            new_entries_sorted = [(-1, entry[1]) for entry in new_entries_sorted]

            # Set id of existing entries to their previous ids to reuse their existing encoded embeddings
            existing_entries = [
                (previous_entry_hashes.index(entry_hash), hash_to_previous_entries[entry_hash])
                for entry_hash in existing_entry_hashes
            ]
            existing_entries_sorted = sorted(existing_entries, key=lambda e: e[0])

            entries_with_ids = existing_entries_sorted + new_entries_sorted

        return entries_with_ids

    @staticmethod
    def convert_text_maps_to_jsonl(entries: List[Entry]) -> str:
        # Convert each entry to JSON and write to JSONL file
        return "".join([f"{entry.to_json()}\n" for entry in entries])
