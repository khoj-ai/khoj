# Standard Packages
import logging
from pathlib import Path
from typing import List, Tuple

# Internal Packages
from khoj.processor.text_to_jsonl import TextEmbeddings
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry
from database.models import Embeddings


logger = logging.getLogger(__name__)


class PlaintextToJsonl(TextEmbeddings):
    # Define Functions
    def process(
        self, previous_entries: List[Entry] = [], files: dict[str, str] = None, full_corpus: bool = True
    ) -> List[Tuple[int, Entry]]:
        if not full_corpus:
            deletion_file_names = set([file for file in files if files[file] == ""])
            files_to_process = set(files) - deletion_file_names
            files = {file: files[file] for file in files_to_process}
        else:
            deletion_file_names = None

        # Extract Entries from specified plaintext files
        with timer("Parse entries from plaintext files", logger):
            current_entries = PlaintextToJsonl.convert_plaintext_entries_to_maps(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            entries_with_ids = self.update_embeddings(
                current_entries,
                Embeddings.EmbeddingsType.PLAINTEXT,
                key="compiled",
                logger=logger,
                deletion_filenames=deletion_file_names,
            )

        return entries_with_ids

    @staticmethod
    def convert_plaintext_entries_to_maps(entry_to_file_map: dict) -> List[Entry]:
        "Convert each plaintext entries into a dictionary"
        entries = []
        for file, entry in entry_to_file_map.items():
            entries.append(
                Entry(
                    raw=entry,
                    file=file,
                    compiled=f"{Path(file).stem}\n{entry}",
                    heading=Path(file).stem,
                )
            )
        return entries

    @staticmethod
    def convert_entries_to_jsonl(entries: List[Entry]):
        "Convert each entry to JSON and collate as JSONL"
        return "".join([f"{entry.to_json()}\n" for entry in entries])
