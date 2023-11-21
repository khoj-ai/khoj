# Standard Packages
import logging
from pathlib import Path
from typing import List, Tuple
from bs4 import BeautifulSoup


# Internal Packages
from khoj.processor.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry
from khoj.database.models import Entry as DbEntry, KhojUser


logger = logging.getLogger(__name__)


class PlaintextToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(
        self, files: dict[str, str] = None, full_corpus: bool = True, user: KhojUser = None, regenerate: bool = False
    ) -> Tuple[int, int]:
        if not full_corpus:
            deletion_file_names = set([file for file in files if files[file] == ""])
            files_to_process = set(files) - deletion_file_names
            files = {file: files[file] for file in files_to_process}
        else:
            deletion_file_names = None

        with timer("Scrub plaintext files and extract text", logger):
            for file in files:
                try:
                    plaintext_content = files[file]
                    if file.endswith(("html", "htm", "xml")):
                        plaintext_content = PlaintextToEntries.extract_html_content(
                            plaintext_content, file.split(".")[-1]
                        )
                    files[file] = plaintext_content
                except Exception as e:
                    logger.warning(f"Unable to read file: {file} as plaintext. Skipping file.")
                    logger.warning(e, exc_info=True)

        # Extract Entries from specified plaintext files
        with timer("Parse entries from plaintext files", logger):
            current_entries = PlaintextToEntries.convert_plaintext_entries_to_maps(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                current_entries,
                DbEntry.EntryType.PLAINTEXT,
                DbEntry.EntrySource.COMPUTER,
                key="compiled",
                logger=logger,
                deletion_filenames=deletion_file_names,
                user=user,
                regenerate=regenerate,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_html_content(markup_content: str, markup_type: str):
        "Extract content from HTML"
        if markup_type == "xml":
            soup = BeautifulSoup(markup_content, "xml")
        else:
            soup = BeautifulSoup(markup_content, "html.parser")
        return soup.get_text(strip=True, separator="\n")

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
