import logging
import re
from pathlib import Path
from typing import Dict, List, Tuple

import urllib3
from bs4 import BeautifulSoup

from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

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

        # Extract Entries from specified plaintext files
        with timer("Extract entries from specified Plaintext files", logger):
            file_to_text_map, current_entries = PlaintextToEntries.extract_plaintext_entries(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256, raw_is_compiled=True)

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
                file_to_text_map=file_to_text_map,
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
    def extract_plaintext_entries(text_files: Dict[str, str]) -> Tuple[Dict, List[Entry]]:
        entries: List[str] = []
        entry_to_file_map: List[Tuple[str, str]] = []
        file_to_text_map = dict()
        for text_file in text_files:
            try:
                text_content = text_files[text_file]
                entries, entry_to_file_map = PlaintextToEntries.process_single_plaintext_file(
                    text_content, text_file, entries, entry_to_file_map
                )
                file_to_text_map[text_file] = text_content
            except Exception as e:
                logger.warning(f"Unable to read file: {text_file} as plaintext. Skipping file.")
                logger.warning(e, exc_info=True)

        # Extract Entries from specified plaintext files
        return file_to_text_map, PlaintextToEntries.convert_text_files_to_entries(entries, dict(entry_to_file_map))

    @staticmethod
    def process_single_plaintext_file(
        text_content: str,
        text_file: str,
        entries: List[str],
        entry_to_file_map: List[Tuple[str, str]],
    ) -> Tuple[List[str], List[Tuple[str, str]]]:
        if text_file.endswith(("html", "htm", "xml")):
            text_content = PlaintextToEntries.extract_html_content(text_content, text_file.split(".")[-1])
        entry_to_file_map += [(text_content, text_file)]
        entries.extend([text_content])
        return entries, entry_to_file_map

    @staticmethod
    def convert_text_files_to_entries(parsed_entries: List[str], entry_to_file_map: dict[str, str]) -> List[Entry]:
        "Convert each plaintext file into an entry"
        entries: List[Entry] = []
        for parsed_entry in parsed_entries:
            raw_filename = entry_to_file_map[parsed_entry]
            # Check if raw_filename is a URL. If so, save it as is. If not, convert it to a Path.
            if type(raw_filename) == str and re.search(r"^https?://", raw_filename):
                # Escape the URL to avoid issues with special characters
                entry_filename = urllib3.util.parse_url(raw_filename).url
            else:
                entry_filename = raw_filename

            # Append base filename to compiled entry for context to model
            entries.append(
                Entry(
                    raw=parsed_entry,
                    file=f"{entry_filename}",
                    compiled=f"{entry_filename}\n{parsed_entry}",
                    heading=entry_filename,
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} plaintext files to entries")
        return entries
