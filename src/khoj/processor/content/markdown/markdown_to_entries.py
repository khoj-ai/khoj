import logging
import re
from pathlib import Path
from typing import List, Tuple

import urllib3
from langchain.text_splitter import MarkdownHeaderTextSplitter

from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

logger = logging.getLogger(__name__)


class MarkdownToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(
        self, files: dict[str, str] = None, full_corpus: bool = True, user: KhojUser = None, regenerate: bool = False
    ) -> Tuple[int, int]:
        # Extract required fields from config
        if not full_corpus:
            deletion_file_names = set([file for file in files if files[file] == ""])
            files_to_process = set(files) - deletion_file_names
            files = {file: files[file] for file in files_to_process}
        else:
            deletion_file_names = None

        # Extract Entries from specified Markdown files
        with timer("Extract entries from specified Markdown files", logger):
            current_entries = MarkdownToEntries.extract_markdown_entries(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                current_entries,
                DbEntry.EntryType.MARKDOWN,
                DbEntry.EntrySource.COMPUTER,
                "compiled",
                logger,
                deletion_file_names,
                user,
                regenerate=regenerate,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_markdown_entries(markdown_files) -> List[Entry]:
        "Extract entries by heading from specified Markdown files"
        entries: List[str] = []
        entry_to_file_map: List[Tuple[str, Path]] = []
        for markdown_file in markdown_files:
            try:
                markdown_content = markdown_files[markdown_file]
                entries, entry_to_file_map = MarkdownToEntries.process_single_markdown_file(
                    markdown_content, markdown_file, entries, entry_to_file_map
                )
            except Exception as e:
                logger.warning(
                    f"Unable to process file: {markdown_file}. This file will not be indexed.\n{e}", exc_info=True
                )

        return MarkdownToEntries.convert_markdown_entries_to_maps(entries, dict(entry_to_file_map))

    @staticmethod
    def process_single_markdown_file(
        markdown_content: str, markdown_file: Path, entries: List[str], entry_to_file_map: List[Tuple[str, Path]]
    ):
        headers_to_split_on = [("#", "1"), ("##", "2"), ("###", "3"), ("####", "4"), ("#####", "5"), ("######", "6")]
        reversed_headers_to_split_on = list(reversed(headers_to_split_on))
        markdown_entries_per_file: List[str] = []
        previous_section_metadata, current_section_metadata = None, None

        splitter = MarkdownHeaderTextSplitter(headers_to_split_on, strip_headers=False, return_each_line=True)
        for section in splitter.split_text(markdown_content):
            current_section_metadata = section.metadata.copy()
            # Append the section's content to the last entry if the metadata is the same
            if previous_section_metadata == current_section_metadata:
                markdown_entries_per_file[-1] = f"{markdown_entries_per_file[-1]}\n{section.page_content}"
            # Insert new entry with it's heading ancestry, if the section is under a new heading
            else:
                # Drop the current heading from the metadata. It is already in the section content
                if section.metadata:
                    section.metadata.pop(max(section.metadata))
                # Prepend the markdown section's heading ancestry
                for heading in reversed_headers_to_split_on:
                    if heading[1] in section.metadata:
                        section.page_content = f"{heading[0]} {section.metadata[heading[1]]}\n{section.page_content}"
                previous_section_metadata = current_section_metadata
                markdown_entries_per_file += [section.page_content]

        entry_to_file_map += zip(markdown_entries_per_file, [markdown_file] * len(markdown_entries_per_file))
        entries.extend(markdown_entries_per_file)
        return entries, entry_to_file_map

    @staticmethod
    def convert_markdown_entries_to_maps(parsed_entries: List[str], entry_to_file_map) -> List[Entry]:
        "Convert each Markdown entries into a dictionary"
        entries: List[Entry] = []
        for parsed_entry in parsed_entries:
            raw_filename = entry_to_file_map[parsed_entry]

            # Check if raw_filename is a URL. If so, save it as is. If not, convert it to a Path.
            if type(raw_filename) == str and re.search(r"^https?://", raw_filename):
                # Escape the URL to avoid issues with special characters
                entry_filename = urllib3.util.parse_url(raw_filename).url
            else:
                entry_filename = str(Path(raw_filename))
            stem = Path(raw_filename).stem

            heading = parsed_entry.splitlines()[0] if re.search("^#+\s", parsed_entry) else ""
            # Append base filename to compiled entry for context to model
            # Increment heading level for heading entries and make filename as its top level heading
            prefix = f"# {stem}\n#" if heading else f"# {stem}\n"
            compiled_entry = f"{entry_filename}\n{prefix}{parsed_entry}"
            entries.append(
                Entry(
                    compiled=compiled_entry,
                    raw=parsed_entry,
                    heading=f"{prefix}{heading}",
                    file=f"{entry_filename}",
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} markdown entries to dictionaries")

        return entries
