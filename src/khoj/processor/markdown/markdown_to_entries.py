# Standard Packages
import logging
import re
import urllib3
from pathlib import Path
from typing import Tuple, List

# Internal Packages
from khoj.processor.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.constants import empty_escape_sequences
from khoj.utils.rawconfig import Entry
from khoj.database.models import Entry as DbEntry, KhojUser


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
        with timer("Parse entries from Markdown files into dictionaries", logger):
            current_entries = MarkdownToEntries.convert_markdown_entries_to_maps(
                *MarkdownToEntries.extract_markdown_entries(files)
            )

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
    def extract_markdown_entries(markdown_files):
        "Extract entries by heading from specified Markdown files"

        # Regex to extract Markdown Entries by Heading

        entries = []
        entry_to_file_map = []
        for markdown_file in markdown_files:
            try:
                markdown_content = markdown_files[markdown_file]
                entries, entry_to_file_map = MarkdownToEntries.process_single_markdown_file(
                    markdown_content, markdown_file, entries, entry_to_file_map
                )
            except Exception as e:
                logger.warning(f"Unable to process file: {markdown_file}. This file will not be indexed.")
                logger.warning(e, exc_info=True)

        return entries, dict(entry_to_file_map)

    @staticmethod
    def process_single_markdown_file(
        markdown_content: str, markdown_file: Path, entries: List, entry_to_file_map: List
    ):
        markdown_heading_regex = r"^#"

        markdown_entries_per_file = []
        any_headings = re.search(markdown_heading_regex, markdown_content, flags=re.MULTILINE)
        for entry in re.split(markdown_heading_regex, markdown_content, flags=re.MULTILINE):
            # Add heading level as the regex split removed it from entries with headings
            prefix = "#" if entry.startswith("#") else "# " if any_headings else ""
            stripped_entry = entry.strip(empty_escape_sequences)
            if stripped_entry != "":
                markdown_entries_per_file.append(f"{prefix}{stripped_entry}")

        entry_to_file_map += zip(markdown_entries_per_file, [markdown_file] * len(markdown_entries_per_file))
        entries.extend(markdown_entries_per_file)
        return entries, entry_to_file_map

    @staticmethod
    def convert_markdown_entries_to_maps(parsed_entries: List[str], entry_to_file_map) -> List[Entry]:
        "Convert each Markdown entries into a dictionary"
        entries = []
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
            compiled_entry = f"{prefix}{parsed_entry}"
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

    @staticmethod
    def convert_markdown_maps_to_jsonl(entries: List[Entry]):
        "Convert each Markdown entry to JSON and collate as JSONL"
        return "".join([f"{entry.to_json()}\n" for entry in entries])
