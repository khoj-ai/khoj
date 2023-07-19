# Standard Packages
import glob
import logging
import re
import urllib3
from pathlib import Path
from typing import List

# Internal Packages
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.helpers import get_absolute_path, is_none_or_empty, timer
from khoj.utils.constants import empty_escape_sequences
from khoj.utils.jsonl import compress_jsonl_data
from khoj.utils.rawconfig import Entry, TextContentConfig


logger = logging.getLogger(__name__)


class MarkdownToJsonl(TextToJsonl):
    def __init__(self, config: TextContentConfig):
        super().__init__(config)
        self.config = config

    # Define Functions
    def process(self, previous_entries=[]):
        # Extract required fields from config
        markdown_files, markdown_file_filter, output_file = (
            self.config.input_files,
            self.config.input_filter,
            self.config.compressed_jsonl,
        )

        # Input Validation
        if is_none_or_empty(markdown_files) and is_none_or_empty(markdown_file_filter):
            print("At least one of markdown-files or markdown-file-filter is required to be specified")
            exit(1)

        # Get Markdown Files to Process
        markdown_files = MarkdownToJsonl.get_markdown_files(markdown_files, markdown_file_filter)

        # Extract Entries from specified Markdown files
        with timer("Parse entries from Markdown files into dictionaries", logger):
            current_entries = MarkdownToJsonl.convert_markdown_entries_to_maps(
                *MarkdownToJsonl.extract_markdown_entries(markdown_files)
            )

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            entries_with_ids = TextToJsonl.mark_entries_for_update(
                current_entries, previous_entries, key="compiled", logger=logger
            )

        with timer("Write markdown entries to JSONL file", logger):
            # Process Each Entry from All Notes Files
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            jsonl_data = MarkdownToJsonl.convert_markdown_maps_to_jsonl(entries)

            # Compress JSONL formatted Data
            compress_jsonl_data(jsonl_data, output_file)

        return entries_with_ids

    @staticmethod
    def get_markdown_files(markdown_files=None, markdown_file_filters=None):
        "Get Markdown files to process"
        absolute_markdown_files, filtered_markdown_files = set(), set()
        if markdown_files:
            absolute_markdown_files = {get_absolute_path(markdown_file) for markdown_file in markdown_files}
        if markdown_file_filters:
            filtered_markdown_files = {
                filtered_file
                for markdown_file_filter in markdown_file_filters
                for filtered_file in glob.glob(get_absolute_path(markdown_file_filter), recursive=True)
            }

        all_markdown_files = sorted(absolute_markdown_files | filtered_markdown_files)

        files_with_non_markdown_extensions = {
            md_file
            for md_file in all_markdown_files
            if not md_file.endswith(".md") and not md_file.endswith(".markdown")
        }

        if any(files_with_non_markdown_extensions):
            logger.warning(
                f"[Warning] There maybe non markdown-mode files in the input set: {files_with_non_markdown_extensions}"
            )

        logger.debug(f"Processing files: {all_markdown_files}")

        return all_markdown_files

    @staticmethod
    def extract_markdown_entries(markdown_files):
        "Extract entries by heading from specified Markdown files"

        # Regex to extract Markdown Entries by Heading

        entries = []
        entry_to_file_map = []
        for markdown_file in markdown_files:
            with open(markdown_file, "r", encoding="utf8") as f:
                markdown_content = f.read()
                entries, entry_to_file_map = MarkdownToJsonl.process_single_markdown_file(
                    markdown_content, markdown_file, entries, entry_to_file_map
                )

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
