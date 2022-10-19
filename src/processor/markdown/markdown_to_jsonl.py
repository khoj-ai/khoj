# Standard Packages
import glob
import re
import logging
import time

# Internal Packages
from src.processor.text_to_jsonl import TextToJsonl
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.constants import empty_escape_sequences
from src.utils.jsonl import dump_jsonl, compress_jsonl_data
from src.utils.rawconfig import Entry


logger = logging.getLogger(__name__)


class MarkdownToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries=None):
        # Extract required fields from config
        markdown_files, markdown_file_filter, output_file = self.config.input_files, self.config.input_filter, self.config.compressed_jsonl

        # Input Validation
        if is_none_or_empty(markdown_files) and is_none_or_empty(markdown_file_filter):
            print("At least one of markdown-files or markdown-file-filter is required to be specified")
            exit(1)

        # Get Markdown Files to Process
        markdown_files = MarkdownToJsonl.get_markdown_files(markdown_files, markdown_file_filter)

        # Extract Entries from specified Markdown files
        start = time.time()
        current_entries = MarkdownToJsonl.convert_markdown_entries_to_maps(*MarkdownToJsonl.extract_markdown_entries(markdown_files))
        end = time.time()
        logger.debug(f"Parse entries from Markdown files into dictionaries: {end - start} seconds")

        # Identify, mark and merge any new entries with previous entries
        start = time.time()
        if not previous_entries:
            entries_with_ids = list(enumerate(current_entries))
        else:
            entries_with_ids = self.mark_entries_for_update(current_entries, previous_entries, key='compiled', logger=logger)
        end = time.time()
        logger.debug(f"Identify new or updated entries: {end - start} seconds")

        # Process Each Entry from All Notes Files
        start = time.time()
        entries = list(map(lambda entry: entry[1], entries_with_ids))
        jsonl_data = MarkdownToJsonl.convert_markdown_maps_to_jsonl(entries)

        # Compress JSONL formatted Data
        if output_file.suffix == ".gz":
            compress_jsonl_data(jsonl_data, output_file)
        elif output_file.suffix == ".jsonl":
            dump_jsonl(jsonl_data, output_file)
        end = time.time()
        logger.debug(f"Write markdown entries to JSONL file: {end - start} seconds")

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
                for filtered_file in glob.glob(get_absolute_path(markdown_file_filter))
            }

        all_markdown_files = sorted(absolute_markdown_files | filtered_markdown_files)

        files_with_non_markdown_extensions = {
            md_file
            for md_file
            in all_markdown_files
            if not md_file.endswith(".md") and not md_file.endswith('.markdown')
        }

        if any(files_with_non_markdown_extensions):
            logger.warn(f"[Warning] There maybe non markdown-mode files in the input set: {files_with_non_markdown_extensions}")

        logger.info(f'Processing files: {all_markdown_files}')

        return all_markdown_files

    @staticmethod
    def extract_markdown_entries(markdown_files):
        "Extract entries by heading from specified Markdown files"

        # Regex to extract Markdown Entries by Heading
        markdown_heading_regex = r'^#'

        entries = []
        entry_to_file_map = []
        for markdown_file in markdown_files:
            with open(markdown_file) as f:
                markdown_content = f.read()
                markdown_entries_per_file = [f'#{entry.strip(empty_escape_sequences)}'
                for entry
                in re.split(markdown_heading_regex, markdown_content, flags=re.MULTILINE)
                if entry.strip(empty_escape_sequences) != '']
                entry_to_file_map += zip(markdown_entries_per_file, [markdown_file]*len(markdown_entries_per_file))
                entries.extend(markdown_entries_per_file)

        return entries, dict(entry_to_file_map)

    @staticmethod
    def convert_markdown_entries_to_maps(parsed_entries: list[str], entry_to_file_map) -> list[Entry]:
        "Convert each Markdown entries into a dictionary"
        entries = []
        for parsed_entry in parsed_entries:
            entries.append(Entry(compiled=parsed_entry, raw=parsed_entry, file=f'{entry_to_file_map[parsed_entry]}'))

        logger.info(f"Converted {len(parsed_entries)} markdown entries to dictionaries")

        return entries

    @staticmethod
    def convert_markdown_maps_to_jsonl(entries: list[Entry]):
        "Convert each Markdown entry to JSON and collate as JSONL"
        return ''.join([f'{entry.to_json()}\n' for entry in entries])
