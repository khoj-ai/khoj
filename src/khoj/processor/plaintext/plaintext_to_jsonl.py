# Standard Packages
import glob
import logging
from pathlib import Path
from typing import List

# Internal Packages
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.helpers import get_absolute_path, timer
from khoj.utils.jsonl import load_jsonl, compress_jsonl_data
from khoj.utils.rawconfig import Entry


logger = logging.getLogger(__name__)


class PlaintextToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries=[]):
        # Extract required fields from config
        input_files, input_filter, output_file = (
            self.config.input_files,
            self.config.input_filter,
            self.config.compressed_jsonl,
        )

        # Get Plaintext Input Files to Process
        all_input_plaintext_files = PlaintextToJsonl.get_plaintext_files(input_files, input_filter)

        # Extract Entries from specified plaintext files
        with timer("Parse entries from plaintext files", logger):
            current_entries = PlaintextToJsonl.convert_plaintext_entries_to_maps(
                PlaintextToJsonl.extract_plaintext_entries(all_input_plaintext_files)
            )

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            entries_with_ids = TextToJsonl.mark_entries_for_update(
                current_entries, previous_entries, key="compiled", logger=logger
            )

        with timer("Write entries to JSONL file", logger):
            # Process Each Entry from All Notes Files
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            plaintext_data = PlaintextToJsonl.convert_entries_to_jsonl(entries)

            # Compress JSONL formatted Data
            compress_jsonl_data(plaintext_data, output_file)

        return entries_with_ids

    @staticmethod
    def get_plaintext_files(plaintext_files=None, plaintext_file_filters=None):
        "Get all files to process"
        absolute_plaintext_files, filtered_plaintext_files = set(), set()
        if plaintext_files:
            absolute_plaintext_files = {get_absolute_path(jsonl_file) for jsonl_file in plaintext_files}
        if plaintext_file_filters:
            filtered_plaintext_files = {
                filtered_file
                for jsonl_file_filter in plaintext_file_filters
                for filtered_file in glob.glob(get_absolute_path(jsonl_file_filter), recursive=True)
            }

        all_target_files = sorted(absolute_plaintext_files | filtered_plaintext_files)

        files_with_no_plaintext_extensions = {
            target_files for target_files in all_target_files if not PlaintextToJsonl.is_plaintextfile(target_files)
        }
        if any(files_with_no_plaintext_extensions):
            logger.warn(f"Skipping unsupported files from plaintext indexing: {files_with_no_plaintext_extensions}")
            all_target_files = list(set(all_target_files) - files_with_no_plaintext_extensions)

        logger.debug(f"Processing files: {all_target_files}")

        return all_target_files

    @staticmethod
    def is_plaintextfile(file: str):
        "Check if file is plaintext file"
        return file.endswith(("txt", "md", "markdown", "org", "mbox", "rst", "html", "htm", "xml"))

    @staticmethod
    def extract_plaintext_entries(plaintext_files: List[str]):
        "Extract entries from specified plaintext files"
        entry_to_file_map = []

        for plaintext_file in plaintext_files:
            with open(plaintext_file, "r") as f:
                try:
                    plaintext_content = f.read()
                    entry_to_file_map.append((plaintext_content, plaintext_file))
                except Exception as e:
                    logger.warning(f"Unable to process file: {plaintext_file}. This file will not be indexed.")
                    logger.warning(e, exc_info=True)

        return dict(entry_to_file_map)

    @staticmethod
    def convert_plaintext_entries_to_maps(entry_to_file_map: dict) -> List[Entry]:
        "Convert each plaintext entries into a dictionary"
        entries = []
        for entry, file in entry_to_file_map.items():
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
