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


class JsonlToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries=[]):
        # Extract required fields from config
        input_jsonl_files, input_jsonl_filter, output_file = (
            self.config.input_files,
            self.config.input_filter,
            self.config.compressed_jsonl,
        )

        # Get Jsonl Input Files to Process
        all_input_jsonl_files = JsonlToJsonl.get_jsonl_files(input_jsonl_files, input_jsonl_filter)

        # Extract Entries from specified jsonl files
        with timer("Parse entries from jsonl files", logger):
            input_jsons = JsonlToJsonl.extract_jsonl_entries(all_input_jsonl_files)
            current_entries = list(map(Entry.from_dict, input_jsons))

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
            jsonl_data = JsonlToJsonl.convert_entries_to_jsonl(entries)

            # Compress JSONL formatted Data
            compress_jsonl_data(jsonl_data, output_file)

        return entries_with_ids

    @staticmethod
    def get_jsonl_files(jsonl_files=None, jsonl_file_filters=None):
        "Get all jsonl files to process"
        absolute_jsonl_files, filtered_jsonl_files = set(), set()
        if jsonl_files:
            absolute_jsonl_files = {get_absolute_path(jsonl_file) for jsonl_file in jsonl_files}
        if jsonl_file_filters:
            filtered_jsonl_files = {
                filtered_file
                for jsonl_file_filter in jsonl_file_filters
                for filtered_file in glob.glob(get_absolute_path(jsonl_file_filter), recursive=True)
            }

        all_jsonl_files = sorted(absolute_jsonl_files | filtered_jsonl_files)

        files_with_non_jsonl_extensions = {
            jsonl_file for jsonl_file in all_jsonl_files if not jsonl_file.endswith(".jsonl")
        }
        if any(files_with_non_jsonl_extensions):
            print(f"[Warning] There maybe non jsonl files in the input set: {files_with_non_jsonl_extensions}")

        logger.debug(f"Processing files: {all_jsonl_files}")

        return all_jsonl_files

    @staticmethod
    def extract_jsonl_entries(jsonl_files):
        "Extract entries from specified jsonl files"
        entries = []
        for jsonl_file in jsonl_files:
            entries.extend(load_jsonl(Path(jsonl_file)))
        return entries

    @staticmethod
    def convert_entries_to_jsonl(entries: List[Entry]):
        "Convert each entry to JSON and collate as JSONL"
        return "".join([f"{entry.to_json()}\n" for entry in entries])
