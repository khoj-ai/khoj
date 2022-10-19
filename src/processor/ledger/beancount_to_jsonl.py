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


class BeancountToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries=None):
        # Extract required fields from config
        beancount_files, beancount_file_filter, output_file = self.config.input_files, self.config.input_filter,self.config.compressed_jsonl

        # Input Validation
        if is_none_or_empty(beancount_files) and is_none_or_empty(beancount_file_filter):
            print("At least one of beancount-files or beancount-file-filter is required to be specified")
            exit(1)

        # Get Beancount Files to Process
        beancount_files = BeancountToJsonl.get_beancount_files(beancount_files, beancount_file_filter)

        # Extract Entries from specified Beancount files
        start = time.time()
        current_entries = BeancountToJsonl.convert_transactions_to_maps(*BeancountToJsonl.extract_beancount_transactions(beancount_files))
        end = time.time()
        logger.debug(f"Parse transactions from Beancount files into dictionaries: {end - start} seconds")

        # Identify, mark and merge any new entries with previous entries
        start = time.time()
        if not previous_entries:
            entries_with_ids = list(enumerate(current_entries))
        else:
            entries_with_ids = self.mark_entries_for_update(current_entries, previous_entries, key='compiled', logger=logger)
        end = time.time()
        logger.debug(f"Identify new or updated transaction: {end - start} seconds")

        # Process Each Entry from All Notes Files
        start = time.time()
        entries = list(map(lambda entry: entry[1], entries_with_ids))
        jsonl_data = BeancountToJsonl.convert_transaction_maps_to_jsonl(entries)

        # Compress JSONL formatted Data
        if output_file.suffix == ".gz":
            compress_jsonl_data(jsonl_data, output_file)
        elif output_file.suffix == ".jsonl":
            dump_jsonl(jsonl_data, output_file)
        end = time.time()
        logger.debug(f"Write transactions to JSONL file: {end - start} seconds")

        return entries_with_ids

    @staticmethod
    def get_beancount_files(beancount_files=None, beancount_file_filters=None):
        "Get Beancount files to process"
        absolute_beancount_files, filtered_beancount_files = set(), set()
        if beancount_files:
            absolute_beancount_files = {get_absolute_path(beancount_file)
                                for beancount_file
                                in beancount_files}
        if beancount_file_filters:
            filtered_beancount_files = {
                filtered_file
                for beancount_file_filter in beancount_file_filters
                for filtered_file in glob.glob(get_absolute_path(beancount_file_filter))
            }

        all_beancount_files = sorted(absolute_beancount_files | filtered_beancount_files)

        files_with_non_beancount_extensions = {
            beancount_file
            for beancount_file
            in all_beancount_files
            if not beancount_file.endswith(".bean") and not beancount_file.endswith(".beancount")
        }
        if any(files_with_non_beancount_extensions):
            print(f"[Warning] There maybe non beancount files in the input set: {files_with_non_beancount_extensions}")

        logger.info(f'Processing files: {all_beancount_files}')

        return all_beancount_files

    @staticmethod
    def extract_beancount_transactions(beancount_files):
        "Extract entries from specified Beancount files"

        # Initialize Regex for extracting Beancount Entries
        transaction_regex = r'^\n?\d{4}-\d{2}-\d{2} [\*|\!] '
        empty_newline = f'^[\n\r\t\ ]*$'

        entries = []
        transaction_to_file_map = []
        for beancount_file in beancount_files:
            with open(beancount_file) as f:
                ledger_content = f.read()
                transactions_per_file = [entry.strip(empty_escape_sequences)
                for entry
                in re.split(empty_newline, ledger_content, flags=re.MULTILINE)
                if re.match(transaction_regex, entry)]
                transaction_to_file_map += zip(transactions_per_file, [beancount_file]*len(transactions_per_file))
                entries.extend(transactions_per_file)
        return entries, dict(transaction_to_file_map)

    @staticmethod
    def convert_transactions_to_maps(parsed_entries: list[str], transaction_to_file_map) -> list[Entry]:
        "Convert each parsed Beancount transaction into a Entry"
        entries = []
        for parsed_entry in parsed_entries:
            entries.append(Entry(compiled=parsed_entry, raw=parsed_entry, file=f'{transaction_to_file_map[parsed_entry]}'))

        logger.info(f"Converted {len(parsed_entries)} transactions to dictionaries")

        return entries

    @staticmethod
    def convert_transaction_maps_to_jsonl(entries: list[Entry]) -> str:
        "Convert each Beancount transaction entry to JSON and collate as JSONL"
        return ''.join([f'{entry.to_json()}\n' for entry in entries])
