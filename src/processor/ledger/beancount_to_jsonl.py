#!/usr/bin/env python3

# Standard Packages
import json
import glob
import re
import logging
import time

# Internal Packages
from src.utils.helpers import get_absolute_path, is_none_or_empty, mark_entries_for_update
from src.utils.constants import empty_escape_sequences
from src.utils.jsonl import dump_jsonl, compress_jsonl_data
from src.utils.rawconfig import TextContentConfig


logger = logging.getLogger(__name__)


# Define Functions
def beancount_to_jsonl(config: TextContentConfig, previous_entries=None):
    # Extract required fields from config
    beancount_files, beancount_file_filter, output_file = config.input_files, config.input_filter, config.compressed_jsonl

    # Input Validation
    if is_none_or_empty(beancount_files) and is_none_or_empty(beancount_file_filter):
        print("At least one of beancount-files or beancount-file-filter is required to be specified")
        exit(1)

    # Get Beancount Files to Process
    beancount_files = get_beancount_files(beancount_files, beancount_file_filter)

    # Extract Entries from specified Beancount files
    start = time.time()
    current_entries = convert_transactions_to_maps(*extract_beancount_transactions(beancount_files))
    end = time.time()
    logger.debug(f"Parse transactions from Beancount files into dictionaries: {end - start} seconds")

    # Identify, mark and merge any new entries with previous entries
    start = time.time()
    if not previous_entries:
        entries_with_ids = list(enumerate(current_entries))
    else:
        entries_with_ids = mark_entries_for_update(current_entries, previous_entries, key='compiled', logger=logger)
    end = time.time()
    logger.debug(f"Identify new or updated transaction: {end - start} seconds")

    # Process Each Entry from All Notes Files
    start = time.time()
    entries = list(map(lambda entry: entry[1], entries_with_ids))
    jsonl_data = convert_transaction_maps_to_jsonl(entries)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file)
    end = time.time()
    logger.debug(f"Write transactions to JSONL file: {end - start} seconds")

    return entries_with_ids


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


def convert_transactions_to_maps(entries: list[str], transaction_to_file_map) -> list[dict]:
    "Convert each Beancount transaction into a dictionary"
    entry_maps = []
    for entry in entries:
        entry_maps.append({'compiled': entry, 'raw': entry, 'file': f'{transaction_to_file_map[entry]}'})

    logger.info(f"Converted {len(entries)} transactions to dictionaries")

    return entry_maps


def convert_transaction_maps_to_jsonl(entries: list[dict]) -> str:
    "Convert each Beancount transaction dictionary to JSON and collate as JSONL"
    return ''.join([f'{json.dumps(entry_dict, ensure_ascii=False)}\n' for entry_dict in entries])
