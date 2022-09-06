#!/usr/bin/env python3

# Standard Packages
import json
import argparse
import pathlib
import glob
import re
import logging

# Internal Packages
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.constants import empty_escape_sequences
from src.utils.jsonl import dump_jsonl, compress_jsonl_data


logger = logging.getLogger(__name__)


# Define Functions
def beancount_to_jsonl(beancount_files, beancount_file_filter, output_file):
    # Input Validation
    if is_none_or_empty(beancount_files) and is_none_or_empty(beancount_file_filter):
        print("At least one of beancount-files or beancount-file-filter is required to be specified")
        exit(1)

    # Get Beancount Files to Process
    beancount_files = get_beancount_files(beancount_files, beancount_file_filter)

    # Extract Entries from specified Beancount files
    entries, transaction_to_file_map = extract_beancount_entries(beancount_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_beancount_entries_to_jsonl(entries, transaction_to_file_map)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file)

    return entries


def get_beancount_files(beancount_files=None, beancount_file_filter=None):
    "Get Beancount files to process"
    absolute_beancount_files, filtered_beancount_files = set(), set()
    if beancount_files:
        absolute_beancount_files = {get_absolute_path(beancount_file)
                              for beancount_file
                              in beancount_files}
    if beancount_file_filter:
        filtered_beancount_files = set(glob.glob(get_absolute_path(beancount_file_filter)))

    all_beancount_files = absolute_beancount_files | filtered_beancount_files

    files_with_non_beancount_extensions = {beancount_file
                                    for beancount_file
                                    in all_beancount_files
                                    if not beancount_file.endswith(".bean") and not beancount_file.endswith(".beancount")}
    if any(files_with_non_beancount_extensions):
        print(f"[Warning] There maybe non beancount files in the input set: {files_with_non_beancount_extensions}")

    logger.info(f'Processing files: {all_beancount_files}')

    return all_beancount_files


def extract_beancount_entries(beancount_files):
    "Extract entries from specified Beancount files"

    # Initialize Regex for extracting Beancount Entries
    transaction_regex = r'^\n?\d{4}-\d{2}-\d{2} [\*|\!] '
    empty_newline = f'^[{empty_escape_sequences}]*$'

    entries = []
    transaction_to_file_map = []
    for beancount_file in beancount_files:
        with open(beancount_file) as f:
            ledger_content = f.read()
            transactions_per_file = [entry.strip(empty_escape_sequences)
               for entry
               in re.split(empty_newline, ledger_content, flags=re.MULTILINE)
               if re.match(transaction_regex, entry)]
            transaction_to_file_map += [beancount_file]*len(transactions_per_file)
            entries.extend(transactions_per_file)
    return entries, transaction_to_file_map


def convert_beancount_entries_to_jsonl(entries, transaction_to_file_map):
    "Convert each Beancount transaction to JSON and collate as JSONL"
    jsonl = ''
    for entry_id, entry in enumerate(entries):
        entry_dict = {'compiled': entry, 'raw': entry, 'file': f'{transaction_to_file_map[entry_id]}'}
        # Convert Dictionary to JSON and Append to JSONL string
        jsonl += f'{json.dumps(entry_dict, ensure_ascii=False)}\n'

    logger.info(f"Converted {len(entries)} to jsonl format")

    return jsonl


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Map Beancount transactions into (compressed) JSONL format")
    parser.add_argument('--output-file', '-o', type=pathlib.Path, required=True, help="Output file for (compressed) JSONL formatted transactions. Expected file extensions: jsonl or jsonl.gz")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of beancount files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for beancount files to process")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs, Default: 0")
    args = parser.parse_args()

    # Map transactions in beancount files to (compressed) JSONL formatted file
    beancount_to_jsonl(args.input_files, args.input_filter, args.output_file, args.verbose)
