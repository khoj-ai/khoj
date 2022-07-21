#!/usr/bin/env python3

# Standard Packages
import json
import argparse
import pathlib
import glob
import re

# Internal Packages
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.constants import empty_escape_sequences
from src.utils.jsonl import dump_jsonl, compress_jsonl_data


# Define Functions
def markdown_to_jsonl(markdown_files, markdown_file_filter, output_file, verbose=0):
    # Input Validation
    if is_none_or_empty(markdown_files) and is_none_or_empty(markdown_file_filter):
        print("At least one of markdown-files or markdown-file-filter is required to be specified")
        exit(1)

    # Get Markdown Files to Process
    markdown_files = get_markdown_files(markdown_files, markdown_file_filter, verbose)

    # Extract Entries from specified Markdown files
    entries = extract_markdown_entries(markdown_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_markdown_entries_to_jsonl(entries, verbose=verbose)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file, verbose=verbose)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file, verbose=verbose)

    return entries


def get_markdown_files(markdown_files=None, markdown_file_filter=None, verbose=0):
    "Get Markdown files to process"
    absolute_markdown_files, filtered_markdown_files = set(), set()
    if markdown_files:
        absolute_markdown_files = {get_absolute_path(markdown_file) for markdown_file in markdown_files}
    if markdown_file_filter:
        filtered_markdown_files = set(glob.glob(get_absolute_path(markdown_file_filter)))

    all_markdown_files = absolute_markdown_files | filtered_markdown_files

    files_with_non_markdown_extensions = {
        md_file
        for md_file
        in all_markdown_files
        if not md_file.endswith(".md") and not md_file.endswith('.markdown')
    }

    if any(files_with_non_markdown_extensions):
        print(f"[Warning] There maybe non markdown-mode files in the input set: {files_with_non_markdown_extensions}")

    if verbose > 0:
        print(f'Processing files: {all_markdown_files}')

    return all_markdown_files


def extract_markdown_entries(markdown_files):
    "Extract entries by heading from specified Markdown files"

    # Regex to extract Markdown Entries by Heading
    markdown_heading_regex = r'^#'

    entries = []
    for markdown_file in markdown_files:
        with open(markdown_file) as f:
            markdown_content = f.read()
            entries.extend([f'#{entry.strip(empty_escape_sequences)}'
               for entry
               in re.split(markdown_heading_regex, markdown_content, flags=re.MULTILINE)])

    return entries


def convert_markdown_entries_to_jsonl(entries, verbose=0):
    "Convert each Markdown entries to JSON and collate as JSONL"
    jsonl = ''
    for entry in entries:
        entry_dict = {'compiled': entry, 'raw': entry}
        # Convert Dictionary to JSON and Append to JSONL string
        jsonl += f'{json.dumps(entry_dict, ensure_ascii=False)}\n'

    if verbose > 0:
        print(f"Converted {len(entries)} to jsonl format")

    return jsonl


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Map Markdown entries into (compressed) JSONL format")
    parser.add_argument('--output-file', '-o', type=pathlib.Path, required=True, help="Output file for (compressed) JSONL formatted notes. Expected file extensions: jsonl or jsonl.gz")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of markdown files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for markdown files to process")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs, Default: 0")
    args = parser.parse_args()

    # Map notes in Markdown files to (compressed) JSONL formatted file
    markdown_to_jsonl(args.input_files, args.input_filter, args.output_file, args.verbose)
