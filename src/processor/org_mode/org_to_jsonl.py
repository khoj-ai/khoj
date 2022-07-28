#!/usr/bin/env python3

# Standard Packages
import json
import argparse
import pathlib
import glob

# Internal Packages
from src.processor.org_mode import orgnode
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.constants import empty_escape_sequences
from src.utils.jsonl import dump_jsonl, compress_jsonl_data


# Define Functions
def org_to_jsonl(org_files, org_file_filter, output_file, verbose=0):
    # Input Validation
    if is_none_or_empty(org_files) and is_none_or_empty(org_file_filter):
        print("At least one of org-files or org-file-filter is required to be specified")
        exit(1)

    # Get Org Files to Process
    org_files = get_org_files(org_files, org_file_filter, verbose)

    # Extract Entries from specified Org files
    entries = extract_org_entries(org_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_org_entries_to_jsonl(entries, verbose=verbose)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file, verbose=verbose)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file, verbose=verbose)

    return entries


def get_org_files(org_files=None, org_file_filter=None, verbose=0):
    "Get Org files to process"
    absolute_org_files, filtered_org_files = set(), set()
    if org_files:
        absolute_org_files = {get_absolute_path(org_file)
                              for org_file
                              in org_files}
    if org_file_filter:
        filtered_org_files = set(glob.glob(get_absolute_path(org_file_filter)))

    all_org_files = absolute_org_files | filtered_org_files

    files_with_non_org_extensions = {org_file for org_file in all_org_files if not org_file.endswith(".org")}
    if any(files_with_non_org_extensions):
        print(f"[Warning] There maybe non org-mode files in the input set: {files_with_non_org_extensions}")

    if verbose > 0:
        print(f'Processing files: {all_org_files}')

    return all_org_files


def extract_org_entries(org_files):
    "Extract entries from specified Org files"
    entries = []
    for org_file in org_files:
        entries.extend(
            orgnode.makelist(
                str(org_file)))

    return entries


def convert_org_entries_to_jsonl(entries, verbose=0):
    "Convert each Org-Mode entries to JSON and collate as JSONL"
    jsonl = ''
    for entry in entries:
        entry_dict = dict()

        # Ignore title notes i.e notes with just headings and empty body
        if not entry.Body() or entry.Body().strip(empty_escape_sequences) == "":
            continue

        entry_dict["compiled"] = f'{entry.Heading()}.'
        if verbose > 2:
            print(f"Title: {entry.Heading()}")

        if entry.Tags():
            tags_str = " ".join(entry.Tags())
            entry_dict["compiled"] += f'\t {tags_str}.'
            if verbose > 2:
                print(f"Tags: {tags_str}")

        if entry.Closed():
            entry_dict["compiled"] += f'\n Closed on {entry.Closed().strftime("%Y-%m-%d")}.'
            if verbose > 2:
                print(f'Closed: {entry.Closed().strftime("%Y-%m-%d")}')

        if entry.Scheduled():
            entry_dict["compiled"] += f'\n Scheduled for {entry.Scheduled().strftime("%Y-%m-%d")}.'
            if verbose > 2:
                print(f'Scheduled: {entry.Scheduled().strftime("%Y-%m-%d")}')

        if entry.Body():
            entry_dict["compiled"] += f'\n {entry.Body()}'
            if verbose > 2:
                print(f"Body: {entry.Body()}")

        if entry_dict:
            entry_dict["raw"] = f'{entry}'

            # Convert Dictionary to JSON and Append to JSONL string
            jsonl += f'{json.dumps(entry_dict, ensure_ascii=False)}\n'

    if verbose > 0:
        print(f"Converted {len(entries)} to jsonl format")

    return jsonl


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Map Org-Mode notes into (compressed) JSONL format")
    parser.add_argument('--output-file', '-o', type=pathlib.Path, required=True, help="Output file for (compressed) JSONL formatted notes. Expected file extensions: jsonl or jsonl.gz")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of org-mode files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for org-mode files to process")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs, Default: 0")
    args = parser.parse_args()

    # Map notes in Org-Mode files to (compressed) JSONL formatted file
    org_to_jsonl(args.input_files, args.input_filter, args.output_file, args.verbose)
