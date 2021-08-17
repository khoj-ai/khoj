#!/usr/bin/env python3

# Import Modules
from processor.org_mode import orgnode
from utils.helpers import get_absolute_path, is_none_or_empty
import json
import argparse
import pathlib
import glob
import gzip


# Define Functions
def org_to_jsonl(org_files, org_file_filter, output_file, verbose=0):
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

def dump_jsonl(jsonl_data, output_path, verbose=0):
    "Write List of JSON objects to JSON line file"
    with open(get_absolute_path(output_path), 'w', encoding='utf-8') as f:
        f.write(jsonl_data)

    if verbose > 0:
        print(f'Wrote {len(jsonl_data)} lines to jsonl at {output_path}')


def compress_jsonl_data(jsonl_data, output_path, verbose=0):
    with gzip.open(get_absolute_path(output_path), 'wt') as gzip_file:
        gzip_file.write(jsonl_data)

    if verbose > 0:
        print(f'Wrote {len(jsonl_data)} lines to gzip compressed jsonl at {output_path}')


def load_jsonl(input_path, verbose=0):
    "Read List of JSON objects from JSON line file"
    data = []
    with open(get_absolute_path(input_path), 'r', encoding='utf-8') as f:
        for line in f:
            data.append(json.loads(line.rstrip('\n|\r')))

    if verbose > 0:
        print(f'Loaded {len(data)} records from {input_path}')

    return data


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

        entry_dict["Title"] = entry.Heading()
        if verbose > 1:
            print(f"Title: {entry.Heading()}")

        if entry.Tags():
            tags_str = " ".join([tag for tag in entry.Tags()])
            entry_dict["Tags"] = tags_str
            if verbose > 1:
                print(f"Tags: {tags_str}")

        if entry.Body():
            entry_dict["Body"] = entry.Body()
            if verbose > 2:
                print(f"Body: {entry.Body()}")

        if entry_dict:
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
    parser.add_argument('--verbose', '-v', action='count', help="Show verbose conversion logs")
    args = parser.parse_args()

    # Input Validation
    if is_none_or_empty(args.input_files) and is_none_or_empty(args.input_filter):
        print("At least one of org-files or org-file-filter is required to be specified")
        exit(1)

    # Map notes in Org-Mode files to (compressed) JSONL formatted file
    org_to_jsonl(args.input_files, args.input_filter, args.output_file, args.verbose)
