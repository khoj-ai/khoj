#!/usr/bin/env python3

# Import Modules
import orgnode
import json
import argparse
import pathlib
import glob
import gzip


# Define Functions
def dump_jsonl(jsonl_data, output_path, verbose=0):
    "Write List of JSON objects to JSON line file"
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(jsonl_data)

    if verbose > 0:
        print(f'Wrote {len(jsonl_data)} records to jsonl at {output_path}')


def compress_jsonl_data(jsonl_data, output_path, verbose=0):
    with gzip.open(f'{output_path}.gz', 'wt') as gzip_file:
        gzip_file.write(jsonl_data)

    if verbose > 0:
        print(f'Wrote {len(jsonl_data)} records to gzip compressed jsonl at {output_path}.gz')


def load_jsonl(input_path, verbose=0):
    "Read List of JSON objects from JSON line file"
    data = []
    with open(input_path, 'r', encoding='utf-8') as f:
        for line in f:
            data.append(json.loads(line.rstrip('\n|\r')))

    if verbose > 0:
        print(f'Loaded {len(data)} records from {input_path}')

    return data


def get_org_files(org_files=None, org_file_filter=None):
    "Get Org files to process"
    absolute_org_files, filtered_org_files = set(), set()
    if org_files:
        absolute_org_files = {get_absolute_path(org_file)
                              for org_file
                              in org_files}
    if org_file_filter:
        filtered_org_files = set(glob.glob(get_absolute_path(org_file_filter)))

    all_org_files = absolute_org_files | filtered_org_files
    if args.verbose:
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


def convert_org_entries_to_jsonl(entries, jsonl_file, verbose=0):
    "Convert each org entries to json and write to jsonl file"
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

    return jsonl


def is_none_or_empty(item):
    return item == None or (hasattr(item, '__iter__') and len(item) == 0)


def get_absolute_path(filepath):
    return str(pathlib.Path(filepath).expanduser().absolute())


if __name__ == '__main__':
    # Setup argument parser
    parser = argparse.ArgumentParser(description="Map Org-Mode notes into (compressed) JSONL format")
    parser.add_argument('--jsonl-file', '-o', type=pathlib.Path, required=True, help="Output file for JSONL formatted notes")
    parser.add_argument('--org-files', '-i', nargs='*', help="List of org-mode files to process")
    parser.add_argument('--org-file-filter', type=str, default=None, help="Regex filter for org-mode files to process")
    parser.add_argument('--no-compress', action='store_true', default=False, help="Do not compress jsonl output with gunzip. Default: False")
    parser.add_argument('--verbose', '-v', action='count', help="Show verbose conversion logs")
    args = parser.parse_args()

    if is_none_or_empty(args.org_files) and is_none_or_empty(args.org_file_filter):
        print("At least one of org-files or org-file-filter is required to be specified")

    # Get Org Files to Process
    org_files = get_org_files(args.org_files, args.org_file_filter)

    # Extract Entries from specified Org files
    entries = extract_org_entries(org_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_org_entries_to_jsonl(entries, get_absolute_path(args.jsonl_file), verbose=args.verbose)

    # Compress JSONL formatted Data
    if args.no_compress:
        dump_jsonl(jsonl_data, get_absolute_path(args.jsonl_file), verbose=args.verbose)
    else:
        compress_jsonl_data(jsonl_data, get_absolute_path(args.jsonl_file), verbose=args.verbose)
