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


def get_org_files(org_directory, org_files=None, org_file_filter="*.org"):
    "Get Org files to process"
    expanded_org_directory = org_directory.expanduser()
    filtered_org_files = {org_file
                 for org_file
                 in expanded_org_directory.glob(org_file_filter)
                 if not org_file.name.startswith('.')}

    # Filter to User specified Org files when set by User
    if org_files:
        filtered_org_files = {str(org_file)
                     for org_file in filtered_org_files
                     if str(org_file.relative_to(expanded_org_directory)) in set(org_files)}

    return filtered_org_files


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


if __name__ == '__main__':
    # Setup argument parser
    parser = argparse.ArgumentParser(description="Map Org-Mode notes into JSONL format")
    parser.add_argument('--jsonl-file', type=pathlib.Path, required=True, help="Output file for JSONL formatted notes")
    parser.add_argument('--org-directory', default=pathlib.Path("./"), type=pathlib.Path, help="Input directory from which to retrieve Org-Mode files to convert. Default: Current directory.")
    parser.add_argument('--org-files', '-f', nargs='+', help="List of org mode files to process. Requires file path relative to org_directory")
    parser.add_argument('--org-file-filter', type=str, default="*.org", help="Regex filter org files in org_directory to process. Default: All org files in org_directory")
    parser.add_argument('--compress', action='store_true', default=False, help="Compress output to gunzipped jsonl file")
    parser.add_argument('--verbose', '-v', action='count', help="Show verbose conversion logs")
    args = parser.parse_args()

    # Get Org Files to Process
    org_files = get_org_files(args.org_directory, args.org_files, args.org_file_filter)

    # Extract Entries from specified Org files
    entries = extract_org_entries(org_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_org_entries_to_jsonl(entries, str(args.jsonl_file), verbose=args.verbose)

    # Compress JSONL formatted Data
    if args.compress:
        compress_jsonl_data(jsonl_data, str(args.jsonl_file.expanduser()), verbose=args.verbose)
    else:
        dump_jsonl(jsonl_data, str(args.jsonl_file.expanduser()), verbose=args.verbose)
