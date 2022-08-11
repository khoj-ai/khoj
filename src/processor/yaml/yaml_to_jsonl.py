#!/usr/bin/env python3

# Standard Packages
import json
import argparse
import pathlib
import glob
import re
import yaml

# Internal Packages
from panchayat import vdb
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.constants import empty_escape_sequences
from src.utils.jsonl import dump_jsonl, compress_jsonl_data


def panchayat_constructor(loader, node):
    fields = loader.construct_mapping(node)
    return vdb.VDB(**fields)


# Define Functions
def yaml_to_jsonl(yaml_files, yaml_file_filter, output_file, verbose=0):

    # yaml.add_constructor("!python.vdb.VDB", panchayat_constructor)
    # Input Validation
    if is_none_or_empty(yaml_files) and is_none_or_empty(yaml_file_filter):
        print("At least one of markdown-files or markdown-file-filter is required to be specified")
        exit(1)

    # Get Markdown Files to Process
    yaml_files = get_yaml_files(yaml_files, yaml_file_filter, verbose)

    # Extract Entries from specified Markdown files
    entries = extract_yaml_entries(yaml_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_yaml_entries_to_jsonl(entries, verbose=verbose)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file, verbose=verbose)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file, verbose=verbose)

    return entries


def get_yaml_files(yaml_files=None, yaml_file_filter=None, verbose=0):
    "Get Yaml files to process"
    absolute_yaml_files, filtered_yaml_files = set(), set()
    if yaml_files:
        absolute_yaml_files = {get_absolute_path(yaml_file) for yaml_file in yaml_files}
    if yaml_file_filter:
        filtered_yaml_files = set(glob.glob(get_absolute_path(yaml_file_filter)))

    all_yaml_files = absolute_yaml_files | filtered_yaml_files

    files_with_non_yaml_extensions = {
        yaml_file
        for yaml_file
        in all_yaml_files
        if not yaml_file.endswith(".yaml") and not yaml_file.endswith(".yml")
    }

    if any(files_with_non_yaml_extensions):
        print(f"[Warning] There maybe non markdown-mode files in the input set: {files_with_non_yaml_extensions}")

    if verbose > 0:
        print(f'Processing files: {all_yaml_files}')

    return all_yaml_files


def extract_yaml_entries(yaml_files):
    "Extract entries by post from specified Yaml files"

    entries = []
    for yaml_file in yaml_files:
        with open(yaml_file) as f:

            # try:
            raw_data = yaml.load(f, Loader=yaml.UnsafeLoader)

            # print(raw_data)
            # print(raw_data.posts.zig_zag())

            seen_ids = set()

            for post in raw_data.posts.zig_zag():
                all_subposts = post.descendants_and_i
                for subpost in all_subposts:
                    if subpost.post_id not in seen_ids:
                        seen_ids.add(subpost.post_id)
                        entry = {
                            "author": subpost.author.username,
                            "title": subpost.title,
                            "body": subpost.body}
                        entries.append(entry)

            # except yaml.YAMLError as exception:
                # print(f"Exception encountered while parsing {yaml_file}: {exception}")

            # markdown_content = f.read()
            # entries.extend([f'#{entry.strip(empty_escape_sequences)}'
            #    for entry
            #    in re.split(markdown_heading_regex, markdown_content, flags=re.MULTILINE)])

    return entries


def convert_yaml_entries_to_jsonl(entries, verbose=0):
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
    parser = argparse.ArgumentParser(description="Map Yaml entries into (compressed) JSONL format")
    parser.add_argument('--output-file', '-o', type=pathlib.Path, required=True, help="Output file for (compressed) JSONL formatted notes. Expected file extensions: jsonl or jsonl.gz")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of yaml files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for yaml files to process")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs, Default: 0")
    args = parser.parse_args()

    # Map notes in Yaml files to (compressed) JSONL formatted file
    yaml_to_jsonl(args.input_files, args.input_filter, args.output_file, args.verbose)
