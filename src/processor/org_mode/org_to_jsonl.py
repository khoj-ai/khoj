#!/usr/bin/env python3

# Standard Packages
import re
import json
import argparse
import pathlib
import glob
import logging
import hashlib
import time

# Internal Packages
from src.processor.org_mode import orgnode
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.jsonl import dump_jsonl, compress_jsonl_data
from src.utils import state


logger = logging.getLogger(__name__)


# Define Functions
def org_to_jsonl(org_files, org_file_filter, output_file, previous_entries=None):
    # Input Validation
    if is_none_or_empty(org_files) and is_none_or_empty(org_file_filter):
        print("At least one of org-files or org-file-filter is required to be specified")
        exit(1)

    # Get Org Files to Process
    start = time.time()
    org_files = get_org_files(org_files, org_file_filter)

    # Extract Entries from specified Org files
    start = time.time()
    entry_nodes, file_to_entries = extract_org_entries(org_files)
    end = time.time()
    logger.debug(f"Parse entries from org files into OrgNode objects: {end - start} seconds")

    start = time.time()
    current_entries = convert_org_nodes_to_entries(entry_nodes, file_to_entries)
    end = time.time()
    logger.debug(f"Convert OrgNodes into entry dictionaries: {end - start} seconds")

    # Identify, mark and merge any new entries with previous entries
    if not previous_entries:
        entries_with_ids = list(enumerate(current_entries))
    else:
        # Hash all current and previous entries to identify new entries
        start = time.time()
        current_entry_hashes = list(map(lambda e: hashlib.md5(bytes(e['compiled'], encoding='utf-8')).hexdigest(), current_entries))
        previous_entry_hashes = list(map(lambda e: hashlib.md5(bytes(e['compiled'], encoding='utf-8')).hexdigest(), previous_entries))
        end = time.time()
        logger.debug(f"Hash previous, current entries: {end - start} seconds")

        start = time.time()
        hash_to_current_entries = dict(zip(current_entry_hashes, current_entries))
        hash_to_previous_entries = dict(zip(previous_entry_hashes, previous_entries))

        # All entries that did not exist in the previous set are to be added
        new_entry_hashes = set(current_entry_hashes) - set(previous_entry_hashes)
        # All entries that exist in both current and previous sets are kept
        existing_entry_hashes = set(current_entry_hashes) & set(previous_entry_hashes)

        # Mark new entries with no ids for later embeddings generation
        new_entries = [
            (None, hash_to_current_entries[entry_hash])
            for entry_hash in new_entry_hashes
        ]
        # Set id of existing entries to their previous ids to reuse their existing encoded embeddings
        existing_entries = [
            (previous_entry_hashes.index(entry_hash), hash_to_previous_entries[entry_hash])
            for entry_hash in existing_entry_hashes
        ]

        existing_entries_sorted = sorted(existing_entries, key=lambda e: e[0])
        entries_with_ids = existing_entries_sorted + new_entries
        end = time.time()
        logger.debug(f"Identify, Mark, Combine new, existing entries: {end - start} seconds")

    # Process Each Entry from All Notes Files
    start = time.time()
    entries = map(lambda entry: entry[1], entries_with_ids)
    jsonl_data = convert_org_entries_to_jsonl(entries)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file)
    end = time.time()
    logger.debug(f"Write org entries to JSONL file: {end - start} seconds")

    return entries_with_ids


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

    files_with_non_org_extensions = {org_file for org_file in all_org_files if not org_file.endswith(".org")}
    if any(files_with_non_org_extensions):
        logger.warn(f"There maybe non org-mode files in the input set: {files_with_non_org_extensions}")

    logger.info(f'Processing files: {all_org_files}')

    return all_org_files


def extract_org_entries(org_files):
    "Extract entries from specified Org files"
    entries = []
    entry_to_file_map = []
    for org_file in org_files:
        org_file_entries = orgnode.makelist(str(org_file))
        entry_to_file_map += zip(org_file_entries, [org_file]*len(org_file_entries))
        entries.extend(org_file_entries)

    return entries, dict(entry_to_file_map)


def convert_org_nodes_to_entries(entries: list[orgnode.Orgnode], entry_to_file_map) -> list[dict]:
    "Convert Org-Mode entries into list of dictionary"
    entry_maps = []
    for entry in entries:
        entry_dict = dict()

        # Ignore title notes i.e notes with just headings and empty body
        if not entry.Body() or re.sub(r'\n|\t|\r| ', '', entry.Body()) == "":
            continue

        entry_dict["compiled"] = f'{entry.Heading()}.'
        if state.verbose > 2:
            logger.debug(f"Title: {entry.Heading()}")

        if entry.Tags():
            tags_str = " ".join(entry.Tags())
            entry_dict["compiled"] += f'\t {tags_str}.'
            if state.verbose > 2:
                logger.debug(f"Tags: {tags_str}")

        if entry.Closed():
            entry_dict["compiled"] += f'\n Closed on {entry.Closed().strftime("%Y-%m-%d")}.'
            if state.verbose > 2:
                logger.debug(f'Closed: {entry.Closed().strftime("%Y-%m-%d")}')

        if entry.Scheduled():
            entry_dict["compiled"] += f'\n Scheduled for {entry.Scheduled().strftime("%Y-%m-%d")}.'
            if state.verbose > 2:
                logger.debug(f'Scheduled: {entry.Scheduled().strftime("%Y-%m-%d")}')

        if entry.Body():
            entry_dict["compiled"] += f'\n {entry.Body()}'
            if state.verbose > 2:
                logger.debug(f"Body: {entry.Body()}")

        if entry_dict:
            entry_dict["raw"] = f'{entry}'
            entry_dict["file"] = f'{entry_to_file_map[entry]}'

            # Convert Dictionary to JSON and Append to JSONL string
            entry_maps.append(entry_dict)

    return entry_maps


def convert_org_entries_to_jsonl(entries) -> str:
    "Convert each Org-Mode entry to JSON and collate as JSONL"
    return ''.join([f'{json.dumps(entry_dict, ensure_ascii=False)}\n' for entry_dict in entries])


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
