#!/usr/bin/env python3

# Standard Packages
import json
import glob
import logging
import time
from typing import Iterable

# Internal Packages
from src.processor.org_mode import orgnode
from src.utils.helpers import get_absolute_path, is_none_or_empty, mark_entries_for_update
from src.utils.jsonl import dump_jsonl, compress_jsonl_data
from src.utils import state
from src.utils.rawconfig import TextContentConfig


logger = logging.getLogger(__name__)


# Define Functions
def org_to_jsonl(config: TextContentConfig, previous_entries=None):
    # Extract required fields from config
    org_files, org_file_filter, output_file = config.input_files, config.input_filter, config.compressed_jsonl
    index_heading_entries = config.index_heading_entries

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
    current_entries = convert_org_nodes_to_entries(entry_nodes, file_to_entries, index_heading_entries)
    end = time.time()
    logger.debug(f"Convert OrgNodes into entry dictionaries: {end - start} seconds")

    # Identify, mark and merge any new entries with previous entries
    if not previous_entries:
        entries_with_ids = list(enumerate(current_entries))
    else:
        entries_with_ids = mark_entries_for_update(current_entries, previous_entries, key='compiled', logger=logger)

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


def get_org_files(org_files=None, org_file_filters=None):
    "Get Org files to process"
    absolute_org_files, filtered_org_files = set(), set()
    if org_files:
        absolute_org_files = {
            get_absolute_path(org_file)
            for org_file
            in org_files
        }
    if org_file_filters:
        filtered_org_files = {
            filtered_file
            for org_file_filter in org_file_filters
            for filtered_file in glob.glob(get_absolute_path(org_file_filter))
        }

    all_org_files = sorted(absolute_org_files | filtered_org_files)

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


def convert_org_nodes_to_entries(entries: list[orgnode.Orgnode], entry_to_file_map, index_heading_entries=False) -> list[dict]:
    "Convert Org-Mode entries into list of dictionary"
    entry_maps = []
    for entry in entries:
        entry_dict = dict()

        if not entry.hasBody and not index_heading_entries:
            # Ignore title notes i.e notes with just headings and empty body
            continue

        entry_dict["compiled"] = f'{entry.heading}.'
        if state.verbose > 2:
            logger.debug(f"Title: {entry.heading}")

        if entry.tags:
            tags_str = " ".join(entry.tags)
            entry_dict["compiled"] += f'\t {tags_str}.'
            if state.verbose > 2:
                logger.debug(f"Tags: {tags_str}")

        if entry.closed:
            entry_dict["compiled"] += f'\n Closed on {entry.closed.strftime("%Y-%m-%d")}.'
            if state.verbose > 2:
                logger.debug(f'Closed: {entry.closed.strftime("%Y-%m-%d")}')

        if entry.scheduled:
            entry_dict["compiled"] += f'\n Scheduled for {entry.scheduled.strftime("%Y-%m-%d")}.'
            if state.verbose > 2:
                logger.debug(f'Scheduled: {entry.scheduled.strftime("%Y-%m-%d")}')

        if entry.hasBody:
            entry_dict["compiled"] += f'\n {entry.body}'
            if state.verbose > 2:
                logger.debug(f"Body: {entry.body}")

        if entry_dict:
            entry_dict["raw"] = f'{entry}'
            entry_dict["file"] = f'{entry_to_file_map[entry]}'

            # Convert Dictionary to JSON and Append to JSONL string
            entry_maps.append(entry_dict)

    return entry_maps


def convert_org_entries_to_jsonl(entries: Iterable[dict]) -> str:
    "Convert each Org-Mode entry to JSON and collate as JSONL"
    return ''.join([f'{json.dumps(entry_dict, ensure_ascii=False)}\n' for entry_dict in entries])
