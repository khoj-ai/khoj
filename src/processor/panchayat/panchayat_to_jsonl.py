#!/usr/bin/env python3

# Standard Packages
import json
import logging
import glob
import yaml

# Internal Packages
from panchayat import vdb
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.jsonl import dump_jsonl, compress_jsonl_data
from src.utils.rawconfig import TextContentConfig

logger = logging.getLogger(__name__)

def panchayat_constructor(loader, node):
    fields = loader.construct_mapping(node)
    return vdb.VDB(**fields)


class VDBEntry():
    post_id: str
    body: str
    title: str
    author: str

    def __init__(self, post_id, body, title, author):
        self.post_id = post_id
        self.body = body
        self.title = title
        self.author = author


# Define Functions
def panchayat_to_jsonl(config: TextContentConfig, previous_entries = None):

    # Input Validation
    if is_none_or_empty(config.input_files) and is_none_or_empty(config.input_filter):
        print("At least one of input-files or input-file-filter is required to be specified")
        exit(1)

    # Get Markdown Files to Process
    yaml_files = get_panchayat_files(config.input_files, config.input_filter)

    output_file = config.compressed_jsonl

    # Extract Entries from specified Markdown files
    entries = extract_panchayat_entries(yaml_files)

    # Process Each Entry from All Notes Files
    jsonl_data = convert_panchayat_entries_to_jsonl(entries)

    # Compress JSONL formatted Data
    if output_file.suffix == ".gz":
        compress_jsonl_data(jsonl_data, output_file)
    elif output_file.suffix == ".jsonl":
        dump_jsonl(jsonl_data, output_file)

    return list(enumerate(entries))


def get_panchayat_files(yaml_files=None, yaml_file_filter=None, verbose=0):
    "Get the Panchayat file to process"
    absolute_yaml_files, filtered_yaml_files = set(), set()
    if yaml_files:
        absolute_yaml_files = {get_absolute_path(yaml_file) for yaml_file in yaml_files}
    if yaml_file_filter:
        filtered_yaml_files = {
            filtered_file
            for filter in yaml_file_filter
            for filtered_file in glob.glob(get_absolute_path(filter))
        }

    all_yaml_files = sorted(absolute_yaml_files | filtered_yaml_files)

    files_with_non_yaml_extensions = {
        yaml_file
        for yaml_file
        in all_yaml_files
        if not yaml_file.endswith(".yaml") and not yaml_file.endswith(".yml")
    }

    if any(files_with_non_yaml_extensions):
        logger.warn(f"[Warning] There maybe non markdown-mode files in the input set: {files_with_non_yaml_extensions}")

    if verbose > 0:
        print(f'Processing files: {all_yaml_files}')

    return all_yaml_files


def extract_panchayat_entries(yaml_files):
    "Extract entries by post from specified Yaml files"

    entries = []
    for yaml_file in yaml_files:
        with open(yaml_file) as f:

            raw_data = yaml.load(f, Loader=yaml.UnsafeLoader)

            seen_ids = set()

            for post in raw_data.posts.zig_zag():
                all_subposts = post.descendants_and_i
                for subpost in all_subposts:
                    if subpost.post_id not in seen_ids:
                        seen_ids.add(subpost.post_id)
                        entry = dict()
                        
                        entry['compiled'] = f"""body: {subpost.body}
                            author: {subpost.author.username}
                            title: {subpost.title}
                            created: {subpost.created}
                            upvotes: {len(subpost.upvotes)}"""
                        
                        entry['raw'] = subpost.post_id
                        entries.append(entry)

    return entries


def convert_panchayat_entries_to_jsonl(entries, verbose=0):
    "Convert each Panchayat Yaml entry to JSON and collate as JSONL"
    # jsonl = ''
    # for entry in entries:
    #     entry_dict = {'compiled': f'body: {entry["body"]} author: {entry["author"]} title: {entry["title"]}', 'raw': entry["post_id"]}
    #     # Convert Dictionary to JSON and Append to JSONL string
    #     jsonl += f'{json.dumps(entry_dict, ensure_ascii=False)}\n'


    # if verbose > 0:
    #     logger.info(f"Converted {len(entries)} to jsonl format")

    # return jsonl

    return ''.join([f'{json.dumps(entry_dict, ensure_ascii=False)}\n' for entry_dict in entries])

