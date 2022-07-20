# Standard Packages
import json
import gzip

# Internal Packages
from src.utils.constants import empty_escape_sequences
from src.utils.helpers import get_absolute_path


def load_jsonl(input_path, verbose=0):
    "Read List of JSON objects from JSON line file"
    # Initialize Variables
    data = []
    jsonl_file = None

    # Open JSONL file
    if input_path.suffix == ".gz":
        jsonl_file = gzip.open(get_absolute_path(input_path), 'rt', encoding='utf-8')
    elif input_path.suffix == ".jsonl":
        jsonl_file = open(get_absolute_path(input_path), 'r', encoding='utf-8')

    # Read JSONL file
    for line in jsonl_file:
        data.append(json.loads(line.strip(empty_escape_sequences)))

    # Close JSONL file
    jsonl_file.close()

    # Log JSONL entries loaded
    if verbose > 0:
        print(f'Loaded {len(data)} records from {input_path}')

    return data


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