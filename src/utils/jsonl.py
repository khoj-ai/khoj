# Standard Packages
import json
import gzip
import logging

# Internal Packages
from src.utils.constants import empty_escape_sequences
from src.utils.helpers import get_absolute_path


logger = logging.getLogger(__name__)


def load_jsonl(input_path):
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
    logger.info(f'Loaded {len(data)} records from {input_path}')

    return data


def dump_jsonl(jsonl_data, output_path):
    "Write List of JSON objects to JSON line file"
    # Create output directory, if it doesn't exist
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(jsonl_data)

    logger.info(f'Wrote jsonl data to {output_path}')


def compress_jsonl_data(jsonl_data, output_path):
    # Create output directory, if it doesn't exist
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with gzip.open(output_path, 'wt', encoding='utf-8') as gzip_file:
        gzip_file.write(jsonl_data)

    logger.info(f'Wrote jsonl data to gzip compressed jsonl at {output_path}')