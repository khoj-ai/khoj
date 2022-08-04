# Standard Packages
import argparse
import pathlib

# External Packages
import yaml

# Internal Packages
from src.utils.helpers import is_none_or_empty, get_absolute_path, resolve_absolute_path, merge_dicts
from src.utils.rawconfig import FullConfig

def cli(args=None):
    # Setup Argument Parser for the Commandline Interface
    parser = argparse.ArgumentParser(description="Start Khoj; A Natural Language Search Engine for your personal Notes, Transactions and Photos")
    parser.add_argument('config_file', type=pathlib.Path, help="YAML file to configure Khoj")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate model embeddings from source files. Default: false")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs. Default: 0")
    parser.add_argument('--host', type=str, default='127.0.0.1', help="Host address of the server. Default: 127.0.0.1")
    parser.add_argument('--port', '-p', type=int, default=8000, help="Port of the server. Default: 8000")
    parser.add_argument('--socket', type=pathlib.Path, help="Path to UNIX socket for server. Use to run server behind reverse proxy. Default: /tmp/uvicorn.sock")

    args = parser.parse_args(args)

    if not resolve_absolute_path(args.config_file).exists():
        raise ValueError(f"Config file {args.config_file} does not exist")

    # Read Config from YML file
    config_from_file = None
    with open(get_absolute_path(args.config_file), 'r', encoding='utf-8') as config_file:
        config_from_file = yaml.safe_load(config_file)

    # Parse, Validate Config in YML file
    args.config = FullConfig.parse_obj(config_from_file)

    return args