# Standard Packages
import argparse
import pathlib

# External Packages
import yaml

# Internal Packages
from src.utils.helpers import is_none_or_empty, get_absolute_path, resolve_absolute_path, merge_dicts
from src.utils.rawconfig import FullConfig

def cli(args=None):
    if is_none_or_empty(args):
        return None

    # Setup Argument Parser for the Commandline Interface
    parser = argparse.ArgumentParser(description="Expose API for Khoj")
    parser.add_argument('--config-file', '-c', type=pathlib.Path, help="YAML file with user configuration")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate model embeddings from source files. Default: false")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs. Default: 0")
    parser.add_argument('--host', type=str, default='127.0.0.1', help="Host address of the server. Default: 127.0.0.1")
    parser.add_argument('--port', '-p', type=int, default=8000, help="Port of the server. Default: 8000")
    parser.add_argument('--socket', type=pathlib.Path, help="Path to UNIX socket for server. Use to run server behind reverse proxy. Default: /tmp/uvicorn.sock")

    args = parser.parse_args(args)

    if not (args.config_file):
        print(f"Need --config-file flag to be passed from commandline")
        exit(1)
    elif not resolve_absolute_path(args.config_file).exists():
        print(f"Config file {args.config_file} does not exist")
        exit(1)

    # Read Config from YML file
    config_from_file = None
    with open(get_absolute_path(args.config_file), 'r', encoding='utf-8') as config_file:
        config_from_file = yaml.safe_load(config_file)

    # Parse, Validate Config in YML file
    args.config = FullConfig.parse_obj(config_from_file)

    return args