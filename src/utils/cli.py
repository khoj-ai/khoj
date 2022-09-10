# Standard Packages
import argparse
import pathlib
from importlib.metadata import version

# Internal Packages
from src.utils.helpers import resolve_absolute_path
from src.utils.yaml import parse_config_from_file


def cli(args=None):
    # Setup Argument Parser for the Commandline Interface
    parser = argparse.ArgumentParser(description="Start Khoj; A Natural Language Search Engine for your personal Notes, Transactions and Photos")
    parser.add_argument('--config-file', '-c', default='~/.khoj/khoj.yml', type=pathlib.Path, help="YAML file to configure Khoj")
    parser.add_argument('--no-gui', action='store_true', default=False, help="Do not show native desktop GUI. Default: false")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate model embeddings from source files. Default: false")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs. Default: 0")
    parser.add_argument('--host', type=str, default='127.0.0.1', help="Host address of the server. Default: 127.0.0.1")
    parser.add_argument('--port', '-p', type=int, default=8000, help="Port of the server. Default: 8000")
    parser.add_argument('--socket', type=pathlib.Path, help="Path to UNIX socket for server. Use to run server behind reverse proxy. Default: /tmp/uvicorn.sock")
    parser.add_argument('--version', '-V', action='store_true', help="Print the installed Khoj version and exit")

    args = parser.parse_args(args)

    if args.version:
        # Show version of khoj installed and exit
        print(version('khoj-assistant'))
        exit(0)

    # Normalize config_file path to absolute path
    args.config_file = resolve_absolute_path(args.config_file)

    if not args.config_file.exists():
        args.config = None
    else:
        args.config = parse_config_from_file(args.config_file)

    return args
