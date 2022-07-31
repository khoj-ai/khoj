# Standard Packages
import argparse
import pathlib
import json

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

    # Config Priority: Config File > Default Config
    args.config = default_config
    if args.config_file and resolve_absolute_path(args.config_file).exists():
        with open(get_absolute_path(args.config_file), 'r', encoding='utf-8') as config_file:
            config_from_file = yaml.safe_load(config_file)
            args.config = merge_dicts(priority_dict=config_from_file, default_dict=args.config)

    args.config = FullConfig.parse_obj(args.config)

    return args


default_config = {
    'content-type':
    {
        'org': None,
        'ledger': None,
        'image': None,
        'music': None,
        'markdown': None,
    },
    'search-type':
    {
        'symmetric':
        {
            'encoder': "sentence-transformers/all-MiniLM-L6-v2",
            'cross-encoder': "cross-encoder/ms-marco-MiniLM-L-6-v2",
            'model_directory': None
        },
         'asymmetric':
        {
            'encoder': "sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
            'cross-encoder': "cross-encoder/ms-marco-MiniLM-L-6-v2",
            'model_directory': None
        },
        'image':
        {
            'encoder': "clip-ViT-B-32",
            'model_directory': None
        },
    },
    'processor':
    {
        'conversation':
        {
            'openai-api-key': "",
            'conversation-logfile': ".conversation_logs.json",
            'conversation-history': ""
        },
    }
}
