# Standard Packages
import argparse
import pathlib

# External Packages
import yaml

# Internal Packages
from utils.helpers import is_none_or_empty, get_absolute_path, resolve_absolute_path, get_from_dict, merge_dicts

def cli(args=None):
    if is_none_or_empty(args):
        return None

    # Setup Argument Parser for the Commandline Interface
    parser = argparse.ArgumentParser(description="Expose API for Semantic Search")
    parser.add_argument('--org-files', '-i', nargs='*', help="List of org-mode files to process")
    parser.add_argument('--org-filter', type=str, default=None, help="Regex filter for org-mode files to process")
    parser.add_argument('--config-file', '-c', type=pathlib.Path, help="YAML file with user configuration")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate model embeddings from source files. Default: false")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs. Default: 0")
    args = parser.parse_args(args)

    if not (args.config_file or args.org_files):
        print(f"Require at least 1 of --org-file, --org-filter or --config-file flags to be passed from commandline")
        exit(1)

    # Config Priority: Cmd Args > Config File > Default Config
    args.config = default_config
    if args.config_file and resolve_absolute_path(args.config_file).exists():
        with open(get_absolute_path(args.config_file), 'r', encoding='utf-8') as config_file:
            config_from_file = yaml.safe_load(config_file)
            args.config = merge_dicts(priority_dict=config_from_file, default_dict=args.config)

    if args.org_files:
        args.config['content-type']['org']['input-files'] = args.org_files

    if args.org_filter:
        args.config['content-type']['org']['input-filter'] = args.org_filter

    return args


default_config = {
    'content-type':
    {
        'org':
        {
            'compressed-jsonl': '.notes.jsonl.gz',
            'embeddings-file': '.note_embeddings.pt'
        },
        'ledger':
        {
            'compressed-jsonl': '.transactions.jsonl.gz',
            'embeddings-file': '.transaction_embeddings.pt'
        },
        'image':
        {
            'embeddings-file': '.image_embeddings.pt',
            'batch-size': 50
        },
        'music':
        {
            'compressed-jsonl': '.songs.jsonl.gz',
            'embeddings-file': '.song_embeddings.pt'
        },
    },
    'search-type':
    {
        'asymmetric':
        {
            'encoder': "sentence-transformers/msmarco-MiniLM-L-6-v3",
            'cross-encoder': "cross-encoder/ms-marco-MiniLM-L-6-v2"
        },
        'image':
        {
            'encoder': "clip-ViT-B-32"
        }
    }
}
