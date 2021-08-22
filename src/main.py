# Standard Packages
import sys
import argparse
import pathlib
from typing import Optional

# External Packages
import uvicorn
import yaml
from fastapi import FastAPI

# Internal Packages
from search_type import asymmetric
from processor.org_mode.org_to_jsonl import org_to_jsonl
from utils.helpers import is_none_or_empty, get_absolute_path, get_from_dict, merge_dicts


app = FastAPI()


@app.get('/search')
def search(q: str, n: Optional[int] = 5, t: Optional[str] = None):
    if q is None or q == '':
        print(f'No query param (q) passed in API call to initiate search')
        return {}

    user_query = q
    results_count = n

    if (t == 'notes' or t == None) and notes_search_enabled:
        # query notes
        hits = asymmetric.query_notes(
            user_query,
            corpus_embeddings,
            entries,
            bi_encoder,
            cross_encoder,
            top_k)

        # collate and return results
        return asymmetric.collate_results(hits, entries, results_count)

    else:
        return {}


@app.get('/regenerate')
def regenerate(t: Optional[str] = None):
    if (t == 'notes' or t == None) and notes_search_enabled:
        # Extract Entries, Generate Embeddings
        global corpus_embeddings
        global entries
        entries, corpus_embeddings, _, _, _ = asymmetric.setup(
            org_config['input-files'],
            org_config['input-filter'],
            pathlib.Path(org_config['compressed-jsonl']),
            pathlib.Path(org_config['embeddings-file']),
            regenerate=True,
            verbose=args.verbose)


    return {'status': 'ok', 'message': 'regeneration completed'}


def cli(args=None):
    if is_none_or_empty(args):
        args = sys.argv[1:]

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
    if args.config_file and args.config_file.exists():
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
        }
    },
    'search-type':
    {
        'asymmetric':
        {
            'encoder': "sentence-transformers/msmarco-MiniLM-L-6-v3",
            'cross-encoder': "cross-encoder/ms-marco-MiniLM-L-6-v2"
        }
    }
}


if __name__ == '__main__':
    args = cli()
    org_config = get_from_dict(args.config, 'content-type', 'org')

    notes_search_enabled = False
    if 'input-files' in org_config or 'input-filter' in org_config:
        notes_search_enabled = True
        entries, corpus_embeddings, bi_encoder, cross_encoder, top_k = asymmetric.setup(
            org_config['input-files'],
            org_config['input-filter'],
            pathlib.Path(org_config['compressed-jsonl']),
            pathlib.Path(org_config['embeddings-file']),
            args.regenerate,
            args.verbose)


    # Start Application Server
    uvicorn.run(app)
