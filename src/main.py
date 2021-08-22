# Standard Packages
import sys
import pathlib
from typing import Optional

# External Packages
import uvicorn
from fastapi import FastAPI

# Internal Packages
from search_type import asymmetric
from utils.helpers import get_from_dict
from utils.cli import cli


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


if __name__ == '__main__':
    args = cli(sys.argv[1:])
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
