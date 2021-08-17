# Standard Packages
import sys
import argparse
import pathlib
from typing import Optional

# External Packages
import uvicorn
from fastapi import FastAPI

# Internal Packages
from search_type import asymmetric
from processor.org_mode.org_to_jsonl import org_to_jsonl
from utils.helpers import is_none_or_empty


app = FastAPI()


@app.get('/search')
def search(q: str, n: Optional[int] = 5, t: Optional[str] = 'notes'):
    if q is None or q == '':
        print(f'No query param (q) passed in API call to initiate search')
        return {}

    user_query = q
    results_count = n

    if t == 'notes':
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
def regenerate():
    # Extract Entries, Generate Embeddings
    extracted_entries, computed_embeddings, _, _, _ = asymmetric.setup(args.input_files, args.input_filter, args.compressed_jsonl, args.embeddings, regenerate=True, verbose=args.verbose)

    # Now Update State
    # update state variables after regeneration complete
    # minimize time the application is in inconsistent, partially updated state
    global corpus_embeddings
    global entries
    entries = extracted_entries
    corpus_embeddings = computed_embeddings

    return {'status': 'ok', 'message': 'regeneration completed'}


def cli(args=None):
    if not args:
        args = sys.argv[1:]

    # Setup Argument Parser for the Commandline Interface
    parser = argparse.ArgumentParser(description="Expose API for Semantic Search")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of org-mode files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for org-mode files to process")
    parser.add_argument('--compressed-jsonl', '-j', type=pathlib.Path, default=pathlib.Path(".notes.jsonl.gz"), help="Compressed JSONL formatted notes file to compute embeddings from")
    parser.add_argument('--embeddings', '-e', type=pathlib.Path, default=pathlib.Path(".notes_embeddings.pt"), help="File to save/load model embeddings to/from")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate embeddings from org-mode files. Default: false")
    parser.add_argument('--verbose', '-v', action='count', default=0, help="Show verbose conversion logs. Default: 0")

    return parser.parse_args(args)


if __name__ == '__main__':
    args = cli()

    entries, corpus_embeddings, bi_encoder, cross_encoder, top_k = asymmetric.setup(args.input_files, args.input_filter, args.compressed_jsonl, args.embeddings, args.regenerate, args.verbose)

    # Start Application Server
    uvicorn.run(app)
