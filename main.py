from typing import Optional
from fastapi import FastAPI
from search_type import asymmetric
from processor.org_mode.org_to_jsonl import org_to_jsonl
from utils.helpers import is_none_or_empty
import argparse
import pathlib
import uvicorn

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
    org_to_jsonl(args.input_files, args.input_filter, args.compressed_jsonl, args.verbose)

    # Extract Entries
    global entries
    entries = asymmetric.extract_entries(args.compressed_jsonl, args.verbose)

    # Compute or Load Embeddings
    global corpus_embeddings
    corpus_embeddings = asymmetric.compute_embeddings(entries, bi_encoder, args.embeddings, regenerate=True, verbose=args.verbose)


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Expose API for Semantic Search")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of org-mode files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for org-mode files to process")
    parser.add_argument('--compressed-jsonl', '-j', type=pathlib.Path, default=pathlib.Path(".notes.jsonl.gz"), help="Compressed JSONL formatted notes file to compute embeddings from")
    parser.add_argument('--embeddings', '-e', type=pathlib.Path, default=pathlib.Path(".notes_embeddings.pt"), help="File to save/load model embeddings to/from")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate embeddings from org-mode files. Default: false")
    parser.add_argument('--verbose', action='count', help="Show verbose conversion logs. Default: 0")
    args = parser.parse_args()

    # Input Validation
    if is_none_or_empty(args.input_files) and is_none_or_empty(args.input_filter):
        print("At least one of org-files or org-file-filter is required to be specified")
        exit(1)

    # Initialize Model
    bi_encoder, cross_encoder, top_k = asymmetric.initialize_model()

    # Map notes in Org-Mode files to (compressed) JSONL formatted file
    if not args.compressed_jsonl.exists() or args.regenerate:
        org_to_jsonl(args.input_files, args.input_filter, args.compressed_jsonl, args.verbose)

    # Extract Entries
    entries = asymmetric.extract_entries(args.compressed_jsonl, args.verbose)

    # Compute or Load Embeddings
    corpus_embeddings = asymmetric.compute_embeddings(entries, bi_encoder, args.embeddings, regenerate=args.regenerate, verbose=args.verbose)

    # Start Application Server
    uvicorn.run(app)
