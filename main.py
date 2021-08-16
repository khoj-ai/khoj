from typing import Optional
from fastapi import FastAPI
from search_type import asymmetric
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
            q,
            corpus_embeddings,
            entries,
            bi_encoder,
            cross_encoder,
            top_k)

        # collate and return results
        return asymmetric.collate_results(hits, entries, results_count)

    else:
        return {}


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Expose API for Semantic Search")
    parser.add_argument('--compressed-jsonl', '-j', required=True, type=pathlib.Path, help="Compressed JSONL formatted notes file to compute embeddings from")
    parser.add_argument('--embeddings', '-e', required=True, type=pathlib.Path, help="File to save/load model embeddings to/from")
    parser.add_argument('--verbose', action='store_true', default=False, help="Show verbose conversion logs. Default: false")
    args = parser.parse_args()

    # Initialize Model
    bi_encoder, cross_encoder, top_k = asymmetric.initialize_model()

    # Extract Entries
    entries = asymmetric.extract_entries(args.compressed_jsonl, args.verbose)

    # Compute or Load Embeddings
    corpus_embeddings = asymmetric.compute_embeddings(entries, bi_encoder, args.embeddings, args.verbose)

    # Start Application Server
    uvicorn.run(app)
