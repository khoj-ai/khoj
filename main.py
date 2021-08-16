from typing import Optional
from fastapi import FastAPI
from search_types import asymmetric
import argparse
import pathlib
import uvicorn

app = FastAPI()

def create_search_notes(corpus_embeddings, entries, bi_encoder, cross_encoder, top_k):
    "Closure to create search_notes method from initialized model, entries and embeddings"
    def search_notes(query):
        return asymmetric.query_notes(
            query,
            corpus_embeddings,
            entries,
            bi_encoder,
            cross_encoder,
            top_k)

    return search_notes


@app.get('/search')
def search(q: str, n: Optional[int] = 5, t: Optional[str] = 'notes'):
    if q is None or q == '':
        print(f'No query param (q) passed in API call to initiate search')
        return {}

    user_query = q
    results_count = n

    if t == 'notes':
        # query notes
        hits = search_notes(user_query)

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

    # Generate search_notes method from initialized model, entries and embeddings
    search_notes = create_search_notes(corpus_embeddings, entries, bi_encoder, cross_encoder, top_k)

    # Start Application Server
    uvicorn.run(app)
