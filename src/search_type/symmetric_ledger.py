# Standard Packages
import argparse
import pathlib
from copy import deepcopy

# External Packages
import torch
from sentence_transformers import SentenceTransformer, CrossEncoder, util

# Internal Packages
from src.utils.helpers import get_absolute_path, resolve_absolute_path, load_model
from src.processor.ledger.beancount_to_jsonl import beancount_to_jsonl, load_jsonl
from src.utils.config import TextSearchModel
from src.utils.rawconfig import SymmetricSearchConfig, TextContentConfig


def initialize_model(search_config: SymmetricSearchConfig):
    "Initialize model for symmetric semantic search. That is, where query of similar size to results"
    torch.set_num_threads(4)

    # Number of entries we want to retrieve with the bi-encoder
    top_k = 30

    # The bi-encoder encodes all entries to use for semantic search
    bi_encoder = load_model(
        model_dir  = search_config.model_directory,
        model_name = search_config.encoder,
        model_type = SentenceTransformer)

    # The cross-encoder re-ranks the results to improve quality
    cross_encoder = load_model(
        model_dir  = search_config.model_directory,
        model_name = search_config.cross_encoder,
        model_type = CrossEncoder)

    return bi_encoder, cross_encoder, top_k


def extract_entries(notesfile, verbose=0):
    "Load entries from compressed jsonl"
    return [{'raw': f'{entry["Title"]}', 'embed': f'{entry["Title"]}'}
            for entry
            in load_jsonl(notesfile, verbose=verbose)]


def compute_embeddings(entries, bi_encoder, embeddings_file, regenerate=False, verbose=0):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    # Load pre-computed embeddings from file if exists
    if resolve_absolute_path(embeddings_file).exists() and not regenerate:
        corpus_embeddings = torch.load(get_absolute_path(embeddings_file))
        if verbose > 0:
            print(f"Loaded embeddings from {embeddings_file}")

    else:  # Else compute the corpus_embeddings from scratch, which can take a while
        corpus_embeddings = bi_encoder.encode(entries, convert_to_tensor=True, show_progress_bar=True)
        torch.save(corpus_embeddings, get_absolute_path(embeddings_file))
        if verbose > 0:
            print(f"Computed embeddings and saved them to {embeddings_file}")

    return corpus_embeddings


def query(raw_query, model: TextSearchModel, filters=[]):
    "Search all notes for entries that answer the query"
    # Copy original embeddings, entries to filter them for query
    query = raw_query
    corpus_embeddings = deepcopy(model.corpus_embeddings)
    entries = deepcopy(model.entries)

    # Filter query, entries and embeddings before semantic search
    for filter in filters:
        query, entries, corpus_embeddings = filter(query, entries, corpus_embeddings)
    if entries is None or len(entries) == 0:
        return [], []

    # Encode the query using the bi-encoder
    question_embedding = model.bi_encoder.encode(query, convert_to_tensor=True)

    # Find relevant entries for the query
    hits = util.semantic_search(question_embedding, corpus_embeddings, top_k=model.top_k)[0]

    # Score all retrieved entries using the cross-encoder
    cross_inp = [[query, entries[hit['corpus_id']]['embed']] for hit in hits]
    cross_scores = model.cross_encoder.predict(cross_inp)

    # Store cross-encoder scores in results dictionary for ranking
    for idx in range(len(cross_scores)):
        hits[idx]['cross-score'] = cross_scores[idx]

    # Order results by cross encoder score followed by biencoder score
    hits.sort(key=lambda x: x['score'], reverse=True) # sort by biencoder score
    hits.sort(key=lambda x: x['cross-score'], reverse=True) # sort by cross encoder score

    return hits, entries


def render_results(hits, entries, count=5, display_biencoder_results=False):
    "Render the Results returned by Search for the Query"
    if display_biencoder_results:
        # Output of top hits from bi-encoder
        print("\n-------------------------\n")
        print(f"Top-{count} Bi-Encoder Retrieval hits")
        hits = sorted(hits, key=lambda x: x['score'], reverse=True)
        for hit in hits[0:count]:
            print(f"Score: {hit['score']:.3f}\n------------\n{entries[hit['corpus_id']]['embed']}")

    # Output of top hits from re-ranker
    print("\n-------------------------\n")
    print(f"Top-{count} Cross-Encoder Re-ranker hits")
    hits = sorted(hits, key=lambda x: x['cross-score'], reverse=True)
    for hit in hits[0:count]:
        print(f"CrossScore: {hit['cross-score']:.3f}\n-----------------\n{entries[hit['corpus_id']]['embed']}")


def collate_results(hits, entries, count=5):
    return [
        {
            "entry": entries[hit['corpus_id']]['raw'],
            "score": f"{hit['cross-score']:.3f}"
        }
        for hit
        in hits[0:count]]


def setup(config: TextContentConfig, search_config: SymmetricSearchConfig, regenerate: bool, verbose: bool) -> TextSearchModel:
    # Initialize Model
    bi_encoder, cross_encoder, top_k = initialize_model(search_config)

    # Map notes in Org-Mode files to (compressed) JSONL formatted file
    if not resolve_absolute_path(config.compressed_jsonl).exists() or regenerate:
        beancount_to_jsonl(config.input_files, config.input_filter, config.compressed_jsonl, verbose)

    # Extract Entries
    entries = extract_entries(config.compressed_jsonl, verbose)
    top_k = min(len(entries), top_k)

    # Compute or Load Embeddings
    corpus_embeddings = compute_embeddings(entries, bi_encoder, config.embeddings_file, regenerate=regenerate, verbose=verbose)

    return TextSearchModel(entries, corpus_embeddings, bi_encoder, cross_encoder, top_k, verbose=verbose)


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Map Beancount transactions into (compressed) JSONL format")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of Beancount files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for Beancount files to process")
    parser.add_argument('--compressed-jsonl', '-j', type=pathlib.Path, default=pathlib.Path(".transactions.jsonl.gz"), help="Compressed JSONL formatted transactions file to compute embeddings from")
    parser.add_argument('--embeddings', '-e', type=pathlib.Path, default=pathlib.Path(".transaction_embeddings.pt"), help="File to save/load model embeddings to/from")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate embeddings from Beancount files. Default: false")
    parser.add_argument('--results-count', '-n', default=5, type=int, help="Number of results to render. Default: 5")
    parser.add_argument('--interactive', action='store_true', default=False, help="Interactive mode allows user to run queries on the model. Default: true")
    parser.add_argument('--verbose', action='count', default=0, help="Show verbose conversion logs. Default: 0")
    args = parser.parse_args()

    entries, corpus_embeddings, bi_encoder, cross_encoder, top_k = setup(args.input_files, args.input_filter, args.compressed_jsonl, args.embeddings, args.regenerate, args.verbose)

    # Run User Queries on Entries in Interactive Mode
    while args.interactive:
        # get query from user
        user_query = input("Enter your query: ")
        if user_query == "exit":
            exit(0)

        # query
        hits = query(user_query, corpus_embeddings, entries, bi_encoder, cross_encoder, top_k)

        # render results
        render_results(hits, entries, count=args.results_count)
