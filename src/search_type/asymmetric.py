#!/usr/bin/env python

# Standard Packages
import json
import gzip
import argparse
import pathlib
from copy import deepcopy

# External Packages
import torch
from sentence_transformers import SentenceTransformer, CrossEncoder, util

# Internal Packages
from src.utils.helpers import get_absolute_path, resolve_absolute_path, load_model
from src.processor.org_mode.org_to_jsonl import org_to_jsonl
from src.utils.config import TextSearchModel
from src.utils.rawconfig import AsymmetricSearchConfig, TextContentConfig
from src.utils.constants import empty_escape_sequences


def initialize_model(search_config: AsymmetricSearchConfig):
    "Initialize model for assymetric semantic search. That is, where query smaller than results"
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
    entries = []
    jsonl_file = None

    # Open File
    if notesfile.suffix == ".gz":
        jsonl_file = gzip.open(get_absolute_path(notesfile), "rt", encoding='utf8')
    elif notesfile.suffix == ".jsonl":
        jsonl_file = open(get_absolute_path(notesfile), "r", encoding='utf8')

    # Read File
    for line in jsonl_file:
        note = json.loads(line.strip(empty_escape_sequences))

        # Ignore title notes i.e notes with just headings and empty body
        if not "Body" in note or note["Body"].strip(empty_escape_sequences) == "":
            continue

        note_string = f'{note["Title"]}' \
                      f'\t{note["Tags"] if "Tags" in note else ""}' \
                      f'\n{note["Body"] if "Body" in note else ""}'
        entries.append([note_string, note["Raw"]])

    # Close File
    jsonl_file.close()

    if verbose > 0:
        print(f"Loaded {len(entries)} entries from {notesfile}")

    return entries


def compute_embeddings(entries, bi_encoder, embeddings_file, regenerate=False, device='cpu', verbose=0):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    # Load pre-computed embeddings from file if exists
    if resolve_absolute_path(embeddings_file).exists() and not regenerate:
        corpus_embeddings = torch.load(get_absolute_path(embeddings_file))
        if verbose > 0:
            print(f"Loaded embeddings from {embeddings_file}")

    else:  # Else compute the corpus_embeddings from scratch, which can take a while
        corpus_embeddings = bi_encoder.encode([entry[0] for entry in entries], convert_to_tensor=True, show_progress_bar=True)
        corpus_embeddings.to(device)
        corpus_embeddings = util.normalize_embeddings(corpus_embeddings)
        torch.save(corpus_embeddings, get_absolute_path(embeddings_file))
        if verbose > 0:
            print(f"Computed embeddings and saved them to {embeddings_file}")

    return corpus_embeddings


def query(raw_query: str, model: TextSearchModel, device=torch.device('cpu'), filters: list = []):
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
    question_embedding = model.bi_encoder.encode([query], convert_to_tensor=True)
    question_embedding.to(device)
    question_embedding = util.normalize_embeddings(question_embedding)

    # Find relevant entries for the query
    hits = util.semantic_search(question_embedding, corpus_embeddings, top_k=model.top_k, score_function=util.dot_score)[0]

    # Score all retrieved entries using the cross-encoder
    cross_inp = [[query, entries[hit['corpus_id']][0]] for hit in hits]
    cross_scores = model.cross_encoder.predict(cross_inp)

    # Store cross-encoder scores in results dictionary for ranking
    for idx in range(len(cross_scores)):
        hits[idx]['cross-score'] = cross_scores[idx]

    # Order results by cross-encoder score followed by bi-encoder score
    hits.sort(key=lambda x: x['score'], reverse=True) # sort by bi-encoder score
    hits.sort(key=lambda x: x['cross-score'], reverse=True) # sort by cross-encoder score

    return hits, entries


def render_results(hits, entries, count=5, display_biencoder_results=False):
    "Render the Results returned by Search for the Query"
    if display_biencoder_results:
        # Output of top hits from bi-encoder
        print("\n-------------------------\n")
        print(f"Top-{count} Bi-Encoder Retrieval hits")
        hits = sorted(hits, key=lambda x: x['score'], reverse=True)
        for hit in hits[0:count]:
            print(f"Score: {hit['score']:.3f}\n------------\n{entries[hit['corpus_id']][0]}")

    # Output of top hits from re-ranker
    print("\n-------------------------\n")
    print(f"Top-{count} Cross-Encoder Re-ranker hits")
    hits = sorted(hits, key=lambda x: x['cross-score'], reverse=True)
    for hit in hits[0:count]:
        print(f"CrossScore: {hit['cross-score']:.3f}\n-----------------\n{entries[hit['corpus_id']][0]}")


def collate_results(hits, entries, count=5):
    return [
        {
            "Entry": entries[hit['corpus_id']][1],
            "Score": f"{hit['cross-score']:.3f}"
        }
        for hit
        in hits[0:count]]


def setup(config: TextContentConfig, search_config: AsymmetricSearchConfig, regenerate: bool, device=torch.device('cpu'), verbose: bool=False) -> TextSearchModel:
    # Initialize Model
    bi_encoder, cross_encoder, top_k = initialize_model(search_config)

    # Map notes in Org-Mode files to (compressed) JSONL formatted file
    if not resolve_absolute_path(config.compressed_jsonl).exists() or regenerate:
        org_to_jsonl(config.input_files, config.input_filter, config.compressed_jsonl, verbose)

    # Extract Entries
    entries = extract_entries(config.compressed_jsonl, verbose)
    top_k = min(len(entries), top_k)  # top_k hits can't be more than the total entries in corpus

    # Compute or Load Embeddings
    corpus_embeddings = compute_embeddings(entries, bi_encoder, config.embeddings_file, regenerate=regenerate, device=device, verbose=verbose)

    return TextSearchModel(entries, corpus_embeddings, bi_encoder, cross_encoder, top_k, verbose=verbose)


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Map Org-Mode notes into (compressed) JSONL format")
    parser.add_argument('--input-files', '-i', nargs='*', help="List of org-mode files to process")
    parser.add_argument('--input-filter', type=str, default=None, help="Regex filter for org-mode files to process")
    parser.add_argument('--compressed-jsonl', '-j', type=pathlib.Path, default=pathlib.Path(".notes.jsonl.gz"), help="Compressed JSONL formatted notes file to compute embeddings from")
    parser.add_argument('--embeddings', '-e', type=pathlib.Path, default=pathlib.Path(".notes_embeddings.pt"), help="File to save/load model embeddings to/from")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate embeddings from org-mode files. Default: false")
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

        # query notes
        hits = query(user_query, corpus_embeddings, entries, bi_encoder, cross_encoder, top_k)

        # render results
        render_results(hits, entries, count=args.results_count)
