#!/usr/bin/env python

import json
from sentence_transformers import SentenceTransformer, CrossEncoder, util
import time
import gzip
import os
import sys
import torch
import argparse
import pathlib


def initialize_model():
    "Initialize model for assymetric semantic search. That is, where query smaller than results"
    bi_encoder = SentenceTransformer('msmarco-MiniLM-L-6-v3')             # The bi-encoder encodes all entries to use for semantic search
    top_k = 100                                                           # Number of entries we want to retrieve with the bi-encoder
    cross_encoder = CrossEncoder('cross-encoder/ms-marco-MiniLM-L-6-v2')  # The cross-encoder re-ranks the results to improve quality
    return bi_encoder, cross_encoder, top_k


def extract_entries(notesfile, verbose=False):
    "Load entries from compressed jsonl"
    entries = []
    with gzip.open(str(notesfile.expanduser()), 'rt', encoding='utf8') as jsonl:
        for line in jsonl:
            note = json.loads(line.strip())

            # Ignore title notes i.e notes with just headings and empty body
            if not "Body" in note or note["Body"].strip() == "":
                continue

            note_string = f'{note["Title"]}\t{note["Tags"] if "Tags" in note else ""}\n{note["Body"] if "Body" in note else ""}'
            entries.extend([note_string])

    if verbose:
        print(f"Loaded {len(entries)} entries from {notesfile}")

    return entries


def compute_embeddings(entries, bi_encoder, embeddings_file, verbose=False):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    # Load pre-computed embeddings from file if exists
    if embeddings_file.exists():
        corpus_embeddings = torch.load(str(embeddings_file.expanduser()))
        if verbose:
            print(f"Loaded embeddings from {embeddings_file}")

    else:  # Else compute the corpus_embeddings from scratch, which can take a while
        corpus_embeddings = bi_encoder.encode(entries, convert_to_tensor=True, show_progress_bar=True)
        torch.save(corpus_embeddings, str(embeddings_file.expanduser()))
        if verbose:
            print(f"Computed embeddings and save them to {embeddings_file}")

    return corpus_embeddings


def query_notes(query, corpus_embeddings, entries, bi_encoder, cross_encoder, topk=100):
    "Search all notes for entries that answer the query"
    # Encode the query using the bi-encoder
    question_embedding = bi_encoder.encode(query, convert_to_tensor=True)

    # Find relevant entries for the query
    hits = util.semantic_search(question_embedding, corpus_embeddings, top_k=top_k)
    hits = hits[0]  # Get the hits for the first query

    # Score all retrieved entries using the cross-encoder
    cross_inp = [[query, entries[hit['corpus_id']]] for hit in hits]
    cross_scores = cross_encoder.predict(cross_inp)

    # Store cross-encoder scores in results dictionary for ranking
    for idx in range(len(cross_scores)):
        hits[idx]['cross-score'] = cross_scores[idx]

    # Order results by cross encoder score followed by biencoder score
    hits.sort(key=lambda x: x['score'], reverse=True) # sort by biencoder score
    hits.sort(key=lambda x: x['cross-score'], reverse=True) # sort by cross encoder score
    return hits


def render_results(hits, entries, count=5, display_biencoder_results=False):
    "Render the Results returned by Search for the Query"
    if display_biencoder_results:
        # Output of top hits from bi-encoder
        print("\n-------------------------\n")
        print(f"Top-{count} Bi-Encoder Retrieval hits")
        hits = sorted(hits, key=lambda x: x['score'], reverse=True)
        for hit in hits[0:count]:
            print(f"Score: {hit['score']:.3f}\n------------\n{entries[hit['corpus_id']]}")

    # Output of top hits from re-ranker
    print("\n-------------------------\n")
    print(f"Top-{count} Cross-Encoder Re-ranker hits")
    hits = sorted(hits, key=lambda x: x['cross-score'], reverse=True)
    for hit in hits[0:count]:
        print(f"CrossScore: {hit['cross-score']:.3f}\n-----------------\n{entries[hit['corpus_id']]}")


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Map Org-Mode notes into JSONL format")
    parser.add_argument('--jsonl-file', '-j', required=True, type=pathlib.Path, help="Input file for compressed JSONL formatted notes to compute embeddings from")
    parser.add_argument('--embeddings-file', '-e', type=pathlib.Path, help="File to save/load model embeddings to/from. Default: ./embeddings.pt")
    parser.add_argument('--results-count', '-n', default=5, type=int, help="Number of results to render. Default: 5")
    parser.add_argument('--interactive', action='store_true', default=False, help="Interactive mode allows user to run queries on the model. Default: true")
    parser.add_argument('--verbose', action='store_true', default=False, help="Show verbose conversion logs. Default: false")
    args = parser.parse_args()

    # Initialize Model
    bi_encoder, cross_encoder, top_k = initialize_model()

    # Extract Entries
    entries = extract_entries(args.jsonl_file, args.verbose)

    # Compute or Load Embeddings
    corpus_embeddings = compute_embeddings(entries, bi_encoder, args.embeddings_file, args.verbose)

    # Run User Queries on Entries in Interactive Mode
    while args.interactive:
        # get query from user
        user_query = input("Enter your query: ")
        if user_query == "exit":
            exit(0)

        # query notes
        hits = query_notes(user_query, corpus_embeddings, entries, bi_encoder, cross_encoder, top_k)

        # render results
        render_results(hits, entries, count=args.results_count)
