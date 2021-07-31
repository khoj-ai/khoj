#!/usr/bin/env python

import json
from sentence_transformers import SentenceTransformer, CrossEncoder, util
import time
import gzip
import os
import sys

# We use the Bi-Encoder to encode all passages, so that we can use it with sematic search
model_name = 'msmarco-MiniLM-L-6-v3'
bi_encoder = SentenceTransformer(model_name)
top_k = 100  # Number of passages we want to retrieve with the bi-encoder

# The bi-encoder will retrieve 100 documents.
# We use a cross-encoder, to re-rank the results list to improve the quality
cross_encoder = CrossEncoder('cross-encoder/ms-marco-MiniLM-L-6-v2')

# We split these articles into paragraphs and encode them with the bi-encoder
notes_filepath = 'Notes.jsonl.gz'

passages = []
with gzip.open(notes_filepath, 'rt', encoding='utf8') as fIn:
    for line in fIn:
        data = json.loads(line.strip())
        passages.extend([f'{data["Title"]}\n{data["Body"] if "Body" in data else ""}'])

print(f"Passages: {len(passages)}")

# Here, we compute the corpus_embeddings from scratch (which can take a while depending on the GPU)
corpus_embeddings = bi_encoder.encode(passages, convert_to_tensor=True, show_progress_bar=True)

# This function will search all notes for passages that answer the query
def search(query):
    print("Input question:", query)

    ##### Sematic Search #####
    # Encode the query using the bi-encoder and find potentially relevant passages
    question_embedding = bi_encoder.encode(query, convert_to_tensor=True)
    #question_embedding = question_embedding.cuda()
    hits = util.semantic_search(question_embedding, corpus_embeddings, top_k=top_k)
    hits = hits[0]  # Get the hits for the first query

    ##### Re-Ranking #####
    # Now, score all retrieved passages with the cross_encoder
    cross_inp = [[query, passages[hit['corpus_id']]] for hit in hits]
    cross_scores = cross_encoder.predict(cross_inp)

    # Sort results by the cross-encoder scores
    for idx in range(len(cross_scores)):
        hits[idx]['cross-score'] = cross_scores[idx]

    # Output of top-5 hits from bi-encoder
    print("\n-------------------------\n")
    print("Top-3 Bi-Encoder Retrieval hits")
    hits = sorted(hits, key=lambda x: x['score'], reverse=True)
    for hit in hits[0:3]:
        print("\t{:.3f}\t{}".format(hit['score'], passages[hit['corpus_id']].replace("\n", " ")))

    # Output of top-5 hits from re-ranker
    print("\n-------------------------\n")
    print("Top-3 Cross-Encoder Re-ranker hits")
    hits = sorted(hits, key=lambda x: x['cross-score'], reverse=True)
    for hit in hits[0:3]:
        print("\t{:.3f}\t{}".format(hit['cross-score'], passages[hit['corpus_id']].replace("\n", " ")))

while True:
    user_query = input("Enter your query: ")
    if user_query == "exit":
        exit(0)
    search(query = user_query)
