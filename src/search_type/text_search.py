# Standard Packages
import logging
import time

# External Packages
import torch
from sentence_transformers import SentenceTransformer, CrossEncoder, util
from src.search_filter.base_filter import BaseFilter

# Internal Packages
from src.utils import state
from src.utils.helpers import get_absolute_path, is_none_or_empty, resolve_absolute_path, load_model
from src.utils.config import TextSearchModel
from src.utils.rawconfig import TextSearchConfig, TextContentConfig
from src.utils.jsonl import load_jsonl


logger = logging.getLogger(__name__)


def initialize_model(search_config: TextSearchConfig):
    "Initialize model for semantic search on text"
    torch.set_num_threads(4)

    # Number of entries we want to retrieve with the bi-encoder
    top_k = 15

    # Convert model directory to absolute path
    search_config.model_directory = resolve_absolute_path(search_config.model_directory)

    # Create model directory if it doesn't exist
    search_config.model_directory.parent.mkdir(parents=True, exist_ok=True)

    # The bi-encoder encodes all entries to use for semantic search
    bi_encoder = load_model(
        model_dir  = search_config.model_directory,
        model_name = search_config.encoder,
        model_type = SentenceTransformer,
        device=f'{state.device}')

    # The cross-encoder re-ranks the results to improve quality
    cross_encoder = load_model(
        model_dir  = search_config.model_directory,
        model_name = search_config.cross_encoder,
        model_type = CrossEncoder,
        device=f'{state.device}')

    return bi_encoder, cross_encoder, top_k


def extract_entries(jsonl_file):
    "Load entries from compressed jsonl"
    return load_jsonl(jsonl_file)


def compute_embeddings(entries_with_ids, bi_encoder, embeddings_file, regenerate=False):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    new_entries = []
    # Load pre-computed embeddings from file if exists and update them if required
    if embeddings_file.exists() and not regenerate:
        corpus_embeddings = torch.load(get_absolute_path(embeddings_file), map_location=state.device)
        logger.info(f"Loaded embeddings from {embeddings_file}")

        # Encode any new entries in the corpus and update corpus embeddings
        new_entries = [entry['compiled'] for id, entry in entries_with_ids if id is None]
        if new_entries:
            new_embeddings = bi_encoder.encode(new_entries, convert_to_tensor=True, device=state.device, show_progress_bar=True)
            existing_entry_ids = [id for id, _ in entries_with_ids if id is not None]
            existing_embeddings = torch.index_select(corpus_embeddings, 0, torch.tensor(existing_entry_ids)) if existing_entry_ids else torch.Tensor()
            corpus_embeddings = torch.cat([existing_embeddings, new_embeddings], dim=0)
    # Else compute the corpus embeddings from scratch
    else:
        new_entries = [entry['compiled'] for _, entry in entries_with_ids]
        corpus_embeddings = bi_encoder.encode(new_entries, convert_to_tensor=True, device=state.device, show_progress_bar=True)

    # Save regenerated or updated embeddings to file
    if new_entries:
        corpus_embeddings = util.normalize_embeddings(corpus_embeddings)
        torch.save(corpus_embeddings, embeddings_file)
        logger.info(f"Computed embeddings and saved them to {embeddings_file}")

    return corpus_embeddings


def query(raw_query: str, model: TextSearchModel, rank_results=False):
    "Search for entries that answer the query"
    query, entries, corpus_embeddings = raw_query, model.entries, model.corpus_embeddings

    # Filter query, entries and embeddings before semantic search
    start_filter = time.time()
    included_entry_indices = set(range(len(entries)))
    filters_in_query = [filter for filter in model.filters if filter.can_filter(query)]
    for filter in filters_in_query:
        query, included_entry_indices_by_filter = filter.apply(query, entries)
        included_entry_indices.intersection_update(included_entry_indices_by_filter)

    # Get entries (and associated embeddings) satisfying all filters
    if not included_entry_indices:
        return [], []
    else:
        start = time.time()
        entries = [entries[id] for id in included_entry_indices]
        corpus_embeddings = torch.index_select(corpus_embeddings, 0, torch.tensor(list(included_entry_indices)))
        end = time.time()
        logger.debug(f"Keep entries satisfying all filters: {end - start} seconds")

    end_filter = time.time()
    logger.debug(f"Total Filter Time: {end_filter - start_filter:.3f} seconds")

    if entries is None or len(entries) == 0:
        return [], []

    # If query only had filters it'll be empty now. So short-circuit and return results.
    if query.strip() == "":
        hits = [{"corpus_id": id, "score": 1.0} for id, _ in enumerate(entries)]
        return hits, entries

    # Encode the query using the bi-encoder
    start = time.time()
    question_embedding = model.bi_encoder.encode([query], convert_to_tensor=True, device=state.device)
    question_embedding = util.normalize_embeddings(question_embedding)
    end = time.time()
    logger.debug(f"Query Encode Time: {end - start:.3f} seconds on device: {state.device}")

    # Find relevant entries for the query
    start = time.time()
    hits = util.semantic_search(question_embedding, corpus_embeddings, top_k=model.top_k, score_function=util.dot_score)[0]
    end = time.time()
    logger.debug(f"Search Time: {end - start:.3f} seconds on device: {state.device}")

    # Score all retrieved entries using the cross-encoder
    if rank_results:
        start = time.time()
        cross_inp = [[query, entries[hit['corpus_id']]['compiled']] for hit in hits]
        cross_scores = model.cross_encoder.predict(cross_inp)
        end = time.time()
        logger.debug(f"Cross-Encoder Predict Time: {end - start:.3f} seconds on device: {state.device}")

        # Store cross-encoder scores in results dictionary for ranking
        for idx in range(len(cross_scores)):
            hits[idx]['cross-score'] = cross_scores[idx]

    # Order results by cross-encoder score followed by bi-encoder score
    start = time.time()
    hits.sort(key=lambda x: x['score'], reverse=True) # sort by bi-encoder score
    if rank_results:
        hits.sort(key=lambda x: x['cross-score'], reverse=True) # sort by cross-encoder score
    end = time.time()
    logger.debug(f"Rank Time: {end - start:.3f} seconds on device: {state.device}")

    return hits, entries


def render_results(hits, entries, count=5, display_biencoder_results=False):
    "Render the Results returned by Search for the Query"
    if display_biencoder_results:
        # Output of top hits from bi-encoder
        print("\n-------------------------\n")
        print(f"Top-{count} Bi-Encoder Retrieval hits")
        hits = sorted(hits, key=lambda x: x['score'], reverse=True)
        for hit in hits[0:count]:
            print(f"Score: {hit['score']:.3f}\n------------\n{entries[hit['corpus_id']]['compiled']}")

    # Output of top hits from re-ranker
    print("\n-------------------------\n")
    print(f"Top-{count} Cross-Encoder Re-ranker hits")
    hits = sorted(hits, key=lambda x: x['cross-score'], reverse=True)
    for hit in hits[0:count]:
        print(f"CrossScore: {hit['cross-score']:.3f}\n-----------------\n{entries[hit['corpus_id']]['compiled']}")


def collate_results(hits, entries, count=5):
    return [
        {
            "entry": entries[hit['corpus_id']]['raw'],
            "score": f"{hit['cross-score'] if 'cross-score' in hit else hit['score']:.3f}"
        }
        for hit
        in hits[0:count]]


def setup(text_to_jsonl, config: TextContentConfig, search_config: TextSearchConfig, regenerate: bool, filters: list[BaseFilter] = []) -> TextSearchModel:
    # Initialize Model
    bi_encoder, cross_encoder, top_k = initialize_model(search_config)

    # Map notes in text files to (compressed) JSONL formatted file
    config.compressed_jsonl = resolve_absolute_path(config.compressed_jsonl)
    previous_entries = extract_entries(config.compressed_jsonl) if config.compressed_jsonl.exists() and not regenerate else None
    entries_with_indices = text_to_jsonl(config, previous_entries)

    # Extract Updated Entries
    entries = extract_entries(config.compressed_jsonl)
    if is_none_or_empty(entries):
        raise ValueError(f"No valid entries found in specified files: {config.input_files} or {config.input_filter}")
    top_k = min(len(entries), top_k)  # top_k hits can't be more than the total entries in corpus

    # Compute or Load Embeddings
    config.embeddings_file = resolve_absolute_path(config.embeddings_file)
    corpus_embeddings = compute_embeddings(entries_with_indices, bi_encoder, config.embeddings_file, regenerate=regenerate)

    for filter in filters:
        filter.load(entries, regenerate=regenerate)

    return TextSearchModel(entries, corpus_embeddings, bi_encoder, cross_encoder, filters, top_k)
