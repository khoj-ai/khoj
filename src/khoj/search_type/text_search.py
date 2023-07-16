# Standard Packages
import logging
import math
from pathlib import Path
from typing import List, Tuple, Type, Union

# External Packages
import torch
from sentence_transformers import SentenceTransformer, CrossEncoder, util
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.search_filter.base_filter import BaseFilter

# Internal Packages
from khoj.utils import state
from khoj.utils.helpers import get_absolute_path, is_none_or_empty, resolve_absolute_path, load_model, timer
from khoj.utils.config import TextContent, TextSearchModel
from khoj.utils.models import BaseEncoder
from khoj.utils.rawconfig import SearchResponse, TextSearchConfig, TextConfigBase, Entry
from khoj.utils.jsonl import load_jsonl


logger = logging.getLogger(__name__)


def initialize_model(search_config: TextSearchConfig):
    "Initialize model for semantic search on text"
    torch.set_num_threads(4)

    # If model directory is configured
    if search_config.model_directory:
        # Convert model directory to absolute path
        search_config.model_directory = resolve_absolute_path(search_config.model_directory)
        # Create model directory if it doesn't exist
        search_config.model_directory.parent.mkdir(parents=True, exist_ok=True)

    # The bi-encoder encodes all entries to use for semantic search
    bi_encoder = load_model(
        model_dir=search_config.model_directory,
        model_name=search_config.encoder,
        model_type=search_config.encoder_type or SentenceTransformer,
        device=f"{state.device}",
    )

    # The cross-encoder re-ranks the results to improve quality
    cross_encoder = load_model(
        model_dir=search_config.model_directory,
        model_name=search_config.cross_encoder,
        model_type=CrossEncoder,
        device=f"{state.device}",
    )

    return TextSearchModel(bi_encoder, cross_encoder)


def extract_entries(jsonl_file) -> List[Entry]:
    "Load entries from compressed jsonl"
    return list(map(Entry.from_dict, load_jsonl(jsonl_file)))


def compute_embeddings(
    entries_with_ids: List[Tuple[int, Entry]],
    bi_encoder: BaseEncoder,
    embeddings_file: Path,
    regenerate=False,
    normalize=True,
):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    new_embeddings = torch.tensor([], device=state.device)
    existing_embeddings = torch.tensor([], device=state.device)
    create_index_msg = ""
    # Load pre-computed embeddings from file if exists and update them if required
    if embeddings_file.exists() and not regenerate:
        corpus_embeddings: torch.Tensor = torch.load(get_absolute_path(embeddings_file), map_location=state.device)
        logger.debug(f"Loaded {len(corpus_embeddings)} text embeddings from {embeddings_file}")
    else:
        corpus_embeddings = torch.tensor([], device=state.device)
        create_index_msg = " Creating index from scratch."

    # Encode any new entries in the corpus and update corpus embeddings
    new_entries = [entry.compiled for id, entry in entries_with_ids if id == -1]
    if new_entries:
        logger.info(f"📩 Indexing {len(new_entries)} text entries.{create_index_msg}")
        new_embeddings = bi_encoder.encode(
            new_entries, convert_to_tensor=True, device=state.device, show_progress_bar=True
        )

    # Extract existing embeddings from previous corpus embeddings
    existing_entry_ids = [id for id, _ in entries_with_ids if id != -1]
    if existing_entry_ids:
        existing_embeddings = torch.index_select(
            corpus_embeddings, 0, torch.tensor(existing_entry_ids, device=state.device)
        )

    # Set corpus embeddings to merger of existing and new embeddings
    corpus_embeddings = torch.cat([existing_embeddings, new_embeddings], dim=0)
    if normalize:
        # Normalize embeddings for faster lookup via dot product when querying
        corpus_embeddings = util.normalize_embeddings(corpus_embeddings)

    # Save regenerated or updated embeddings to file
    torch.save(corpus_embeddings, embeddings_file)
    logger.info(f"📩 Saved computed text embeddings to {embeddings_file}")

    return corpus_embeddings


async def query(
    raw_query: str,
    search_model: TextSearchModel,
    content: TextContent,
    question_embedding: Union[torch.Tensor, None] = None,
    rank_results: bool = False,
    score_threshold: float = -math.inf,
    dedupe: bool = True,
) -> Tuple[List[dict], List[Entry]]:
    "Search for entries that answer the query"
    query, entries, corpus_embeddings = raw_query, content.entries, content.corpus_embeddings

    # Filter query, entries and embeddings before semantic search
    query, entries, corpus_embeddings = apply_filters(query, entries, corpus_embeddings, content.filters)

    # If no entries left after filtering, return empty results
    if entries is None or len(entries) == 0:
        return [], []
    # If query only had filters it'll be empty now. So short-circuit and return results.
    if query.strip() == "":
        hits = [{"corpus_id": id, "score": 1.0} for id, _ in enumerate(entries)]
        return hits, entries

    # Encode the query using the bi-encoder
    if question_embedding is None:
        with timer("Query Encode Time", logger, state.device):
            question_embedding = search_model.bi_encoder.encode([query], convert_to_tensor=True, device=state.device)
            question_embedding = util.normalize_embeddings(question_embedding)

    # Find relevant entries for the query
    top_k = min(len(entries), search_model.top_k or 10)  # top_k hits can't be more than the total entries in corpus
    with timer("Search Time", logger, state.device):
        hits = util.semantic_search(question_embedding, corpus_embeddings, top_k, score_function=util.dot_score)[0]

    # Score all retrieved entries using the cross-encoder
    if rank_results and search_model.cross_encoder:
        hits = cross_encoder_score(search_model.cross_encoder, query, entries, hits)

    # Filter results by score threshold
    hits = [hit for hit in hits if hit.get("cross-score", hit.get("score")) >= score_threshold]

    # Order results by cross-encoder score followed by bi-encoder score
    hits = sort_results(rank_results, hits)

    # Deduplicate entries by raw entry text before showing to users
    if dedupe:
        hits = deduplicate_results(entries, hits)

    return hits, entries


def collate_results(hits, entries: List[Entry], count=5) -> List[SearchResponse]:
    return [
        SearchResponse.parse_obj(
            {
                "entry": entries[hit["corpus_id"]].raw,
                "score": f"{hit.get('cross-score') or hit.get('score')}",
                "additional": {
                    "file": entries[hit["corpus_id"]].file,
                    "compiled": entries[hit["corpus_id"]].compiled,
                    "heading": entries[hit["corpus_id"]].heading,
                },
            }
        )
        for hit in hits[0:count]
    ]


def setup(
    text_to_jsonl: Type[TextToJsonl],
    config: TextConfigBase,
    bi_encoder: BaseEncoder,
    regenerate: bool,
    filters: List[BaseFilter] = [],
    normalize: bool = True,
) -> TextContent:
    # Map notes in text files to (compressed) JSONL formatted file
    config.compressed_jsonl = resolve_absolute_path(config.compressed_jsonl)
    previous_entries = []
    if config.compressed_jsonl.exists() and not regenerate:
        previous_entries = extract_entries(config.compressed_jsonl)
    entries_with_indices = text_to_jsonl(config).process(previous_entries)

    # Extract Updated Entries
    entries = extract_entries(config.compressed_jsonl)
    if is_none_or_empty(entries):
        config_params = ", ".join([f"{key}={value}" for key, value in config.dict().items()])
        raise ValueError(f"No valid entries found in specified files: {config_params}")

    # Compute or Load Embeddings
    config.embeddings_file = resolve_absolute_path(config.embeddings_file)
    corpus_embeddings = compute_embeddings(
        entries_with_indices, bi_encoder, config.embeddings_file, regenerate=regenerate, normalize=normalize
    )

    for filter in filters:
        filter.load(entries, regenerate=regenerate)

    return TextContent(entries, corpus_embeddings, filters)


def apply_filters(
    query: str, entries: List[Entry], corpus_embeddings: torch.Tensor, filters: List[BaseFilter]
) -> Tuple[str, List[Entry], torch.Tensor]:
    """Filter query, entries and embeddings before semantic search"""

    with timer("Total Filter Time", logger, state.device):
        included_entry_indices = set(range(len(entries)))
        filters_in_query = [filter for filter in filters if filter.can_filter(query)]
        for filter in filters_in_query:
            query, included_entry_indices_by_filter = filter.apply(query, entries)
            included_entry_indices.intersection_update(included_entry_indices_by_filter)

        # Get entries (and associated embeddings) satisfying all filters
        if not included_entry_indices:
            return "", [], torch.tensor([], device=state.device)
        else:
            entries = [entries[id] for id in included_entry_indices]
            corpus_embeddings = torch.index_select(
                corpus_embeddings, 0, torch.tensor(list(included_entry_indices), device=state.device)
            )

    return query, entries, corpus_embeddings


def cross_encoder_score(cross_encoder: CrossEncoder, query: str, entries: List[Entry], hits: List[dict]) -> List[dict]:
    """Score all retrieved entries using the cross-encoder"""
    with timer("Cross-Encoder Predict Time", logger, state.device):
        cross_inp = [[query, entries[hit["corpus_id"]].compiled] for hit in hits]
        cross_scores = cross_encoder.predict(cross_inp)

    # Store cross-encoder scores in results dictionary for ranking
    for idx in range(len(cross_scores)):
        hits[idx]["cross-score"] = cross_scores[idx]

    return hits


def sort_results(rank_results: bool, hits: List[dict]) -> List[dict]:
    """Order results by cross-encoder score followed by bi-encoder score"""
    with timer("Rank Time", logger, state.device):
        hits.sort(key=lambda x: x["score"], reverse=True)  # sort by bi-encoder score
        if rank_results:
            hits.sort(key=lambda x: x["cross-score"], reverse=True)  # sort by cross-encoder score
    return hits


def deduplicate_results(entries: List[Entry], hits: List[dict]) -> List[dict]:
    """Deduplicate entries by raw entry text before showing to users
    Compiled entries are split by max tokens supported by ML models.
    This can result in duplicate hits, entries shown to user."""

    with timer("Deduplication Time", logger, state.device):
        seen, original_hits_count = set(), len(hits)
        hits = [
            hit
            for hit in hits
            if entries[hit["corpus_id"]].raw not in seen and not seen.add(entries[hit["corpus_id"]].raw)  # type: ignore[func-returns-value]
        ]
        duplicate_hits = original_hits_count - len(hits)

    logger.debug(f"Removed {duplicate_hits} duplicates")
    return hits
