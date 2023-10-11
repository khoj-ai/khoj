# Standard Packages
import logging
import math
from pathlib import Path
from typing import List, Tuple, Type, Union

# External Packages
import torch
from sentence_transformers import SentenceTransformer, CrossEncoder, util

from asgiref.sync import sync_to_async


# Internal Packages
from khoj.utils import state
from khoj.utils.helpers import get_absolute_path, resolve_absolute_path, load_model, timer
from khoj.utils.config import TextContent, TextSearchModel
from khoj.utils.models import BaseEncoder
from khoj.utils.state import SearchType
from khoj.utils.rawconfig import SearchResponse, TextSearchConfig, TextConfigBase, Entry
from khoj.utils.jsonl import load_jsonl
from khoj.processor.text_to_jsonl import TextEmbeddings
from khoj.search_filter.base_filter import BaseFilter
from database.adapters import EmbeddingsAdapters
from database.models import KhojUser, Embeddings

logger = logging.getLogger(__name__)

search_type_to_embeddings_type = {
    SearchType.Org.value: Embeddings.EmbeddingsType.ORG,
    SearchType.Markdown.value: Embeddings.EmbeddingsType.MARKDOWN,
    SearchType.Plaintext.value: Embeddings.EmbeddingsType.PLAINTEXT,
    SearchType.Pdf.value: Embeddings.EmbeddingsType.PDF,
    SearchType.Github.value: Embeddings.EmbeddingsType.GITHUB,
    SearchType.Notion.value: Embeddings.EmbeddingsType.NOTION,
    SearchType.All.value: None,
}


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


def load_embeddings(
    embeddings_file: Path,
):
    "Load pre-computed embeddings from file if exists and update them if required"
    if embeddings_file.exists():
        corpus_embeddings: torch.Tensor = torch.load(get_absolute_path(embeddings_file), map_location=state.device)
        logger.debug(f"Loaded {len(corpus_embeddings)} text embeddings from {embeddings_file}")
        return util.normalize_embeddings(corpus_embeddings)

    return None


async def query(
    user: KhojUser,
    raw_query: str,
    type: SearchType = SearchType.All,
    question_embedding: Union[torch.Tensor, None] = None,
    rank_results: bool = False,
    score_threshold: float = -math.inf,
    dedupe: bool = True,
) -> Tuple[List[dict], List[Entry]]:
    "Search for entries that answer the query"

    file_type = search_type_to_embeddings_type[type.value]

    query = raw_query

    # Filter query, entries and embeddings before semantic search
    # query, entries, corpus_embeddings = apply_filters(query, entries, corpus_embeddings, content.filters)

    # If no entries left after filtering, return empty results
    # if entries is None or len(entries) == 0:
    #     return [], []
    # # If query only had filters it'll be empty now. So short-circuit and return results.
    # if query.strip() == "":
    #     hits = [{"corpus_id": id, "score": 1.0} for id, _ in enumerate(entries)]
    #     return hits, entries

    # Encode the query using the bi-encoder
    if question_embedding is None:
        with timer("Query Encode Time", logger, state.device):
            question_embedding = state.embeddings_model.embed_query(query)

    # Find relevant entries for the query
    top_k = 10
    with timer("Search Time", logger, state.device):
        hits = EmbeddingsAdapters.search_with_embeddings(
            user=user,
            embeddings=question_embedding,
            max_results=top_k,
            file_filter=file_type,
        ).all()
        hits = await sync_to_async(list)(hits)  # type: ignore[call-arg]
        # hits = util.semantic_search(question_embedding, corpus_embeddings, top_k, score_function=util.dot_score)[0]

    # Score all retrieved entries using the cross-encoder
    # if rank_results and search_model.cross_encoder:
    #     hits = cross_encoder_score(search_model.cross_encoder, query, entries, hits)

    # Filter results by score threshold
    # hits = [hit for hit in hits if hit.get("cross-score", hit.get("score")) >= score_threshold]

    # Order results by cross-encoder score followed by bi-encoder score
    # hits = sort_results(rank_results, hits)

    # Deduplicate entries by raw entry text before showing to users
    # if dedupe:
    # hits = deduplicate_results(entries, hits)

    return hits


def collate_results(hits):
    hit_ids = set()
    for hit in hits:
        if hit.id not in hit_ids:
            hit_ids.add(hit.id)
            yield SearchResponse.parse_obj(
                {
                    "entry": hit.raw,
                    "score": hit.distance,
                    "additional": {
                        "file": hit.file_path,
                        "compiled": hit.compiled,
                        "heading": hit.heading,
                    },
                }
            )


def setup(
    text_to_jsonl: Type[TextEmbeddings],
    files: dict[str, str],
    regenerate: bool,
    filters: List[BaseFilter] = [],
    normalize: bool = True,
    full_corpus: bool = True,
    user: KhojUser = None,
    config=None,
) -> None:
    if config:
        num_new_embeddings, num_deleted_embeddings = text_to_jsonl(config).process(
            files=files, full_corpus=full_corpus, user=user, regenerate=regenerate
        )
    else:
        num_new_embeddings, num_deleted_embeddings = text_to_jsonl().process(
            files=files, full_corpus=full_corpus, user=user, regenerate=regenerate
        )

    logger.info(
        f"Created {num_new_embeddings} new embeddings. Deleted {num_deleted_embeddings} embeddings for user {user} and files {files}"
    )

    # TODO: Update the way filters are applied
    # for filter in filters:
    #     filter.load(entries, regenerate=regenerate)


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
