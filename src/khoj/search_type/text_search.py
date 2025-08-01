import logging
import math
from pathlib import Path
from typing import List, Optional, Tuple, Type, Union

import requests
import torch
from asgiref.sync import sync_to_async
from sentence_transformers import util

from khoj.database.adapters import EntryAdapters, get_default_search_model
from khoj.database.models import Agent, KhojUser
from khoj.database.models import Entry as DbEntry
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils import state
from khoj.utils.helpers import get_absolute_path, timer
from khoj.utils.jsonl import load_jsonl
from khoj.utils.models import BaseEncoder
from khoj.utils.rawconfig import Entry, SearchResponse
from khoj.utils.state import SearchType

logger = logging.getLogger(__name__)

search_type_to_embeddings_type = {
    SearchType.Org.value: DbEntry.EntryType.ORG,
    SearchType.Markdown.value: DbEntry.EntryType.MARKDOWN,
    SearchType.Plaintext.value: DbEntry.EntryType.PLAINTEXT,
    SearchType.Pdf.value: DbEntry.EntryType.PDF,
    SearchType.Github.value: DbEntry.EntryType.GITHUB,
    SearchType.Notion.value: DbEntry.EntryType.NOTION,
    SearchType.All.value: None,
}


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
    raw_query: str,
    user: KhojUser,
    type: SearchType = SearchType.All,
    question_embedding: Union[torch.Tensor, None] = None,
    max_distance: float = None,
    agent: Optional[Agent] = None,
) -> Tuple[List[dict], List[Entry]]:
    "Search for entries that answer the query"

    file_type = search_type_to_embeddings_type[type.value]

    query = raw_query
    search_model = await sync_to_async(get_default_search_model)()
    if not max_distance:
        if search_model.bi_encoder_confidence_threshold:
            max_distance = search_model.bi_encoder_confidence_threshold
        else:
            max_distance = math.inf

    # Encode the query using the bi-encoder
    if question_embedding is None:
        with timer("Query Encode Time", logger, state.device):
            question_embedding = state.embeddings_model[search_model.name].embed_query(query)

    # Find relevant entries for the query
    top_k = 10
    with timer("Search Time", logger, state.device):
        hits = EntryAdapters.search_with_embeddings(
            raw_query=raw_query,
            embeddings=question_embedding,
            max_results=top_k,
            file_type_filter=file_type,
            max_distance=max_distance,
            user=user,
            agent=agent,
        ).all()
        hits = await sync_to_async(list)(hits)  # type: ignore[call-arg]

    return hits


def collate_results(hits, dedupe=True):
    hit_ids = set()
    hit_hashes = set()
    for hit in hits:
        if dedupe and (hit.hashed_value in hit_hashes or hit.corpus_id in hit_ids):
            continue

        else:
            hit_hashes.add(hit.hashed_value)
            hit_ids.add(hit.corpus_id)
            yield SearchResponse.model_validate(
                {
                    "entry": hit.raw,
                    "score": hit.distance,
                    "corpus_id": str(hit.corpus_id),
                    "additional": {
                        "source": hit.file_source,
                        "file": hit.file_path,
                        "uri": hit.url,
                        "compiled": hit.compiled,
                        "heading": hit.heading,
                    },
                }
            )


def deduplicated_search_responses(hits: List[SearchResponse]):
    hit_ids = set()
    for hit in hits:
        if hit.additional["compiled"] in hit_ids:
            continue

        else:
            hit_ids.add(hit.additional["compiled"])
            yield SearchResponse.model_validate(
                {
                    "entry": hit.entry,
                    "score": hit.score,
                    "corpus_id": hit.corpus_id,
                    "additional": {
                        "source": hit.additional["source"],
                        "file": hit.additional["file"],
                        "uri": hit.additional["uri"],
                        "query": hit.additional["query"],
                        "compiled": hit.additional["compiled"],
                        "heading": hit.additional["heading"],
                    },
                }
            )


def rerank_and_sort_results(hits, query, rank_results, search_model_name):
    # Rerank results if explicitly requested, if can use inference server
    # AND if we have more than one result
    rank_results = (rank_results or state.cross_encoder_model[search_model_name].inference_server_enabled()) and len(
        list(hits)
    ) > 1

    # Score all retrieved entries using the cross-encoder
    if rank_results:
        hits = cross_encoder_score(query, hits, search_model_name)

    # Sort results by cross-encoder score followed by bi-encoder score
    hits = sort_results(rank_results=rank_results, hits=hits)

    return hits


def setup(
    text_to_entries: Type[TextToEntries],
    files: dict[str, str],
    regenerate: bool,
    user: KhojUser,
    config=None,
) -> Tuple[int, int]:
    if config:
        num_new_embeddings, num_deleted_embeddings = text_to_entries(config).process(
            files=files, user=user, regenerate=regenerate
        )
    else:
        num_new_embeddings, num_deleted_embeddings = text_to_entries().process(
            files=files, user=user, regenerate=regenerate
        )

    if files:
        file_names = [file_name for file_name in files]

        logger.info(
            f"Deleted {num_deleted_embeddings} entries. Created {num_new_embeddings} new entries for user {user} from files {file_names[:10]} ..."
        )

    return num_new_embeddings, num_deleted_embeddings


def cross_encoder_score(query: str, hits: List[SearchResponse], search_model_name: str) -> List[SearchResponse]:
    """Score all retrieved entries using the cross-encoder"""
    try:
        with timer("Cross-Encoder Predict Time", logger, state.device):
            cross_scores = state.cross_encoder_model[search_model_name].predict(query, hits)
    except requests.exceptions.HTTPError as e:
        logger.error(f"Failed to rerank documents using the inference endpoint. Error: {e}.", exc_info=True)
        cross_scores = [0.0] * len(hits)

    # Convert cross-encoder scores to distances and pass in hits for reranking
    for idx in range(len(cross_scores)):
        hits[idx]["cross_score"] = 1 - cross_scores[idx]

    return hits


def sort_results(rank_results: bool, hits: List[dict]) -> List[dict]:
    """Order results by cross-encoder score followed by bi-encoder score"""
    with timer("Rank Time", logger, state.device):
        hits.sort(key=lambda x: x["score"])  # sort by bi-encoder score
        if rank_results:
            hits.sort(key=lambda x: x["cross_score"])  # sort by cross-encoder score
    return hits
