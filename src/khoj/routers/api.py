# Standard Packages
import math
import yaml
import logging
from datetime import datetime
from typing import List, Optional, Union

# External Packages
from fastapi import APIRouter
from fastapi import HTTPException

# Internal Packages
from khoj.configure import configure_processor, configure_search
from khoj.processor.conversation.gpt import converse, extract_questions
from khoj.processor.conversation.utils import message_to_log, message_to_prompt
from khoj.search_type import image_search, text_search
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import FullConfig, SearchResponse
from khoj.utils.state import SearchType
from khoj.utils import state, constants

# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)


# Create Routes
@api.get("/config/data/default")
def get_default_config_data():
    return constants.default_config


@api.get("/config/types", response_model=List[str])
def get_config_types():
    """Get configured content types"""
    configured_content_types = state.config.content_type.dict(exclude_none=True)
    return [
        search_type.value
        for search_type in SearchType
        if search_type.value in configured_content_types
        or ("plugins" in configured_content_types and search_type.name in configured_content_types["plugins"])
    ]


@api.get("/config/data", response_model=FullConfig)
def get_config_data():
    return state.config


@api.post("/config/data")
async def set_config_data(updated_config: FullConfig):
    state.config = updated_config
    with open(state.config_file, "w") as outfile:
        yaml.dump(yaml.safe_load(state.config.json(by_alias=True)), outfile)
        outfile.close()
    return state.config


@api.get("/search", response_model=List[SearchResponse])
def search(
    q: str,
    n: Optional[int] = 5,
    t: Optional[SearchType] = None,
    r: Optional[bool] = False,
    score_threshold: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
):
    results: List[SearchResponse] = []
    if q is None or q == "":
        logger.warn(f"No query param (q) passed in API call to initiate search")
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n
    score_threshold = score_threshold if score_threshold is not None else -math.inf

    # return cached results, if available
    query_cache_key = f"{user_query}-{n}-{t}-{r}-{score_threshold}-{dedupe}"
    if query_cache_key in state.query_cache:
        logger.debug(f"Return response from query cache")
        return state.query_cache[query_cache_key]

    if (t == SearchType.Org or t == None) and state.model.orgmode_search:
        # query org-mode notes
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.orgmode_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
            )

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Markdown or t == None) and state.model.markdown_search:
        # query markdown files
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.markdown_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
            )

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Ledger or t == None) and state.model.ledger_search:
        # query transactions
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.ledger_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
            )

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Music or t == None) and state.model.music_search:
        # query music library
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.music_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
            )

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Image or t == None) and state.model.image_search:
        # query images
        with timer("Query took", logger):
            hits = image_search.query(
                user_query, results_count, state.model.image_search, score_threshold=score_threshold
            )
            output_directory = constants.web_directory / "images"

        # collate and return results
        with timer("Collating results took", logger):
            results = image_search.collate_results(
                hits,
                image_names=state.model.image_search.image_names,
                output_directory=output_directory,
                image_files_url="/static/images",
                count=results_count,
            )

    elif (t in SearchType or t == None) and state.model.plugin_search:
        # query specified plugin type
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query,
                # Get plugin search model for specified search type, or the first one if none specified
                state.model.plugin_search.get(t.value) or next(iter(state.model.plugin_search.values())),
                rank_results=r,
                score_threshold=score_threshold,
                dedupe=dedupe,
            )

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    # Cache results
    state.query_cache[query_cache_key] = results

    return results


@api.get("/update")
def update(t: Optional[SearchType] = None, force: Optional[bool] = False):
    try:
        state.search_index_lock.acquire()
        state.model = configure_search(state.model, state.config, regenerate=force, t=t)
        state.search_index_lock.release()
    except ValueError as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))
    else:
        logger.info("📬 Search index updated via API")

    try:
        state.processor_config = configure_processor(state.config.processor)
    except ValueError as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))
    else:
        logger.info("📬 Processor reconfigured via API")

    return {"status": "ok", "message": "khoj reloaded"}


@api.get("/chat")
def chat(q: Optional[str] = None):
    # Initialize Variables
    api_key = state.processor_config.conversation.openai_api_key
    model = state.processor_config.conversation.model
    user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Load Conversation History
    chat_session = state.processor_config.conversation.chat_session
    meta_log = state.processor_config.conversation.meta_log

    # If user query is empty, return chat history
    if not q:
        if meta_log.get("chat"):
            return {"status": "ok", "response": meta_log["chat"]}
        else:
            return {"status": "ok", "response": []}

    # Infer search queries from user message
    with timer("Extracting search queries took", logger):
        inferred_queries = extract_questions(q, model=model, api_key=api_key, conversation_log=meta_log)

    # Collate search results as context for GPT
    with timer("Searching knowledge base took", logger):
        result_list = []
        for query in inferred_queries:
            result_list.extend(search(query, n=5, r=True, score_threshold=-5.0, dedupe=False))
        compiled_references = [item.additional["compiled"] for item in result_list]

    try:
        with timer("Generating chat response took", logger):
            gpt_response = converse(compiled_references, q, meta_log, api_key=api_key)
        status = "ok"
    except Exception as e:
        gpt_response = str(e)
        status = "error"

    # Update Conversation History
    state.processor_config.conversation.chat_session = message_to_prompt(q, chat_session, gpt_message=gpt_response)
    state.processor_config.conversation.meta_log["chat"] = message_to_log(
        q,
        gpt_response,
        user_message_metadata={"created": user_message_time},
        khoj_message_metadata={"context": compiled_references, "intent": {"inferred-queries": inferred_queries}},
        conversation_log=meta_log.get("chat", []),
    )

    return {"status": status, "response": gpt_response, "context": compiled_references}
