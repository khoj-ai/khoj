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
from khoj.utils.helpers import log_telemetry, timer
from khoj.utils.rawconfig import (
    ContentConfig,
    FullConfig,
    ProcessorConfig,
    SearchConfig,
    SearchResponse,
    TextContentConfig,
    ConversationProcessorConfig,
    GithubContentConfig,
)
from khoj.utils.state import SearchType
from khoj.utils import state, constants
from khoj.utils.yaml import save_config_to_file_updated_state

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
    if state.config is None or state.config.content_type is None:
        raise HTTPException(
            status_code=500,
            detail="Content types not configured. Configure at least one content type on server and restart it.",
        )

    configured_content_types = state.config.content_type.dict(exclude_none=True)
    return [
        search_type.value
        for search_type in SearchType
        if (
            search_type.value in configured_content_types
            and getattr(state.model, f"{search_type.value}_search") is not None
        )
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


@api.post("/config/data/content_type/github", status_code=200)
async def set_content_config_github_data(updated_config: GithubContentConfig):
    if not state.config:
        state.config = FullConfig()
        state.config.search_type = SearchConfig.parse_obj(constants.default_config["search-type"])
    state.config.content_type = ContentConfig(github=updated_config)
    try:
        save_config_to_file_updated_state()
        return {"status": "ok"}
    except Exception as e:
        return {"status": "error", "message": str(e)}


@api.post("/config/data/content_type/{content_type}", status_code=200)
async def set_content_config_data(content_type: str, updated_config: TextContentConfig):
    if not state.config:
        state.config = FullConfig()
        state.config.search_type = SearchConfig.parse_obj(constants.default_config["search-type"])
    state.config.content_type = ContentConfig(**{content_type: updated_config})
    try:
        save_config_to_file_updated_state()
        return {"status": "ok"}
    except Exception as e:
        return {"status": "error", "message": str(e)}


@api.post("/config/data/processor/conversation", status_code=200)
async def set_processor_conversation_config_data(updated_config: ConversationProcessorConfig):
    if not state.config:
        state.config = FullConfig()
        state.config.search_type = SearchConfig.parse_obj(constants.default_config["search-type"])
    state.config.processor = ProcessorConfig(conversation=updated_config)
    try:
        save_config_to_file_updated_state()
        return {"status": "ok"}
    except Exception as e:
        return {"status": "error", "message": str(e)}


@api.get("/search", response_model=List[SearchResponse])
def search(
    q: str,
    n: Optional[int] = 5,
    t: Optional[SearchType] = None,
    r: Optional[bool] = False,
    score_threshold: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
    client: Optional[str] = None,
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

    if (t == SearchType.Org or t == None) and state.model.org_search:
        # query org-mode notes
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.org_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
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

    elif (t == SearchType.Pdf or t == None) and state.model.pdf_search:
        # query pdf files
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.pdf_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
            )

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Github or t == None) and state.model.github_search:
        # query github embeddings
        with timer("Query took", logger):
            hits, entries = text_search.query(
                user_query, state.model.github_search, rank_results=r, score_threshold=score_threshold, dedupe=dedupe
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

    # Only log telemetry if query is new and not a continuation of previous query
    if state.previous_query is None or state.previous_query not in user_query:
        state.telemetry += [
            log_telemetry(telemetry_type="api", api="search", client=client, app_config=state.config.app)
        ]
    state.previous_query = user_query

    return results


@api.get("/update")
def update(t: Optional[SearchType] = None, force: Optional[bool] = False, client: Optional[str] = None):
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

    state.telemetry += [log_telemetry(telemetry_type="api", api="update", client=client, app_config=state.config.app)]

    return {"status": "ok", "message": "khoj reloaded"}


@api.get("/chat")
def chat(q: Optional[str] = None, client: Optional[str] = None):
    if (
        state.processor_config is None
        or state.processor_config.conversation is None
        or state.processor_config.conversation.openai_api_key is None
    ):
        raise HTTPException(
            status_code=500, detail="Chat processor not configured. Configure OpenAI API key on server and restart it."
        )

    # Load Conversation History
    chat_session = state.processor_config.conversation.chat_session
    meta_log = state.processor_config.conversation.meta_log

    # If user query is empty, return chat history
    if not q:
        return {"status": "ok", "response": meta_log.get("chat", [])}

    # Initialize Variables
    api_key = state.processor_config.conversation.openai_api_key
    model = state.processor_config.conversation.model
    chat_model = state.processor_config.conversation.chat_model
    user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    conversation_type = "general" if q.startswith("@general") else "notes"
    compiled_references = []
    inferred_queries = []

    if conversation_type == "notes":
        # Infer search queries from user message
        with timer("Extracting search queries took", logger):
            inferred_queries = extract_questions(q, model=model, api_key=api_key, conversation_log=meta_log)

        # Collate search results as context for GPT
        with timer("Searching knowledge base took", logger):
            result_list = []
            for query in inferred_queries:
                result_list.extend(search(query, n=5, r=True, score_threshold=-5.0, dedupe=False))
            compiled_references = [item.additional["compiled"] for item in result_list]

    # Switch to general conversation type if no relevant notes found for the given query
    conversation_type = "notes" if compiled_references else "general"
    logger.debug(f"Conversation Type: {conversation_type}")

    try:
        with timer("Generating chat response took", logger):
            gpt_response = converse(compiled_references, q, meta_log, model=chat_model, api_key=api_key)
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

    state.telemetry += [log_telemetry(telemetry_type="api", api="chat", client=client, app_config=state.config.app)]

    return {"status": status, "response": gpt_response, "context": compiled_references}
