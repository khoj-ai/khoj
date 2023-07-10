# Standard Packages
import concurrent.futures
import math
import time
import yaml
import logging
import json
from typing import List, Optional, Union

# External Packages
from fastapi import APIRouter, HTTPException, Header, Request
from sentence_transformers import util

# Internal Packages
from khoj.configure import configure_processor, configure_search
from khoj.search_type import image_search, text_search
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.utils.config import TextSearchModel
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
    NotionContentConfig,
)
from khoj.utils.state import SearchType
from khoj.utils import state, constants
from khoj.utils.yaml import save_config_to_file_updated_state
from fastapi.responses import StreamingResponse, Response
from khoj.routers.helpers import perform_chat_checks, generate_chat_response
from khoj.processor.conversation.gpt import extract_questions
from fastapi.requests import Request


# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)

# If it's a demo instance, prevent updating any of the configuration.
if not state.demo:

    def _initialize_config():
        if state.config is None:
            state.config = FullConfig()
            state.config.search_type = SearchConfig.parse_obj(constants.default_config["search-type"])

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
    async def set_content_config_github_data(updated_config: Union[GithubContentConfig, None]):
        _initialize_config()

        if not state.config.content_type:
            state.config.content_type = ContentConfig(**{"github": updated_config})
        else:
            state.config.content_type.github = updated_config

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/content_type/notion", status_code=200)
    async def set_content_config_notion_data(updated_config: Union[NotionContentConfig, None]):
        _initialize_config()

        if not state.config.content_type:
            state.config.content_type = ContentConfig(**{"notion": updated_config})
        else:
            state.config.content_type.notion = updated_config

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/delete/config/data/content_type/{content_type}", status_code=200)
    async def remove_content_config_data(content_type: str):
        if not state.config or not state.config.content_type:
            return {"status": "ok"}

        if state.config.content_type:
            state.config.content_type[content_type] = None

        if content_type == "github":
            state.model.github_search = None
        elif content_type == "notion":
            state.model.notion_search = None
        elif content_type == "plugins":
            state.model.plugin_search = None
        elif content_type == "pdf":
            state.model.pdf_search = None
        elif content_type == "markdown":
            state.model.markdown_search = None
        elif content_type == "org":
            state.model.org_search = None

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/delete/config/data/processor/conversation", status_code=200)
    async def remove_processor_conversation_config_data():
        if not state.config or not state.config.processor or not state.config.processor.conversation:
            return {"status": "ok"}

        state.config.processor.conversation = None

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/content_type/{content_type}", status_code=200)
    async def set_content_config_data(content_type: str, updated_config: Union[TextContentConfig, None]):
        _initialize_config()

        if not state.config.content_type:
            state.config.content_type = ContentConfig(**{content_type: updated_config})
        else:
            state.config.content_type[content_type] = updated_config

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/processor/conversation", status_code=200)
    async def set_processor_conversation_config_data(updated_config: Union[ConversationProcessorConfig, None]):
        _initialize_config()

        state.config.processor = ProcessorConfig(conversation=updated_config)
        state.processor_config = configure_processor(state.config.processor)
        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}


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
        or search_type == SearchType.All
    ]


@api.get("/search", response_model=List[SearchResponse])
async def search(
    q: str,
    request: Request,
    n: Optional[int] = 5,
    t: Optional[SearchType] = SearchType.All,
    r: Optional[bool] = False,
    score_threshold: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    start_time = time.time()

    # Run validation checks
    results: List[SearchResponse] = []
    if q is None or q == "":
        logger.warning(f"No query param (q) passed in API call to initiate search")
        return results
    if not state.model or not any(state.model.__dict__.values()):
        logger.warning(f"No search models loaded. Configure a search model before initiating search")
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n or 5
    score_threshold = score_threshold if score_threshold is not None else -math.inf
    search_futures: List[concurrent.futures.Future] = []

    # return cached results, if available
    query_cache_key = f"{user_query}-{n}-{t}-{r}-{score_threshold}-{dedupe}"
    if query_cache_key in state.query_cache:
        logger.debug(f"Return response from query cache")
        return state.query_cache[query_cache_key]

    # Encode query with filter terms removed
    defiltered_query = user_query
    for filter in [DateFilter(), WordFilter(), FileFilter()]:
        defiltered_query = filter.defilter(user_query)

    encoded_asymmetric_query = None
    if t == SearchType.All or t != SearchType.Image:
        text_search_models: List[TextSearchModel] = [
            model for model in state.model.__dict__.values() if isinstance(model, TextSearchModel)
        ]
        if text_search_models:
            with timer("Encoding query took", logger=logger):
                encoded_asymmetric_query = util.normalize_embeddings(
                    text_search_models[0].bi_encoder.encode(
                        [defiltered_query],
                        convert_to_tensor=True,
                        device=state.device,
                    )
                )

    with concurrent.futures.ThreadPoolExecutor() as executor:
        if (t == SearchType.Org or t == SearchType.All) and state.model.org_search:
            # query org-mode notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.model.org_search,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Markdown or t == SearchType.All) and state.model.markdown_search:
            # query markdown notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.model.markdown_search,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Github or t == SearchType.All) and state.model.github_search:
            # query github issues
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.model.github_search,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Pdf or t == SearchType.All) and state.model.pdf_search:
            # query pdf files
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.model.pdf_search,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Image) and state.model.image_search:
            # query images
            search_futures += [
                executor.submit(
                    image_search.query,
                    user_query,
                    results_count,
                    state.model.image_search,
                    score_threshold=score_threshold,
                )
            ]

        if (t == SearchType.All or t in SearchType) and state.model.plugin_search:
            # query specified plugin type
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    # Get plugin search model for specified search type, or the first one if none specified
                    state.model.plugin_search.get(t.value) or next(iter(state.model.plugin_search.values())),
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Notion or t == SearchType.All) and state.model.notion_search:
            # query notion pages
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.model.notion_search,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        # Query across each requested content types in parallel
        with timer("Query took", logger):
            for search_future in concurrent.futures.as_completed(search_futures):
                if t == SearchType.Image:
                    hits = await search_future.result()
                    output_directory = constants.web_directory / "images"
                    # Collate results
                    results += image_search.collate_results(
                        hits,
                        image_names=state.model.image_search.image_names,
                        output_directory=output_directory,
                        image_files_url="/static/images",
                        count=results_count,
                    )
                else:
                    hits, entries = await search_future.result()
                    # Collate results
                    results += text_search.collate_results(hits, entries, results_count)

            # Sort results across all content types and take top results
            results = sorted(results, key=lambda x: float(x.score), reverse=True)[:results_count]

    # Cache results
    state.query_cache[query_cache_key] = results

    user_state = {
        "client_host": request.client.host if request.client else "unknown",
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
    }

    # Only log telemetry if query is new and not a continuation of previous query
    if state.previous_query is None or state.previous_query not in user_query:
        state.telemetry += [
            log_telemetry(
                telemetry_type="api", api="search", client=client, app_config=state.config.app, properties=user_state
            )
        ]
    state.previous_query = user_query

    end_time = time.time()
    logger.debug(f"🔍 Search took: {end_time - start_time:.3f} seconds")

    return results


@api.get("/update")
def update(
    request: Request,
    t: Optional[SearchType] = None,
    force: Optional[bool] = False,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    try:
        state.search_index_lock.acquire()
        try:
            state.model = configure_search(state.model, state.config, regenerate=force or False, t=t)
        except Exception as e:
            logger.error(e)
            raise HTTPException(status_code=500, detail=str(e))
        finally:
            state.search_index_lock.release()
    except ValueError as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))
    else:
        logger.info("📬 Search index updated via API")

    try:
        if state.config and state.config.processor:
            state.processor_config = configure_processor(state.config.processor)
    except ValueError as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))
    else:
        logger.info("📬 Processor reconfigured via API")

    user_state = {
        "client_host": request.client.host if request.client else None,
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
    }

    state.telemetry += [
        log_telemetry(
            telemetry_type="api", api="update", client=client, app_config=state.config.app, properties=user_state
        )
    ]

    return {"status": "ok", "message": "khoj reloaded"}


@api.get("/chat/history")
def chat_history(
    request: Request,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    perform_chat_checks()

    # Load Conversation History
    meta_log = state.processor_config.conversation.meta_log

    user_state = {
        "client_host": request.client.host if request.client else None,
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
    }

    state.telemetry += [
        log_telemetry(
            telemetry_type="api", api="chat", client=client, app_config=state.config.app, properties=user_state
        )
    ]

    return {"status": "ok", "response": meta_log.get("chat", [])}


@api.get("/chat", response_class=Response)
async def chat(
    request: Request,
    q: str,
    n: Optional[int] = 5,
    client: Optional[str] = None,
    stream: Optional[bool] = False,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
) -> Response:
    perform_chat_checks()
    compiled_references, inferred_queries = await extract_references_and_questions(request, q, (n or 5))

    # Get the (streamed) chat response from GPT.
    gpt_response = generate_chat_response(
        q,
        meta_log=state.processor_config.conversation.meta_log,
        compiled_references=compiled_references,
        inferred_queries=inferred_queries,
    )
    if gpt_response is None:
        return Response(content=gpt_response, media_type="text/plain", status_code=500)

    if stream:
        return StreamingResponse(gpt_response, media_type="text/event-stream", status_code=200)

    # Get the full response from the generator if the stream is not requested.
    aggregated_gpt_response = ""
    while True:
        try:
            aggregated_gpt_response += next(gpt_response)
        except StopIteration:
            break

    actual_response = aggregated_gpt_response.split("### compiled references:")[0]

    response_obj = {"response": actual_response, "context": compiled_references}

    user_state = {
        "client_host": request.client.host if request.client else None,
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
    }

    state.telemetry += [
        log_telemetry(
            telemetry_type="api", api="chat", client=client, app_config=state.config.app, properties=user_state
        )
    ]

    return Response(content=json.dumps(response_obj), media_type="application/json", status_code=200)


async def extract_references_and_questions(
    request: Request,
    q: str,
    n: int,
):
    # Load Conversation History
    meta_log = state.processor_config.conversation.meta_log

    # Initialize Variables
    api_key = state.processor_config.conversation.openai_api_key
    chat_model = state.processor_config.conversation.chat_model
    conversation_type = "general" if q.startswith("@general") else "notes"
    compiled_references = []
    inferred_queries = []

    if conversation_type == "notes":
        # Infer search queries from user message
        with timer("Extracting search queries took", logger):
            inferred_queries = extract_questions(q, model=chat_model, api_key=api_key, conversation_log=meta_log)

        # Collate search results as context for GPT
        with timer("Searching knowledge base took", logger):
            result_list = []
            for query in inferred_queries:
                result_list.extend(
                    await search(query, request=request, n=n, r=True, score_threshold=-5.0, dedupe=False)
                )
            compiled_references = [item.additional["compiled"] for item in result_list]

    return compiled_references, inferred_queries
