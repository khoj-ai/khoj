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
from khoj.configure import configure_processor, configure_server
from khoj.search_type import image_search, text_search
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.utils.config import TextSearchModel
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import (
    ContentConfig,
    FullConfig,
    ProcessorConfig,
    SearchConfig,
    SearchResponse,
    TextContentConfig,
    OpenAIProcessorConfig,
    GithubContentConfig,
    NotionContentConfig,
    ConversationProcessorConfig,
)
from khoj.utils.helpers import resolve_absolute_path
from khoj.utils.state import SearchType
from khoj.utils import state, constants
from khoj.utils.yaml import save_config_to_file_updated_state
from fastapi.responses import StreamingResponse, Response
from khoj.routers.helpers import perform_chat_checks, generate_chat_response, update_telemetry_state
from khoj.processor.conversation.openai.gpt import extract_questions
from khoj.processor.conversation.gpt4all.chat_model import extract_questions_offline
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
        if state.processor_config is None:
            state.processor_config = configure_processor(state.config.processor)

    @api.get("/config/data", response_model=FullConfig)
    def get_config_data():
        return state.config

    @api.post("/config/data")
    async def set_config_data(
        request: Request,
        updated_config: FullConfig,
        client: Optional[str] = None,
    ):
        state.config = updated_config
        with open(state.config_file, "w") as outfile:
            yaml.dump(yaml.safe_load(state.config.json(by_alias=True)), outfile)
            outfile.close()

        configuration_update_metadata = dict()

        if state.config.content_type is not None:
            configuration_update_metadata["github"] = state.config.content_type.github is not None
            configuration_update_metadata["notion"] = state.config.content_type.notion is not None
            configuration_update_metadata["org"] = state.config.content_type.org is not None
            configuration_update_metadata["pdf"] = state.config.content_type.pdf is not None
            configuration_update_metadata["markdown"] = state.config.content_type.markdown is not None
            configuration_update_metadata["plugins"] = state.config.content_type.plugins is not None

        if state.config.processor is not None:
            configuration_update_metadata["conversation_processor"] = state.config.processor.conversation is not None

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_config",
            client=client,
            metadata=configuration_update_metadata,
        )
        return state.config

    @api.post("/config/data/content_type/github", status_code=200)
    async def set_content_config_github_data(
        request: Request,
        updated_config: Union[GithubContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        if not state.config.content_type:
            state.config.content_type = ContentConfig(**{"github": updated_config})
        else:
            state.config.content_type.github = updated_config

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_content_config",
            client=client,
            metadata={"content_type": "github"},
        )

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/content_type/notion", status_code=200)
    async def set_content_config_notion_data(
        request: Request,
        updated_config: Union[NotionContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        if not state.config.content_type:
            state.config.content_type = ContentConfig(**{"notion": updated_config})
        else:
            state.config.content_type.notion = updated_config

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_content_config",
            client=client,
            metadata={"content_type": "notion"},
        )

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/delete/config/data/content_type/{content_type}", status_code=200)
    async def remove_content_config_data(
        request: Request,
        content_type: str,
        client: Optional[str] = None,
    ):
        if not state.config or not state.config.content_type:
            return {"status": "ok"}

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="delete_content_config",
            client=client,
            metadata={"content_type": content_type},
        )

        if state.config.content_type:
            state.config.content_type[content_type] = None

        if content_type == "github":
            state.content_index.github = None
        elif content_type == "notion":
            state.content_index.notion = None
        elif content_type == "plugins":
            state.content_index.plugins = None
        elif content_type == "pdf":
            state.content_index.pdf = None
        elif content_type == "markdown":
            state.content_index.markdown = None
        elif content_type == "org":
            state.content_index.org = None

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/delete/config/data/processor/conversation/openai", status_code=200)
    async def remove_processor_conversation_config_data(
        request: Request,
        client: Optional[str] = None,
    ):
        if (
            not state.config
            or not state.config.processor
            or not state.config.processor.conversation
            or not state.config.processor.conversation.openai
        ):
            return {"status": "ok"}

        state.config.processor.conversation.openai = None
        state.processor_config = configure_processor(state.config.processor, state.processor_config)

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="delete_processor_openai_config",
            client=client,
            metadata={"processor_conversation_type": "openai"},
        )

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/content_type/{content_type}", status_code=200)
    async def set_content_config_data(
        request: Request,
        content_type: str,
        updated_config: Union[TextContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        if not state.config.content_type:
            state.config.content_type = ContentConfig(**{content_type: updated_config})
        else:
            state.config.content_type[content_type] = updated_config

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_content_config",
            client=client,
            metadata={"content_type": content_type},
        )

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/processor/conversation/openai", status_code=200)
    async def set_processor_openai_config_data(
        request: Request,
        updated_config: Union[OpenAIProcessorConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        if not state.config.processor or not state.config.processor.conversation:
            default_config = constants.default_config
            default_conversation_logfile = resolve_absolute_path(
                default_config["processor"]["conversation"]["conversation-logfile"]  # type: ignore
            )
            conversation_logfile = resolve_absolute_path(default_conversation_logfile)
            state.config.processor = ProcessorConfig(conversation=ConversationProcessorConfig(conversation_logfile=conversation_logfile))  # type: ignore

        assert state.config.processor.conversation is not None
        state.config.processor.conversation.openai = updated_config
        state.processor_config = configure_processor(state.config.processor, state.processor_config)

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_processor_config",
            client=client,
            metadata={"processor_conversation_type": "conversation"},
        )

        try:
            save_config_to_file_updated_state()
            return {"status": "ok"}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    @api.post("/config/data/processor/conversation/enable_offline_chat", status_code=200)
    async def set_processor_enable_offline_chat_config_data(
        request: Request,
        enable_offline_chat: bool,
        client: Optional[str] = None,
    ):
        _initialize_config()

        if not state.config.processor or not state.config.processor.conversation:
            default_config = constants.default_config
            default_conversation_logfile = resolve_absolute_path(
                default_config["processor"]["conversation"]["conversation-logfile"]  # type: ignore
            )
            conversation_logfile = resolve_absolute_path(default_conversation_logfile)
            state.config.processor = ProcessorConfig(conversation=ConversationProcessorConfig(conversation_logfile=conversation_logfile))  # type: ignore

        assert state.config.processor.conversation is not None
        state.config.processor.conversation.enable_offline_chat = enable_offline_chat
        state.processor_config = configure_processor(state.config.processor, state.processor_config)

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_processor_config",
            client=client,
            metadata={"processor_conversation_type": f"{'enable' if enable_offline_chat else 'disable'}_local_llm"},
        )

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
            and getattr(state.content_index, search_type.value) is not None
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
    if not state.search_models or not any(state.search_models.__dict__.values()):
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
            model for model in state.search_models.__dict__.values() if isinstance(model, TextSearchModel)
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
        if (t == SearchType.Org or t == SearchType.All) and state.content_index.org and state.search_models.text_search:
            # query org-mode notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.search_models.text_search,
                    state.content_index.org,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (
            (t == SearchType.Markdown or t == SearchType.All)
            and state.content_index.markdown
            and state.search_models.text_search
        ):
            # query markdown notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.search_models.text_search,
                    state.content_index.markdown,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (
            (t == SearchType.Github or t == SearchType.All)
            and state.content_index.github
            and state.search_models.text_search
        ):
            # query github issues
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.search_models.text_search,
                    state.content_index.github,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Pdf or t == SearchType.All) and state.content_index.pdf and state.search_models.text_search:
            # query pdf files
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.search_models.text_search,
                    state.content_index.pdf,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (t == SearchType.Image) and state.content_index.image and state.search_models.image_search:
            # query images
            search_futures += [
                executor.submit(
                    image_search.query,
                    user_query,
                    results_count,
                    state.search_models.image_search,
                    state.content_index.image,
                    score_threshold=score_threshold,
                )
            ]

        if (
            (t == SearchType.All or t in SearchType)
            and state.content_index.plugins
            and state.search_models.plugin_search
        ):
            # query specified plugin type
            # Get plugin content, search model for specified search type, or the first one if none specified
            plugin_search = state.search_models.plugin_search.get(t.value) or next(
                iter(state.search_models.plugin_search.values())
            )
            plugin_content = state.content_index.plugins.get(t.value) or next(
                iter(state.content_index.plugins.values())
            )
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    plugin_search,
                    plugin_content,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        if (
            (t == SearchType.Notion or t == SearchType.All)
            and state.content_index.notion
            and state.search_models.text_search
        ):
            # query notion pages
            search_futures += [
                executor.submit(
                    text_search.query,
                    user_query,
                    state.search_models.text_search,
                    state.content_index.notion,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        # Query across each requested content types in parallel
        with timer("Query took", logger):
            for search_future in concurrent.futures.as_completed(search_futures):
                if t == SearchType.Image and state.content_index.image:
                    hits = await search_future.result()
                    output_directory = constants.web_directory / "images"
                    # Collate results
                    results += image_search.collate_results(
                        hits,
                        image_names=state.content_index.image.image_names,
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

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="search",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

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
    if not state.config:
        error_msg = f"🚨 Khoj is not configured.\nConfigure it via http://localhost:42110/config, plugins or by editing {state.config_file}."
        logger.warning(error_msg)
        raise HTTPException(status_code=500, detail=error_msg)
    try:
        configure_server(state.config, regenerate=force or False, search_type=t)
    except Exception as e:
        error_msg = f"🚨 Failed to update server via API: {e}"
        logger.error(error_msg, exc_info=True)
        raise HTTPException(status_code=500, detail=error_msg)
    else:
        components = []
        if state.search_models:
            components.append("Search models")
        if state.content_index:
            components.append("Content index")
        if state.processor_config:
            components.append("Conversation processor")
        components_msg = ", ".join(components)
        logger.info(f"📬 {components_msg} updated via API")

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="update",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

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
    meta_log = {}
    if state.processor_config.conversation:
        meta_log = state.processor_config.conversation.meta_log

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

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

    # Get the (streamed) chat response from the LLM of choice.
    llm_response = generate_chat_response(
        q,
        meta_log=state.processor_config.conversation.meta_log,
        compiled_references=compiled_references,
        inferred_queries=inferred_queries,
    )

    if llm_response is None:
        return Response(content=llm_response, media_type="text/plain", status_code=500)

    if stream:
        return StreamingResponse(llm_response, media_type="text/event-stream", status_code=200)

    # Get the full response from the generator if the stream is not requested.
    aggregated_gpt_response = ""
    while True:
        try:
            aggregated_gpt_response += next(llm_response)
        except StopIteration:
            break

    actual_response = aggregated_gpt_response.split("### compiled references:")[0]

    response_obj = {"response": actual_response, "context": compiled_references}

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

    return Response(content=json.dumps(response_obj), media_type="application/json", status_code=200)


async def extract_references_and_questions(
    request: Request,
    q: str,
    n: int,
):
    # Load Conversation History
    meta_log = state.processor_config.conversation.meta_log

    # Initialize Variables
    conversation_type = "general" if q.startswith("@general") else "notes"
    compiled_references = []
    inferred_queries = []

    if conversation_type == "notes":
        # Infer search queries from user message
        with timer("Extracting search queries took", logger):
            if state.processor_config.conversation and state.processor_config.conversation.openai_model:
                api_key = state.processor_config.conversation.openai_model.api_key
                chat_model = state.processor_config.conversation.openai_model.chat_model
                inferred_queries = extract_questions(q, model=chat_model, api_key=api_key, conversation_log=meta_log)
            else:
                loaded_model = state.processor_config.conversation.gpt4all_model.loaded_model
                inferred_queries = extract_questions_offline(
                    q, loaded_model=loaded_model, conversation_log=meta_log, should_extract_questions=False
                )

        # Collate search results as context for GPT
        with timer("Searching knowledge base took", logger):
            result_list = []
            for query in inferred_queries:
                n_items = n if state.processor_config.conversation.openai_model else min(n, 3)
                result_list.extend(
                    await search(query, request=request, n=n_items, r=True, score_threshold=-5.0, dedupe=False)
                )
            compiled_references = [item.additional["compiled"] for item in result_list]

    return compiled_references, inferred_queries
