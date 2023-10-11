# Standard Packages
import concurrent.futures
import math
import time
import logging
import json
from typing import List, Optional, Union, Any

# External Packages
from fastapi import APIRouter, HTTPException, Header, Request, Depends
from starlette.authentication import requires
from asgiref.sync import sync_to_async

# Internal Packages
from khoj.configure import configure_processor, configure_server
from khoj.search_type import image_search, text_search
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.utils.config import TextSearchModel
from khoj.utils.helpers import ConversationCommand, is_none_or_empty, timer, command_descriptions
from khoj.utils.rawconfig import (
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
from khoj.processor.embeddings import EmbeddingsModel
from fastapi.responses import StreamingResponse, Response
from khoj.routers.helpers import (
    get_conversation_command,
    perform_chat_checks,
    generate_chat_response,
    update_telemetry_state,
)
from khoj.processor.conversation.prompts import help_message
from khoj.processor.conversation.openai.gpt import extract_questions
from khoj.processor.conversation.gpt4all.chat_model import extract_questions_offline
from fastapi.requests import Request

from database import adapters
from database.adapters import EmbeddingsAdapters
from database.models import LocalMarkdownConfig, LocalOrgConfig, LocalPdfConfig, LocalPlaintextConfig, KhojUser


# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)
embeddings_model = EmbeddingsModel()


def map_config_to_object(content_type: str):
    if content_type == "org":
        return LocalOrgConfig
    if content_type == "markdown":
        return LocalMarkdownConfig
    if content_type == "pdf":
        return LocalPdfConfig
    if content_type == "plaintext":
        return LocalPlaintextConfig


def map_config_to_db(config: FullConfig, user: KhojUser):
    if config.content_type:
        if config.content_type.org:
            LocalOrgConfig.objects.filter(user=user).delete()
            LocalOrgConfig.objects.create(
                input_files=config.content_type.org.input_files,
                input_filter=config.content_type.org.input_filter,
                index_heading_entries=config.content_type.org.index_heading_entries,
                user=user,
            )
        if config.content_type.markdown:
            LocalMarkdownConfig.objects.filter(user=user).delete()
            LocalMarkdownConfig.objects.create(
                input_files=config.content_type.markdown.input_files,
                input_filter=config.content_type.markdown.input_filter,
                index_heading_entries=config.content_type.markdown.index_heading_entries,
                user=user,
            )
        if config.content_type.pdf:
            LocalPdfConfig.objects.filter(user=user).delete()
            LocalPdfConfig.objects.create(
                input_files=config.content_type.pdf.input_files,
                input_filter=config.content_type.pdf.input_filter,
                index_heading_entries=config.content_type.pdf.index_heading_entries,
                user=user,
            )
        if config.content_type.plaintext:
            LocalPlaintextConfig.objects.filter(user=user).delete()
            LocalPlaintextConfig.objects.create(
                input_files=config.content_type.plaintext.input_files,
                input_filter=config.content_type.plaintext.input_filter,
                index_heading_entries=config.content_type.plaintext.index_heading_entries,
                user=user,
            )
        if config.content_type.github:
            adapters.set_user_github_config(
                user=user,
                pat_token=config.content_type.github.pat_token,
                repos=config.content_type.github.repos,
            )
        if config.content_type.notion:
            adapters.set_notion_config(
                user=user,
                token=config.content_type.notion.token,
            )


# If it's a demo instance, prevent updating any of the configuration.
if not state.demo:

    def _initialize_config():
        if state.config is None:
            state.config = FullConfig()
            state.config.search_type = SearchConfig.parse_obj(constants.default_config["search-type"])
        if state.processor_config is None:
            state.processor_config = configure_processor(state.config.processor)

    @api.get("/config/data", response_model=FullConfig)
    def get_config_data(request: Request):
        user = request.user.object if request.user.is_authenticated else None
        enabled_content = EmbeddingsAdapters.get_unique_file_types(user)

        return state.config

    @api.post("/config/data")
    async def set_config_data(
        request: Request,
        updated_config: FullConfig,
        client: Optional[str] = None,
    ):
        user = request.user.object if request.user.is_authenticated else None
        map_config_to_db(updated_config, user)

        configuration_update_metadata = {}

        enabled_content = await sync_to_async(EmbeddingsAdapters.get_unique_file_types)(user)

        if state.config.content_type is not None:
            configuration_update_metadata["github"] = "github" in enabled_content
            configuration_update_metadata["notion"] = "notion" in enabled_content
            configuration_update_metadata["org"] = "org" in enabled_content
            configuration_update_metadata["pdf"] = "pdf" in enabled_content
            configuration_update_metadata["markdown"] = "markdown" in enabled_content

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
    @requires("authenticated")
    async def set_content_config_github_data(
        request: Request,
        updated_config: Union[GithubContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        user = request.user.object if request.user.is_authenticated else None

        await adapters.set_user_github_config(
            user=user,
            pat_token=updated_config.pat_token,
            repos=updated_config.repos,
        )

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_content_config",
            client=client,
            metadata={"content_type": "github"},
        )

        return {"status": "ok"}

    @api.post("/config/data/content_type/notion", status_code=200)
    async def set_content_config_notion_data(
        request: Request,
        updated_config: Union[NotionContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        user = request.user.object if request.user.is_authenticated else None

        await adapters.set_notion_config(
            user=user,
            token=updated_config.token,
        )

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_content_config",
            client=client,
            metadata={"content_type": "notion"},
        )

        return {"status": "ok"}

    @api.post("/delete/config/data/content_type/{content_type}", status_code=200)
    async def remove_content_config_data(
        request: Request,
        content_type: str,
        client: Optional[str] = None,
    ):
        user = request.user.object if request.user.is_authenticated else None
        if not state.config or not state.config.content_type:
            return {"status": "ok"}

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="delete_content_config",
            client=client,
            metadata={"content_type": content_type},
        )

        content_object = map_config_to_object(content_type)
        await content_object.objects.filter(user=user).adelete()

        enabled_content = await sync_to_async(EmbeddingsAdapters.get_unique_file_types)(user)

        # TODO: Add logic here to delete the embeddings for the content type
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

        return {"status": "ok"}

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
    # @requires("authenticated")
    async def set_content_config_data(
        request: Request,
        content_type: str,
        updated_config: Union[TextContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        user = request.user.object if request.user.is_authenticated else None

        content_object = map_config_to_object(content_type)
        await adapters.set_text_content_config(user, content_object, updated_config)

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_content_config",
            client=client,
            metadata={"content_type": content_type},
        )

        return {"status": "ok"}

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
def get_config_types(
    request: Request,
):
    user = request.user.object if request.user.is_authenticated else None

    enabled_file_types = EmbeddingsAdapters.get_unique_file_types(user)

    configured_content_types = list(enabled_file_types)

    if state.config and state.config.content_type:
        for ctype in state.config.content_type.dict(exclude_none=True):
            configured_content_types.append(ctype)

    return [
        search_type.value
        for search_type in SearchType
        if (search_type.value in configured_content_types) or search_type == SearchType.All
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
    user = request.user.object if request.user.is_authenticated else None
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
                encoded_asymmetric_query = embeddings_model.embed_query(defiltered_query)

    with concurrent.futures.ThreadPoolExecutor() as executor:
        if (
            t
            in [
                SearchType.All,
                SearchType.Org,
                SearchType.Markdown,
                SearchType.Github,
                SearchType.Notion,
                SearchType.Plaintext,
            ]
        ) and state.search_models.text_search:
            # query markdown notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user,
                    user_query,
                    state.search_models.text_search,
                    t,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
                    dedupe=dedupe or True,
                )
            ]

        elif (t == SearchType.Image) and state.content_index.image and state.search_models.image_search:
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
                    hits = await search_future.result()
                    # Collate results
                    results += text_search.collate_results(hits)

            # Sort results across all content types and take top results
            results = sorted(results, key=lambda x: float(x.score))[:results_count]

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
    user = request.user.object if request.user.is_authenticated else None
    if not state.config:
        error_msg = f"🚨 Khoj is not configured.\nConfigure it via http://localhost:42110/config, plugins or by editing {state.config_file}."
        logger.warning(error_msg)
        raise HTTPException(status_code=500, detail=error_msg)
    try:
        configure_server(state.config, regenerate=force, search_type=t, user=user)
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


@api.get("/chat/options", response_class=Response)
async def chat_options(
    request: Request,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
) -> Response:
    cmd_options = {}
    for cmd in ConversationCommand:
        cmd_options[cmd.value] = command_descriptions[cmd]

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="chat_options",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )
    return Response(content=json.dumps(cmd_options), media_type="application/json", status_code=200)


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
    conversation_command = get_conversation_command(query=q, any_references=True)
    compiled_references, inferred_queries, defiltered_query = await extract_references_and_questions(
        request, q, (n or 5), conversation_command
    )
    conversation_command = get_conversation_command(query=q, any_references=not is_none_or_empty(compiled_references))
    if conversation_command == ConversationCommand.Help:
        model_type = "offline" if state.processor_config.conversation.enable_offline_chat else "openai"
        formatted_help = help_message.format(model=model_type, version=state.khoj_version)
        return StreamingResponse(iter([formatted_help]), media_type="text/event-stream", status_code=200)

    # Get the (streamed) chat response from the LLM of choice.
    llm_response = generate_chat_response(
        defiltered_query,
        meta_log=state.processor_config.conversation.meta_log,
        compiled_references=compiled_references,
        inferred_queries=inferred_queries,
        conversation_command=conversation_command,
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
    conversation_type: ConversationCommand = ConversationCommand.Default,
):
    # Load Conversation History
    meta_log = state.processor_config.conversation.meta_log

    # Initialize Variables
    compiled_references: List[Any] = []
    inferred_queries: List[str] = []

    if state.content_index is None:
        logger.warning(
            "No content index loaded, so cannot extract references from knowledge base. Please configure your data sources and update the index to chat with your notes."
        )
        return compiled_references, inferred_queries

    if conversation_type == ConversationCommand.General:
        return compiled_references, inferred_queries, q

    # Extract filter terms from user message
    defiltered_query = q
    filter_terms = []
    for filter in [DateFilter(), WordFilter(), FileFilter()]:
        filter_terms += filter.get_filter_terms(q)
        defiltered_query = filter.defilter(q)
    filters_in_query = " ".join(filter_terms)

    # Infer search queries from user message
    with timer("Extracting search queries took", logger):
        # If we've reached here, either the user has enabled offline chat or the openai model is enabled.
        if state.processor_config.conversation.enable_offline_chat:
            loaded_model = state.processor_config.conversation.gpt4all_model.loaded_model
            inferred_queries = extract_questions_offline(
                defiltered_query, loaded_model=loaded_model, conversation_log=meta_log, should_extract_questions=False
            )
        elif state.processor_config.conversation.openai_model:
            api_key = state.processor_config.conversation.openai_model.api_key
            chat_model = state.processor_config.conversation.openai_model.chat_model
            inferred_queries = extract_questions(
                defiltered_query, model=chat_model, api_key=api_key, conversation_log=meta_log
            )

    # Collate search results as context for GPT
    with timer("Searching knowledge base took", logger):
        result_list = []
        for query in inferred_queries:
            n_items = min(n, 3) if state.processor_config.conversation.enable_offline_chat else n
            result_list.extend(
                await search(
                    f"{query} {filters_in_query}",
                    request=request,
                    n=n_items,
                    r=True,
                    score_threshold=-5.0,
                    dedupe=False,
                )
            )
        compiled_references = [item.additional["compiled"] for item in result_list]

    return compiled_references, inferred_queries, defiltered_query
