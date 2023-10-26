# Standard Packages
import concurrent.futures
import math
import time
import logging
import json
from typing import List, Optional, Union, Any

# External Packages
from fastapi import APIRouter, HTTPException, Header, Request
from starlette.authentication import requires
from asgiref.sync import sync_to_async

# Internal Packages
from khoj.configure import configure_server
from khoj.search_type import image_search, text_search
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.utils.config import TextSearchModel, GPT4AllProcessorModel
from khoj.utils.helpers import ConversationCommand, is_none_or_empty, timer, command_descriptions
from khoj.utils.rawconfig import (
    FullConfig,
    SearchConfig,
    SearchResponse,
    TextContentConfig,
    OpenAIProcessorConfig,
    GithubContentConfig,
    NotionContentConfig,
    ConversationProcessorConfig,
    OfflineChatProcessorConfig,
)
from khoj.utils.state import SearchType
from khoj.utils import state, constants
from khoj.utils.helpers import AsyncIteratorWrapper
from fastapi.responses import StreamingResponse, Response
from khoj.routers.helpers import (
    get_conversation_command,
    perform_chat_checks,
    agenerate_chat_response,
    update_telemetry_state,
    is_ready_to_chat,
)
from khoj.processor.conversation.prompts import help_message
from khoj.processor.conversation.openai.gpt import extract_questions
from khoj.processor.conversation.gpt4all.chat_model import extract_questions_offline
from fastapi.requests import Request

from database import adapters
from database.adapters import EmbeddingsAdapters, ConversationAdapters
from database.models import LocalMarkdownConfig, LocalOrgConfig, LocalPdfConfig, LocalPlaintextConfig, KhojUser


# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)


def map_config_to_object(content_type: str):
    if content_type == "org":
        return LocalOrgConfig
    if content_type == "markdown":
        return LocalMarkdownConfig
    if content_type == "pdf":
        return LocalPdfConfig
    if content_type == "plaintext":
        return LocalPlaintextConfig


async def map_config_to_db(config: FullConfig, user: KhojUser):
    if config.content_type:
        if config.content_type.org:
            await LocalOrgConfig.objects.filter(user=user).adelete()
            await LocalOrgConfig.objects.acreate(
                input_files=config.content_type.org.input_files,
                input_filter=config.content_type.org.input_filter,
                index_heading_entries=config.content_type.org.index_heading_entries,
                user=user,
            )
        if config.content_type.markdown:
            await LocalMarkdownConfig.objects.filter(user=user).adelete()
            await LocalMarkdownConfig.objects.acreate(
                input_files=config.content_type.markdown.input_files,
                input_filter=config.content_type.markdown.input_filter,
                index_heading_entries=config.content_type.markdown.index_heading_entries,
                user=user,
            )
        if config.content_type.pdf:
            await LocalPdfConfig.objects.filter(user=user).adelete()
            await LocalPdfConfig.objects.acreate(
                input_files=config.content_type.pdf.input_files,
                input_filter=config.content_type.pdf.input_filter,
                index_heading_entries=config.content_type.pdf.index_heading_entries,
                user=user,
            )
        if config.content_type.plaintext:
            await LocalPlaintextConfig.objects.filter(user=user).adelete()
            await LocalPlaintextConfig.objects.acreate(
                input_files=config.content_type.plaintext.input_files,
                input_filter=config.content_type.plaintext.input_filter,
                index_heading_entries=config.content_type.plaintext.index_heading_entries,
                user=user,
            )
        if config.content_type.github:
            await adapters.set_user_github_config(
                user=user,
                pat_token=config.content_type.github.pat_token,
                repos=config.content_type.github.repos,
            )
        if config.content_type.notion:
            await adapters.set_notion_config(
                user=user,
                token=config.content_type.notion.token,
            )
    if config.processor and config.processor.conversation:
        ConversationAdapters.set_conversation_processor_config(user, config.processor.conversation)


# If it's a demo instance, prevent updating any of the configuration.
if not state.demo:

    def _initialize_config():
        if state.config is None:
            state.config = FullConfig()
            state.config.search_type = SearchConfig.parse_obj(constants.default_config["search-type"])

    @api.get("/config/data", response_model=FullConfig)
    @requires(["authenticated"])
    def get_config_data(request: Request):
        user = request.user.object
        EmbeddingsAdapters.get_unique_file_types(user)

        return state.config

    @api.post("/config/data")
    @requires(["authenticated"])
    async def set_config_data(
        request: Request,
        updated_config: FullConfig,
        client: Optional[str] = None,
    ):
        user = request.user.object
        await map_config_to_db(updated_config, user)

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
    @requires(["authenticated"])
    async def set_content_config_github_data(
        request: Request,
        updated_config: Union[GithubContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        user = request.user.object

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
    @requires(["authenticated"])
    async def set_content_config_notion_data(
        request: Request,
        updated_config: Union[NotionContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        user = request.user.object

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
    @requires(["authenticated"])
    async def remove_content_config_data(
        request: Request,
        content_type: str,
        client: Optional[str] = None,
    ):
        user = request.user.object

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="delete_content_config",
            client=client,
            metadata={"content_type": content_type},
        )

        content_object = map_config_to_object(content_type)
        if content_object is None:
            raise ValueError(f"Invalid content type: {content_type}")

        await content_object.objects.filter(user=user).adelete()
        await sync_to_async(EmbeddingsAdapters.delete_all_embeddings)(user, content_type)

        enabled_content = await sync_to_async(EmbeddingsAdapters.get_unique_file_types)(user)
        return {"status": "ok"}

    @api.post("/delete/config/data/processor/conversation/openai", status_code=200)
    @requires(["authenticated"])
    async def remove_processor_conversation_config_data(
        request: Request,
        client: Optional[str] = None,
    ):
        user = request.user.object

        await sync_to_async(ConversationAdapters.clear_openai_conversation_config)(user)

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="delete_processor_openai_config",
            client=client,
            metadata={"processor_conversation_type": "openai"},
        )

        return {"status": "ok"}

    @api.post("/config/data/content_type/{content_type}", status_code=200)
    @requires(["authenticated"])
    async def set_content_config_data(
        request: Request,
        content_type: str,
        updated_config: Union[TextContentConfig, None],
        client: Optional[str] = None,
    ):
        _initialize_config()

        user = request.user.object

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
    @requires(["authenticated"])
    async def set_processor_openai_config_data(
        request: Request,
        updated_config: Union[OpenAIProcessorConfig, None],
        client: Optional[str] = None,
    ):
        user = request.user.object

        conversation_config = ConversationProcessorConfig(openai=updated_config)

        await sync_to_async(ConversationAdapters.set_conversation_processor_config)(user, conversation_config)

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_processor_config",
            client=client,
            metadata={"processor_conversation_type": "conversation"},
        )

        return {"status": "ok"}

    @api.post("/config/data/processor/conversation/offline_chat", status_code=200)
    @requires(["authenticated"])
    async def set_processor_enable_offline_chat_config_data(
        request: Request,
        enable_offline_chat: bool,
        offline_chat_model: Optional[str] = None,
        client: Optional[str] = None,
    ):
        user = request.user.object

        try:
            if enable_offline_chat:
                conversation_config = ConversationProcessorConfig(
                    offline_chat=OfflineChatProcessorConfig(
                        enable_offline_chat=enable_offline_chat,
                        chat_model=offline_chat_model,
                    )
                )

                await sync_to_async(ConversationAdapters.set_conversation_processor_config)(user, conversation_config)

                offline_chat = await ConversationAdapters.get_offline_chat(user)
                chat_model = offline_chat.chat_model
                if state.gpt4all_processor_config is None:
                    state.gpt4all_processor_config = GPT4AllProcessorModel(chat_model=chat_model)

            else:
                await sync_to_async(ConversationAdapters.clear_offline_chat_conversation_config)(user)
                state.gpt4all_processor_config = None

        except Exception as e:
            logger.error(f"Error updating offline chat config: {e}", exc_info=True)
            return {"status": "error", "message": str(e)}

        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="set_processor_config",
            client=client,
            metadata={"processor_conversation_type": f"{'enable' if enable_offline_chat else 'disable'}_local_llm"},
        )

        return {"status": "ok"}


# Create Routes
@api.get("/config/data/default")
def get_default_config_data():
    return constants.empty_config


@api.get("/config/types", response_model=List[str])
@requires(["authenticated"])
def get_config_types(
    request: Request,
):
    user = request.user.object

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
@requires(["authenticated"])
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
    user = request.user.object
    start_time = time.time()

    # Run validation checks
    results: List[SearchResponse] = []
    if q is None or q == "":
        logger.warning(f"No query param (q) passed in API call to initiate search")
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n or 5
    score_threshold = score_threshold if score_threshold is not None else -math.inf
    search_futures: List[concurrent.futures.Future] = []

    # return cached results, if available
    if user:
        query_cache_key = f"{user_query}-{n}-{t}-{r}-{score_threshold}-{dedupe}"
        if query_cache_key in state.query_cache[user.uuid]:
            logger.debug(f"Return response from query cache")
            return state.query_cache[user.uuid][query_cache_key]

    # Encode query with filter terms removed
    defiltered_query = user_query
    for filter in [DateFilter(), WordFilter(), FileFilter()]:
        defiltered_query = filter.defilter(defiltered_query)

    encoded_asymmetric_query = None
    if t == SearchType.All or t != SearchType.Image:
        text_search_models: List[TextSearchModel] = [
            model for model in state.search_models.__dict__.values() if isinstance(model, TextSearchModel)
        ]
        if text_search_models:
            with timer("Encoding query took", logger=logger):
                encoded_asymmetric_query = state.embeddings_model.embed_query(defiltered_query)

    with concurrent.futures.ThreadPoolExecutor() as executor:
        if t in [
            SearchType.All,
            SearchType.Org,
            SearchType.Markdown,
            SearchType.Github,
            SearchType.Notion,
            SearchType.Plaintext,
        ]:
            # query markdown notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user,
                    user_query,
                    t,
                    question_embedding=encoded_asymmetric_query,
                    rank_results=r or False,
                    score_threshold=score_threshold,
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
                    results += text_search.collate_results(hits, dedupe=dedupe)

            if r:
                results = text_search.rerank_and_sort_results(results, query=defiltered_query)[:results_count]
            else:
                # Sort results across all content types and take top results
                results = sorted(results, key=lambda x: float(x.score))[:results_count]

    # Cache results
    if user:
        state.query_cache[user.uuid][query_cache_key] = results

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="search",
        client=client,
        user_agent=user_agent,
        referer=referer,
        host=host,
    )

    end_time = time.time()
    logger.debug(f"🔍 Search took: {end_time - start_time:.3f} seconds")

    return results


@api.get("/update")
@requires(["authenticated"])
def update(
    request: Request,
    t: Optional[SearchType] = None,
    force: Optional[bool] = False,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    user = request.user.object
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
        components_msg = ", ".join(components)
        logger.info(f"📪 {components_msg} updated via API")

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
@requires(["authenticated"])
def chat_history(
    request: Request,
    client: Optional[str] = None,
    user_agent: Optional[str] = Header(None),
    referer: Optional[str] = Header(None),
    host: Optional[str] = Header(None),
):
    user = request.user.object
    perform_chat_checks(user)

    # Load Conversation History
    meta_log = ConversationAdapters.get_conversation_by_user(user=user).conversation_log

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
@requires(["authenticated"])
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
@requires(["authenticated"])
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
    user = request.user.object

    await is_ready_to_chat(user)
    conversation_command = get_conversation_command(query=q, any_references=True)

    q = q.replace(f"/{conversation_command.value}", "").strip()

    meta_log = (await ConversationAdapters.aget_conversation_by_user(user)).conversation_log

    compiled_references, inferred_queries, defiltered_query = await extract_references_and_questions(
        request, meta_log, q, (n or 5), conversation_command
    )

    if conversation_command == ConversationCommand.Default and is_none_or_empty(compiled_references):
        conversation_command = ConversationCommand.General

    if conversation_command == ConversationCommand.Help:
        model_type = "offline" if await ConversationAdapters.has_offline_chat(user) else "openai"
        formatted_help = help_message.format(model=model_type, version=state.khoj_version)
        return StreamingResponse(iter([formatted_help]), media_type="text/event-stream", status_code=200)

    # Get the (streamed) chat response from the LLM of choice.
    llm_response = await agenerate_chat_response(
        defiltered_query,
        meta_log,
        compiled_references,
        inferred_queries,
        conversation_command,
        user,
    )

    if llm_response is None:
        return Response(content=llm_response, media_type="text/plain", status_code=500)

    if stream:
        return StreamingResponse(llm_response, media_type="text/event-stream", status_code=200)

    iterator = AsyncIteratorWrapper(llm_response)

    # Get the full response from the generator if the stream is not requested.
    aggregated_gpt_response = ""
    async for item in iterator:
        if item is None:
            break
        aggregated_gpt_response += item

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
    meta_log: dict,
    q: str,
    n: int,
    conversation_type: ConversationCommand = ConversationCommand.Default,
):
    user = request.user.object if request.user.is_authenticated else None

    # Initialize Variables
    compiled_references: List[Any] = []
    inferred_queries: List[str] = []

    if conversation_type == ConversationCommand.General:
        return compiled_references, inferred_queries, q

    if not await EmbeddingsAdapters.user_has_embeddings(user=user):
        logger.warning(
            "No content index loaded, so cannot extract references from knowledge base. Please configure your data sources and update the index to chat with your notes."
        )
        return compiled_references, inferred_queries, q

    # Extract filter terms from user message
    defiltered_query = q
    for filter in [DateFilter(), WordFilter(), FileFilter()]:
        defiltered_query = filter.defilter(defiltered_query)
    filters_in_query = q.replace(defiltered_query, "").strip()

    using_offline_chat = False

    # Infer search queries from user message
    with timer("Extracting search queries took", logger):
        # If we've reached here, either the user has enabled offline chat or the openai model is enabled.
        if await ConversationAdapters.has_offline_chat(user):
            using_offline_chat = True
            offline_chat = await ConversationAdapters.get_offline_chat(user)
            chat_model = offline_chat.chat_model
            if state.gpt4all_processor_config is None:
                state.gpt4all_processor_config = GPT4AllProcessorModel(chat_model=chat_model)

            loaded_model = state.gpt4all_processor_config.loaded_model

            inferred_queries = extract_questions_offline(
                defiltered_query, loaded_model=loaded_model, conversation_log=meta_log, should_extract_questions=False
            )
        elif await ConversationAdapters.has_openai_chat(user):
            openai_chat = await ConversationAdapters.get_openai_chat(user)
            api_key = openai_chat.api_key
            chat_model = openai_chat.chat_model
            inferred_queries = extract_questions(
                defiltered_query, model=chat_model, api_key=api_key, conversation_log=meta_log
            )

    # Collate search results as context for GPT
    with timer("Searching knowledge base took", logger):
        result_list = []
        for query in inferred_queries:
            n_items = min(n, 3) if using_offline_chat else n
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
        # Dedupe the results again, as duplicates may be returned across queries.
        result_list = text_search.deduplicated_search_responses(result_list)
        compiled_references = [item.additional["compiled"] for item in result_list]

    return compiled_references, inferred_queries, defiltered_query
