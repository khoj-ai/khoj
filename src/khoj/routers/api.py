import concurrent.futures
import json
import logging
import math
import os
import time
import uuid
from typing import Any, List, Optional, Union

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Depends, File, HTTPException, Request, UploadFile
from fastapi.requests import Request
from fastapi.responses import Response
from starlette.authentication import requires

from khoj.configure import initialize_content
from khoj.database.adapters import (
    ConversationAdapters,
    EntryAdapters,
    get_user_search_model_or_default,
)
from khoj.database.models import ChatModelOptions, KhojUser, SpeechToTextModelOptions
from khoj.processor.conversation.offline.chat_model import extract_questions_offline
from khoj.processor.conversation.offline.whisper import transcribe_audio_offline
from khoj.processor.conversation.openai.gpt import extract_questions
from khoj.processor.conversation.openai.whisper import transcribe_audio
from khoj.routers.helpers import (
    ApiUserRateLimiter,
    CommonQueryParams,
    ConversationCommandRateLimiter,
    update_telemetry_state,
)
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.search_type import text_search
from khoj.utils import constants, state
from khoj.utils.config import OfflineChatProcessorModel
from khoj.utils.helpers import ConversationCommand, timer
from khoj.utils.rawconfig import LocationData, SearchResponse
from khoj.utils.state import SearchType

# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)
conversation_command_rate_limiter = ConversationCommandRateLimiter(
    trial_rate_limit=2, subscribed_rate_limit=100, slug="command"
)


@api.get("/search", response_model=List[SearchResponse])
@requires(["authenticated"])
async def search(
    q: str,
    request: Request,
    common: CommonQueryParams,
    n: Optional[int] = 5,
    t: Optional[SearchType] = SearchType.All,
    r: Optional[bool] = False,
    max_distance: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
):
    user = request.user.object

    results = await execute_search(
        user=user,
        q=q,
        n=n,
        t=t,
        r=r,
        max_distance=max_distance,
        dedupe=dedupe,
    )

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="search",
        **common.__dict__,
    )

    return results


async def execute_search(
    user: KhojUser,
    q: str,
    n: Optional[int] = 5,
    t: Optional[SearchType] = SearchType.All,
    r: Optional[bool] = False,
    max_distance: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
):
    start_time = time.time()

    # Run validation checks
    results: List[SearchResponse] = []
    if q is None or q == "":
        logger.warning(f"No query param (q) passed in API call to initiate search")
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n or 5
    max_distance = max_distance or math.inf
    search_futures: List[concurrent.futures.Future] = []

    # return cached results, if available
    if user:
        query_cache_key = f"{user_query}-{n}-{t}-{r}-{max_distance}-{dedupe}"
        if query_cache_key in state.query_cache[user.uuid]:
            logger.debug(f"Return response from query cache")
            return state.query_cache[user.uuid][query_cache_key]

    # Encode query with filter terms removed
    defiltered_query = user_query
    for filter in [DateFilter(), WordFilter(), FileFilter()]:
        defiltered_query = filter.defilter(defiltered_query)

    encoded_asymmetric_query = None
    if t != SearchType.Image:
        with timer("Encoding query took", logger=logger):
            search_model = await sync_to_async(get_user_search_model_or_default)(user)
            encoded_asymmetric_query = state.embeddings_model[search_model.name].embed_query(defiltered_query)

    with concurrent.futures.ThreadPoolExecutor() as executor:
        if t in [
            SearchType.All,
            SearchType.Org,
            SearchType.Markdown,
            SearchType.Github,
            SearchType.Notion,
            SearchType.Plaintext,
            SearchType.Pdf,
        ]:
            # query markdown notes
            search_futures += [
                executor.submit(
                    text_search.query,
                    user,
                    user_query,
                    t,
                    question_embedding=encoded_asymmetric_query,
                    max_distance=max_distance,
                )
            ]

        # Query across each requested content types in parallel
        with timer("Query took", logger):
            for search_future in concurrent.futures.as_completed(search_futures):
                hits = await search_future.result()
                # Collate results
                results += text_search.collate_results(hits, dedupe=dedupe)

                # Sort results across all content types and take top results
                results = text_search.rerank_and_sort_results(
                    results, query=defiltered_query, rank_results=r, search_model_name=search_model.name
                )[:results_count]

    # Cache results
    if user:
        state.query_cache[user.uuid][query_cache_key] = results

    end_time = time.time()
    logger.debug(f"🔍 Search took: {end_time - start_time:.3f} seconds")

    return results


@api.get("/update")
@requires(["authenticated"])
def update(
    request: Request,
    common: CommonQueryParams,
    t: Optional[SearchType] = None,
    force: Optional[bool] = False,
):
    user = request.user.object
    if not state.config:
        error_msg = f"🚨 Khoj is not configured.\nConfigure it via http://localhost:42110/config, plugins or by editing {state.config_file}."
        logger.warning(error_msg)
        raise HTTPException(status_code=500, detail=error_msg)
    try:
        initialize_content(regenerate=force, search_type=t, init=False, user=user)
    except Exception as e:
        error_msg = f"🚨 Failed to update server via API: {e}"
        logger.error(error_msg, exc_info=True)
        raise HTTPException(status_code=500, detail=error_msg)
    else:
        components = []
        if state.search_models:
            components.append("Search models")
        components_msg = ", ".join(components)
        logger.info(f"📪 {components_msg} updated via API")

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="update",
        **common.__dict__,
    )

    return {"status": "ok", "message": "khoj reloaded"}


@api.post("/transcribe")
@requires(["authenticated"])
async def transcribe(
    request: Request,
    common: CommonQueryParams,
    file: UploadFile = File(...),
    rate_limiter_per_minute=Depends(
        ApiUserRateLimiter(requests=1, subscribed_requests=10, window=60, slug="transcribe_minute")
    ),
    rate_limiter_per_day=Depends(
        ApiUserRateLimiter(requests=10, subscribed_requests=600, window=60 * 60 * 24, slug="transcribe_day")
    ),
):
    user: KhojUser = request.user.object
    audio_filename = f"{user.uuid}-{str(uuid.uuid4())}.webm"
    user_message: str = None

    # If the file is too large, return an unprocessable entity error
    if file.size > 10 * 1024 * 1024:
        logger.warning(f"Audio file too large to transcribe. Audio file size: {file.size}. Exceeds 10Mb limit.")
        return Response(content="Audio size larger than 10Mb limit", status_code=422)

    # Transcribe the audio from the request
    try:
        # Store the audio from the request in a temporary file
        audio_data = await file.read()
        with open(audio_filename, "wb") as audio_file_writer:
            audio_file_writer.write(audio_data)
        audio_file = open(audio_filename, "rb")

        # Send the audio data to the Whisper API
        speech_to_text_config = await ConversationAdapters.get_speech_to_text_config()
        if not speech_to_text_config:
            # If the user has not configured a speech to text model, return an unsupported on server error
            status_code = 501
        elif state.openai_client and speech_to_text_config.model_type == SpeechToTextModelOptions.ModelType.OPENAI:
            speech2text_model = speech_to_text_config.model_name
            user_message = await transcribe_audio(audio_file, speech2text_model, client=state.openai_client)
        elif speech_to_text_config.model_type == SpeechToTextModelOptions.ModelType.OFFLINE:
            speech2text_model = speech_to_text_config.model_name
            user_message = await transcribe_audio_offline(audio_filename, speech2text_model)
    finally:
        # Close and Delete the temporary audio file
        audio_file.close()
        os.remove(audio_filename)

    if user_message is None:
        return Response(status_code=status_code or 500)

    update_telemetry_state(
        request=request,
        telemetry_type="api",
        api="transcribe",
        **common.__dict__,
    )

    # Return the spoken text
    content = json.dumps({"text": user_message})
    return Response(content=content, media_type="application/json", status_code=200)


async def extract_references_and_questions(
    request: Request,
    common: CommonQueryParams,
    meta_log: dict,
    q: str,
    n: int,
    d: float,
    conversation_commands: List[ConversationCommand] = [ConversationCommand.Default],
    location_data: LocationData = None,
):
    user = request.user.object if request.user.is_authenticated else None

    # Initialize Variables
    compiled_references: List[Any] = []
    inferred_queries: List[str] = []

    if (
        not ConversationCommand.Notes in conversation_commands
        and not ConversationCommand.Default in conversation_commands
    ):
        return compiled_references, inferred_queries, q

    if not await sync_to_async(EntryAdapters.user_has_entries)(user=user):
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
        offline_chat_config = await ConversationAdapters.aget_offline_chat_conversation_config()
        conversation_config = await ConversationAdapters.aget_conversation_config(user)
        if conversation_config is None:
            conversation_config = await ConversationAdapters.aget_default_conversation_config()
        if (
            offline_chat_config
            and offline_chat_config.enabled
            and conversation_config.model_type == ChatModelOptions.ModelType.OFFLINE
        ):
            using_offline_chat = True
            default_offline_llm = await ConversationAdapters.get_default_offline_llm()
            chat_model = default_offline_llm.chat_model
            if state.offline_chat_processor_config is None:
                state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model=chat_model)

            loaded_model = state.offline_chat_processor_config.loaded_model

            inferred_queries = extract_questions_offline(
                defiltered_query,
                loaded_model=loaded_model,
                conversation_log=meta_log,
                should_extract_questions=True,
                location_data=location_data,
            )
        elif conversation_config and conversation_config.model_type == ChatModelOptions.ModelType.OPENAI:
            openai_chat_config = await ConversationAdapters.get_openai_chat_config()
            default_openai_llm = await ConversationAdapters.aget_default_openai_llm()
            api_key = openai_chat_config.api_key
            chat_model = default_openai_llm.chat_model
            inferred_queries = extract_questions(
                defiltered_query,
                model=chat_model,
                api_key=api_key,
                conversation_log=meta_log,
                location_data=location_data,
            )

    # Collate search results as context for GPT
    with timer("Searching knowledge base took", logger):
        result_list = []
        logger.info(f"🔍 Searching knowledge base with queries: {inferred_queries}")
        for query in inferred_queries:
            n_items = min(n, 3) if using_offline_chat else n
            result_list.extend(
                await execute_search(
                    user,
                    f"{query} {filters_in_query}",
                    n=n_items,
                    t=SearchType.All,
                    r=True,
                    max_distance=d,
                    dedupe=False,
                )
            )
        result_list = text_search.deduplicated_search_responses(result_list)
        compiled_references = [item.additional["compiled"] for item in result_list]

    return compiled_references, inferred_queries, defiltered_query


@api.get("/health", response_class=Response)
@requires(["authenticated"], status_code=200)
def health_check(request: Request) -> Response:
    response_obj = {"email": request.user.object.email}
    return Response(content=json.dumps(response_obj), media_type="application/json", status_code=200)
