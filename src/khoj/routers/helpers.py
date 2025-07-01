import asyncio
import base64
import concurrent.futures
import hashlib
import json
import logging
import math
import os
import re
import time
from datetime import datetime, timedelta, timezone
from random import random
from typing import (
    Annotated,
    Any,
    AsyncGenerator,
    Callable,
    Dict,
    List,
    Optional,
    Set,
    Tuple,
    Union,
)
from urllib.parse import parse_qs, quote, unquote, urljoin, urlparse

import cron_descriptor
import pyjson5
import pytz
import requests
from apscheduler.job import Job
from apscheduler.triggers.cron import CronTrigger
from asgiref.sync import sync_to_async
from django.utils import timezone as django_timezone
from fastapi import Depends, Header, HTTPException, Request, UploadFile
from pydantic import BaseModel, EmailStr, Field
from starlette.authentication import has_required_scope
from starlette.requests import URL

from khoj.database import adapters
from khoj.database.adapters import (
    LENGTH_OF_FREE_TRIAL,
    AgentAdapters,
    AutomationAdapters,
    ConversationAdapters,
    EntryAdapters,
    FileObjectAdapters,
    aget_user_by_email,
    ais_user_subscribed,
    create_khoj_token,
    get_default_search_model,
    get_khoj_tokens,
    get_user_name,
    get_user_notion_config,
    get_user_subscription_state,
    is_user_subscribed,
    run_with_process_lock,
)
from khoj.database.models import (
    Agent,
    ChatMessageModel,
    ChatModel,
    ClientApplication,
    Conversation,
    GithubConfig,
    KhojUser,
    NotionConfig,
    ProcessLock,
    RateLimitRecord,
    Subscription,
    TextToImageModelConfig,
    UserRequests,
)
from khoj.processor.content.docx.docx_to_entries import DocxToEntries
from khoj.processor.content.github.github_to_entries import GithubToEntries
from khoj.processor.content.images.image_to_entries import ImageToEntries
from khoj.processor.content.markdown.markdown_to_entries import MarkdownToEntries
from khoj.processor.content.notion.notion_to_entries import NotionToEntries
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.pdf.pdf_to_entries import PdfToEntries
from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.processor.conversation import prompts
from khoj.processor.conversation.anthropic.anthropic_chat import (
    anthropic_send_message_to_model,
    converse_anthropic,
)
from khoj.processor.conversation.google.gemini_chat import (
    converse_gemini,
    gemini_send_message_to_model,
)
from khoj.processor.conversation.offline.chat_model import (
    converse_offline,
    send_message_to_model_offline,
)
from khoj.processor.conversation.openai.gpt import (
    converse_openai,
    send_message_to_model,
)
from khoj.processor.conversation.utils import (
    ChatEvent,
    OperatorRun,
    ResearchIteration,
    ResponseWithThought,
    clean_json,
    clean_mermaidjs,
    construct_chat_history,
    construct_question_history,
    defilter_query,
    generate_chatml_messages_with_context,
)
from khoj.processor.speech.text_to_speech import is_eleven_labs_enabled
from khoj.routers.email import is_resend_enabled, send_task_email
from khoj.routers.twilio import is_twilio_enabled
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.search_type import text_search
from khoj.utils import state
from khoj.utils.config import OfflineChatProcessorModel
from khoj.utils.helpers import (
    LRU,
    ConversationCommand,
    get_file_type,
    in_debug_mode,
    is_none_or_empty,
    is_operator_enabled,
    is_valid_url,
    log_telemetry,
    mode_descriptions_for_llm,
    timer,
    tool_descriptions_for_llm,
)
from khoj.utils.rawconfig import (
    ChatRequestBody,
    FileAttachment,
    FileData,
    LocationData,
    SearchResponse,
)
from khoj.utils.state import SearchType

logger = logging.getLogger(__name__)


NOTION_OAUTH_CLIENT_ID = os.getenv("NOTION_OAUTH_CLIENT_ID")
NOTION_OAUTH_CLIENT_SECRET = os.getenv("NOTION_OAUTH_CLIENT_SECRET")
NOTION_REDIRECT_URI = os.getenv("NOTION_REDIRECT_URI")


def is_query_empty(query: str) -> bool:
    return is_none_or_empty(query.strip())


def validate_chat_model(user: KhojUser):
    default_chat_model = ConversationAdapters.get_default_chat_model(user)

    if default_chat_model is None:
        raise HTTPException(status_code=500, detail="Contact the server administrator to add a chat model.")

    if default_chat_model.model_type == "openai" and not default_chat_model.ai_model_api:
        raise HTTPException(status_code=500, detail="Contact the server administrator to add a chat model.")


async def is_ready_to_chat(user: KhojUser):
    user_chat_model = await ConversationAdapters.aget_user_chat_model(user)
    if user_chat_model == None:
        user_chat_model = await ConversationAdapters.aget_default_chat_model(user)

    if user_chat_model and user_chat_model.model_type == ChatModel.ModelType.OFFLINE:
        chat_model_name = user_chat_model.name
        max_tokens = user_chat_model.max_prompt_size
        if state.offline_chat_processor_config is None:
            logger.info("Loading Offline Chat Model...")
            state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model_name, max_tokens)
        return True

    if (
        user_chat_model
        and (
            user_chat_model.model_type
            in [
                ChatModel.ModelType.OPENAI,
                ChatModel.ModelType.ANTHROPIC,
                ChatModel.ModelType.GOOGLE,
            ]
        )
        and user_chat_model.ai_model_api
    ):
        return True

    raise HTTPException(status_code=500, detail="Set your OpenAI API key or enable Local LLM via Khoj settings.")


def get_file_content(file: UploadFile):
    file_content = file.file.read()
    file_type, encoding = get_file_type(file.content_type, file_content)
    return FileData(name=file.filename, content=file_content, file_type=file_type, encoding=encoding)


def update_telemetry_state(
    request: Request,
    telemetry_type: str,
    api: str,
    client: Optional[str] = None,
    user_agent: Optional[str] = None,
    referer: Optional[str] = None,
    host: Optional[str] = None,
    metadata: Optional[dict] = None,
):
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    client_app: ClientApplication = request.user.client_app if request.user.is_authenticated else None
    subscription: Subscription = user.subscription if user and hasattr(user, "subscription") else None
    user_state = {
        "client_host": request.client.host if request.client else None,
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
        "server_id": str(user.uuid) if user else None,
        "subscription_type": subscription.type if subscription else None,
        "is_recurring": subscription.is_recurring if subscription else None,
        "client_id": str(client_app.name) if client_app else "default",
    }

    if metadata:
        user_state.update(metadata)

    state.telemetry += [
        log_telemetry(
            telemetry_type=telemetry_type,
            api=api,
            client=client,
            app_config=state.config.app,
            disable_telemetry_env=state.telemetry_disabled,
            properties=user_state,
        )
    ]


def get_next_url(request: Request) -> str:
    "Construct next url relative to current domain from request"
    next_url_param = urlparse(request.query_params.get("next", "/"))
    next_path = "/"  # default next path
    # If relative path or absolute path to current domain
    if is_none_or_empty(next_url_param.scheme) or next_url_param.netloc == request.base_url.netloc:
        # Use path in next query param
        next_path = next_url_param.path
    # Construct absolute url using current domain and next path from request
    return urljoin(str(request.base_url).rstrip("/"), next_path)


def get_conversation_command(query: str) -> ConversationCommand:
    if query.startswith("/notes"):
        return ConversationCommand.Notes
    elif query.startswith("/general"):
        return ConversationCommand.General
    elif query.startswith("/online"):
        return ConversationCommand.Online
    elif query.startswith("/webpage"):
        return ConversationCommand.Webpage
    elif query.startswith("/image"):
        return ConversationCommand.Image
    elif query.startswith("/automated_task"):
        return ConversationCommand.AutomatedTask
    elif query.startswith("/diagram"):
        return ConversationCommand.Diagram
    elif query.startswith("/code"):
        return ConversationCommand.Code
    elif query.startswith("/research"):
        return ConversationCommand.Research
    elif query.startswith("/operator") and is_operator_enabled():
        return ConversationCommand.Operator
    else:
        return ConversationCommand.Default


def gather_raw_query_files(
    query_files: Dict[str, str],
):
    """
    Gather contextual data from the given (raw) files
    """

    if len(query_files) == 0:
        return ""

    contextual_data = " ".join(
        [f"File: {file_name}\n\n{file_content}\n\n" for file_name, file_content in query_files.items()]
    )
    return f"I have attached the following files:\n\n{contextual_data}"


async def acreate_title_from_history(
    user: KhojUser,
    conversation: Conversation,
):
    """
    Create a title from the given conversation history
    """
    chat_history = construct_chat_history(conversation.messages)

    title_generation_prompt = prompts.conversation_title_generation.format(chat_history=chat_history)

    with timer("Chat actor: Generate title from conversation history", logger):
        response = await send_message_to_model_wrapper(title_generation_prompt, user=user)

    return response.strip()


async def acreate_title_from_query(query: str, user: KhojUser = None) -> str:
    """
    Create a title from the given query
    """
    title_generation_prompt = prompts.subject_generation.format(query=query)

    with timer("Chat actor: Generate title from query", logger):
        response = await send_message_to_model_wrapper(title_generation_prompt, user=user)

    return response.strip()


async def acheck_if_safe_prompt(system_prompt: str, user: KhojUser = None, lax: bool = False) -> Tuple[bool, str]:
    """
    Check if the system prompt is safe to use
    """
    safe_prompt_check = (
        prompts.personality_prompt_safety_expert.format(prompt=system_prompt)
        if not lax
        else prompts.personality_prompt_safety_expert_lax.format(prompt=system_prompt)
    )
    is_safe = True
    reason = ""

    class SafetyCheck(BaseModel):
        safe: bool
        reason: str

    with timer("Chat actor: Check if safe prompt", logger):
        response = await send_message_to_model_wrapper(
            safe_prompt_check, user=user, response_type="json_object", response_schema=SafetyCheck
        )

        response = response.strip()
        try:
            response = json.loads(clean_json(response))
            is_safe = str(response.get("safe", "true")).lower() == "true"
            if not is_safe:
                reason = response.get("reason", "")
        except Exception:
            logger.error(f"Invalid response for checking safe prompt: {response}")

    if not is_safe:
        logger.error(f"Unsafe prompt: {system_prompt}. Reason: {reason}")

    return is_safe, reason


async def aget_data_sources_and_output_format(
    query: str,
    chat_history: list[ChatMessageModel],
    is_task: bool,
    user: KhojUser,
    query_images: List[str] = None,
    agent: Agent = None,
    query_files: str = None,
    tracer: dict = {},
) -> Dict[str, Any]:
    """
    Given a query, determine which of the available data sources and output modes the agent should use to answer appropriately.
    """

    source_options = dict()
    source_options_str = ""

    agent_sources = agent.input_tools if agent else []
    user_has_entries = await EntryAdapters.auser_has_entries(user)

    for source, description in tool_descriptions_for_llm.items():
        # Skip showing Notes tool as an option if user has no entries
        if source == ConversationCommand.Notes and not user_has_entries:
            continue
        if source == ConversationCommand.Operator and not is_operator_enabled():
            continue
        source_options[source.value] = description
        if len(agent_sources) == 0 or source.value in agent_sources:
            source_options_str += f'- "{source.value}": "{description}"\n'

    output_options = dict()
    output_options_str = ""

    agent_outputs = agent.output_modes if agent else []

    for output, description in mode_descriptions_for_llm.items():
        output_options[output.value] = description
        if len(agent_outputs) == 0 or output.value in agent_outputs:
            output_options_str += f'- "{output.value}": "{description}"\n'

    chat_history_str = construct_chat_history(chat_history, n=6)

    if query_images:
        query = f"[placeholder for {len(query_images)} user attached images]\n{query}"

    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    relevant_tools_prompt = prompts.pick_relevant_tools.format(
        query=query,
        sources=source_options_str,
        outputs=output_options_str,
        chat_history=chat_history_str,
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    class PickTools(BaseModel):
        source: List[str] = Field(..., min_items=1)
        output: str

    with timer("Chat actor: Infer information sources to refer", logger):
        response = await send_message_to_model_wrapper(
            relevant_tools_prompt,
            response_type="json_object",
            response_schema=PickTools,
            user=user,
            query_files=query_files,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )

    try:
        response = clean_json(response)
        response = json.loads(response)

        chosen_sources = [s.strip() for s in response.get("source", []) if s.strip()]
        chosen_output = response.get("output", "text").strip()  # Default to text output

        if is_none_or_empty(chosen_sources) or not isinstance(chosen_sources, list):
            raise ValueError(
                f"Invalid response for determining relevant tools: {chosen_sources}. Raw Response: {response}"
            )

        output_mode = ConversationCommand.Text
        # Verify selected output mode is enabled for the agent, as the LLM can sometimes get confused by the tool options.
        if chosen_output in output_options.keys() and (len(agent_outputs) == 0 or chosen_output in agent_outputs):
            # Ensure that the chosen output mode exists as a valid ConversationCommand
            output_mode = ConversationCommand(chosen_output)

        data_sources = []
        # Verify selected data sources are enabled for the agent, as the LLM can sometimes get confused by the tool options.
        for chosen_source in chosen_sources:
            # Ensure that the chosen data source exists as a valid ConversationCommand
            if chosen_source in source_options.keys() and (len(agent_sources) == 0 or chosen_source in agent_sources):
                data_sources.append(ConversationCommand(chosen_source))

        # Fallback to default sources if the inferred data sources are unset or invalid
        if is_none_or_empty(data_sources):
            if len(agent_sources) == 0:
                data_sources = [ConversationCommand.Default]
            else:
                data_sources = [ConversationCommand.General]
    except Exception as e:
        logger.error(f"Invalid response for determining relevant tools: {response}. Error: {e}", exc_info=True)
        data_sources = agent_sources if len(agent_sources) > 0 else [ConversationCommand.Default]
        output_mode = agent_outputs[0] if len(agent_outputs) > 0 else ConversationCommand.Text

    return {"sources": data_sources, "output": output_mode}


async def infer_webpage_urls(
    q: str,
    max_webpages: int,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    user: KhojUser,
    query_images: List[str] = None,
    agent: Agent = None,
    query_files: str = None,
    tracer: dict = {},
) -> List[str]:
    """
    Infer webpage links from the given query
    """
    location = f"{location_data}" if location_data else "Unknown"
    username = prompts.user_name.format(name=user.get_full_name()) if user.get_full_name() else ""
    chat_history_str = construct_chat_history(chat_history)

    utc_date = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    online_queries_prompt = prompts.infer_webpages_to_read.format(
        query=q,
        max_webpages=max_webpages,
        chat_history=chat_history_str,
        current_date=utc_date,
        location=location,
        username=username,
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    class WebpageUrls(BaseModel):
        links: List[str] = Field(..., min_items=1, max_items=max_webpages)

    with timer("Chat actor: Infer webpage urls to read", logger):
        response = await send_message_to_model_wrapper(
            online_queries_prompt,
            query_images=query_images,
            response_type="json_object",
            response_schema=WebpageUrls,
            user=user,
            query_files=query_files,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )

    # Validate that the response is a non-empty, JSON-serializable list of URLs
    try:
        response = clean_json(response)
        urls = json.loads(response)
        valid_unique_urls = {str(url).strip() for url in urls["links"] if is_valid_url(url)}
        if is_none_or_empty(valid_unique_urls):
            raise ValueError(f"Invalid list of urls: {response}")
        if len(valid_unique_urls) == 0:
            logger.error(f"No valid URLs found in response: {response}")
            return []
        return list(valid_unique_urls)[:max_webpages]
    except Exception:
        raise ValueError(f"Invalid list of urls: {response}")


async def generate_online_subqueries(
    q: str,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    user: KhojUser,
    query_images: List[str] = None,
    query_files: str = None,
    max_queries: int = 3,
    agent: Agent = None,
    tracer: dict = {},
) -> Set[str]:
    """
    Generate subqueries from the given query
    """
    location = f"{location_data}" if location_data else "Unknown"
    username = prompts.user_name.format(name=user.get_full_name()) if user.get_full_name() else ""
    chat_history_str = construct_chat_history(chat_history)

    utc_date = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    online_queries_prompt = prompts.online_search_conversation_subqueries.format(
        query=q,
        chat_history=chat_history_str,
        max_queries=max_queries,
        current_date=utc_date,
        location=location,
        username=username,
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    class OnlineQueries(BaseModel):
        queries: List[str] = Field(..., min_items=1, max_items=max_queries)

    with timer("Chat actor: Generate online search subqueries", logger):
        response = await send_message_to_model_wrapper(
            online_queries_prompt,
            query_images=query_images,
            response_type="json_object",
            response_schema=OnlineQueries,
            user=user,
            query_files=query_files,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )

    # Validate that the response is a non-empty, JSON-serializable list
    try:
        response = clean_json(response)
        response = pyjson5.loads(response)
        response = {q.strip() for q in response["queries"] if q.strip()}
        if not isinstance(response, set) or not response or len(response) == 0:
            logger.error(
                f"Invalid response for constructing online subqueries: {response}. Returning original query: {q}"
            )
            return {q}
        return response
    except Exception as e:
        logger.error(f"Invalid response for constructing online subqueries: {response}. Returning original query: {q}")
        return {q}


def schedule_query(
    q: str, chat_history: List[ChatMessageModel], user: KhojUser, query_images: List[str] = None, tracer: dict = {}
) -> Tuple[str, str, str]:
    """
    Schedule the date, time to run the query. Assume the server timezone is UTC.
    """
    chat_history_str = construct_chat_history(chat_history)

    crontime_prompt = prompts.crontime_prompt.format(
        query=q,
        chat_history=chat_history_str,
    )

    raw_response = send_message_to_model_wrapper_sync(
        crontime_prompt, query_images=query_images, response_type="json_object", user=user, tracer=tracer
    )

    # Validate that the response is a non-empty, JSON-serializable list
    try:
        raw_response = raw_response.strip()
        response: Dict[str, str] = json.loads(clean_json(raw_response))
        if not response or not isinstance(response, Dict) or len(response) != 3:
            raise AssertionError(f"Invalid response for scheduling query : {response}")
        return response.get("crontime"), response.get("query"), response.get("subject")
    except Exception:
        raise AssertionError(f"Invalid response for scheduling query: {raw_response}")


async def aschedule_query(
    q: str, chat_history: List[ChatMessageModel], user: KhojUser, query_images: List[str] = None, tracer: dict = {}
) -> Tuple[str, str, str]:
    """
    Schedule the date, time to run the query. Assume the server timezone is UTC.
    """
    chat_history_str = construct_chat_history(chat_history)

    crontime_prompt = prompts.crontime_prompt.format(
        query=q,
        chat_history=chat_history_str,
    )

    raw_response = await send_message_to_model_wrapper(
        crontime_prompt, query_images=query_images, response_type="json_object", user=user, tracer=tracer
    )

    # Validate that the response is a non-empty, JSON-serializable list
    try:
        raw_response = raw_response.strip()
        response: Dict[str, str] = json.loads(clean_json(raw_response))
        if not response or not isinstance(response, Dict) or len(response) != 3:
            raise AssertionError(f"Invalid response for scheduling query : {response}")
        return response.get("crontime"), response.get("query"), response.get("subject")
    except Exception:
        raise AssertionError(f"Invalid response for scheduling query: {raw_response}")


async def extract_relevant_info(
    qs: set[str], corpus: str, user: KhojUser = None, agent: Agent = None, tracer: dict = {}
) -> Union[str, None]:
    """
    Extract relevant information for a given query from the target corpus
    """

    if is_none_or_empty(corpus) or is_none_or_empty(qs):
        return None

    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    extract_relevant_information = prompts.extract_relevant_information.format(
        query=", ".join(qs),
        corpus=corpus.strip(),
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    response = await send_message_to_model_wrapper(
        extract_relevant_information,
        prompts.system_prompt_extract_relevant_information,
        user=user,
        agent_chat_model=agent_chat_model,
        tracer=tracer,
    )
    return response.strip()


async def extract_relevant_summary(
    q: str,
    corpus: str,
    chat_history: List[ChatMessageModel] = [],
    query_images: List[str] = None,
    user: KhojUser = None,
    agent: Agent = None,
    tracer: dict = {},
) -> Union[str, None]:
    """
    Extract relevant information for a given query from the target corpus
    """

    if is_none_or_empty(corpus) or is_none_or_empty(q):
        return None

    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    chat_history_str = construct_chat_history(chat_history)

    extract_relevant_information = prompts.extract_relevant_summary.format(
        query=q,
        chat_history=chat_history_str,
        corpus=corpus.strip(),
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    with timer("Chat actor: Extract relevant information from data", logger):
        response = await send_message_to_model_wrapper(
            extract_relevant_information,
            prompts.system_prompt_extract_relevant_summary,
            user=user,
            query_images=query_images,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )
    return response.strip()


async def generate_summary_from_files(
    q: str,
    user: KhojUser,
    file_filters: List[str],
    chat_history: List[ChatMessageModel] = [],
    query_images: List[str] = None,
    agent: Agent = None,
    send_status_func: Optional[Callable] = None,
    query_files: str = None,
    tracer: dict = {},
):
    try:
        file_objects = None
        if await EntryAdapters.aagent_has_entries(agent):
            file_names = await EntryAdapters.aget_agent_entry_filepaths(agent)
            if len(file_names) > 0:
                file_objects = await FileObjectAdapters.aget_file_objects_by_name(None, file_names.pop(), agent)

        if (file_objects and len(file_objects) == 0 and not query_files) or (not file_objects and not query_files):
            response_log = "Sorry, I couldn't find anything to summarize."
            yield response_log
            return

        contextual_data = " ".join([f"File: {file.file_name}\n\n{file.raw_text}" for file in file_objects])

        if query_files:
            contextual_data += f"\n\n{query_files}"

        if not q:
            q = "Create a general summary of the file"

        file_names = [file.file_name for file in file_objects]
        file_names.extend(file_filters)

        all_file_names = ""

        for file_name in file_names:
            all_file_names += f"- {file_name}\n"

        async for result in send_status_func(f"**Constructing Summary Using:**\n{all_file_names}"):
            yield {ChatEvent.STATUS: result}

        response = await extract_relevant_summary(
            q,
            contextual_data,
            chat_history=chat_history,
            query_images=query_images,
            user=user,
            agent=agent,
            tracer=tracer,
        )

        yield str(response)
    except Exception as e:
        response_log = "Error summarizing file. Please try again, or contact support."
        logger.error(f"Error summarizing file for {user.email}: {e}", exc_info=True)
        yield result


async def generate_excalidraw_diagram(
    q: str,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    note_references: List[Dict[str, Any]],
    online_results: Optional[dict] = None,
    query_images: List[str] = None,
    user: KhojUser = None,
    agent: Agent = None,
    send_status_func: Optional[Callable] = None,
    query_files: str = None,
    tracer: dict = {},
):
    if send_status_func:
        async for event in send_status_func("**Enhancing the Diagramming Prompt**"):
            yield {ChatEvent.STATUS: event}

    better_diagram_description_prompt = await generate_better_diagram_description(
        q=q,
        chat_history=chat_history,
        location_data=location_data,
        note_references=note_references,
        online_results=online_results,
        query_images=query_images,
        user=user,
        agent=agent,
        query_files=query_files,
        tracer=tracer,
    )

    if send_status_func:
        async for event in send_status_func(f"**Diagram to Create:**:\n{better_diagram_description_prompt}"):
            yield {ChatEvent.STATUS: event}
    try:
        excalidraw_diagram_description = await generate_excalidraw_diagram_from_description(
            q=better_diagram_description_prompt,
            user=user,
            agent=agent,
            tracer=tracer,
        )
    except Exception as e:
        logger.error(f"Error generating Excalidraw diagram for {user.email}: {e}", exc_info=True)
        yield better_diagram_description_prompt, None
        return

    scratchpad = excalidraw_diagram_description.get("scratchpad")

    inferred_queries = f"Instruction: {better_diagram_description_prompt}\n\nScratchpad: {scratchpad}"

    yield inferred_queries, excalidraw_diagram_description.get("elements")


async def generate_better_diagram_description(
    q: str,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    note_references: List[Dict[str, Any]],
    online_results: Optional[dict] = None,
    query_images: List[str] = None,
    user: KhojUser = None,
    agent: Agent = None,
    query_files: str = None,
    tracer: dict = {},
) -> str:
    """
    Generate a diagram description from the given query and context
    """

    today_date = datetime.now(tz=timezone.utc).strftime("%Y-%m-%d, %A")
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    location = f"{location_data}" if location_data else "Unknown"

    user_references = "\n\n".join([f"# {item['compiled']}" for item in note_references])

    chat_history_str = construct_chat_history(chat_history)

    simplified_online_results = {}

    if online_results:
        for result in online_results:
            if online_results[result].get("answerBox"):
                simplified_online_results[result] = online_results[result]["answerBox"]
            elif online_results[result].get("webpages"):
                simplified_online_results[result] = online_results[result]["webpages"]

    improve_diagram_description_prompt = prompts.improve_excalidraw_diagram_description_prompt.format(
        query=q,
        chat_history=chat_history_str,
        location=location,
        current_date=today_date,
        references=user_references,
        online_results=simplified_online_results,
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    with timer("Chat actor: Generate better diagram description", logger):
        response = await send_message_to_model_wrapper(
            improve_diagram_description_prompt,
            query_images=query_images,
            user=user,
            query_files=query_files,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )
        response = response.strip()
        if response.startswith(('"', "'")) and response.endswith(('"', "'")):
            response = response[1:-1]

    return response


async def generate_excalidraw_diagram_from_description(
    q: str,
    user: KhojUser = None,
    agent: Agent = None,
    tracer: dict = {},
) -> Dict[str, Any]:
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    excalidraw_diagram_generation = prompts.excalidraw_diagram_generation_prompt.format(
        personality_context=personality_context,
        query=q,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    with timer("Chat actor: Generate excalidraw diagram", logger):
        raw_response = await send_message_to_model_wrapper(
            query=excalidraw_diagram_generation, user=user, agent_chat_model=agent_chat_model, tracer=tracer
        )
        raw_response = clean_json(raw_response)
        try:
            # Expect response to have `elements` and `scratchpad` keys
            response: Dict[str, str] = json.loads(raw_response)
            if (
                not response
                or not isinstance(response, Dict)
                or not response.get("elements")
                or not response.get("scratchpad")
            ):
                raise AssertionError(f"Invalid response for generating Excalidraw diagram: {response}")
        except Exception:
            raise AssertionError(f"Invalid response for generating Excalidraw diagram: {raw_response}")
        if not response or not isinstance(response["elements"], List) or not isinstance(response["elements"][0], Dict):
            # TODO Some additional validation here that it's a valid Excalidraw diagram
            raise AssertionError(f"Invalid response for improving diagram description: {response}")

    return response


async def generate_mermaidjs_diagram(
    q: str,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    note_references: List[Dict[str, Any]],
    online_results: Optional[dict] = None,
    query_images: List[str] = None,
    user: KhojUser = None,
    agent: Agent = None,
    send_status_func: Optional[Callable] = None,
    query_files: str = None,
    tracer: dict = {},
):
    if send_status_func:
        async for event in send_status_func("**Enhancing the Diagramming Prompt**"):
            yield {ChatEvent.STATUS: event}

    better_diagram_description_prompt = await generate_better_mermaidjs_diagram_description(
        q=q,
        chat_history=chat_history,
        location_data=location_data,
        note_references=note_references,
        online_results=online_results,
        query_images=query_images,
        user=user,
        agent=agent,
        query_files=query_files,
        tracer=tracer,
    )

    if send_status_func:
        async for event in send_status_func(f"**Diagram to Create:**:\n{better_diagram_description_prompt}"):
            yield {ChatEvent.STATUS: event}

    mermaidjs_diagram_description = await generate_mermaidjs_diagram_from_description(
        q=better_diagram_description_prompt,
        user=user,
        agent=agent,
        tracer=tracer,
    )

    inferred_queries = f"Instruction: {better_diagram_description_prompt}"

    yield inferred_queries, mermaidjs_diagram_description


async def generate_better_mermaidjs_diagram_description(
    q: str,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    note_references: List[Dict[str, Any]],
    online_results: Optional[dict] = None,
    query_images: List[str] = None,
    user: KhojUser = None,
    agent: Agent = None,
    query_files: str = None,
    tracer: dict = {},
) -> str:
    """
    Generate a diagram description from the given query and context
    """

    today_date = datetime.now(tz=timezone.utc).strftime("%Y-%m-%d, %A")
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    location = f"{location_data}" if location_data else "Unknown"

    user_references = "\n\n".join([f"# {item['compiled']}" for item in note_references])

    chat_history_str = construct_chat_history(chat_history)

    simplified_online_results = {}

    if online_results:
        for result in online_results:
            if online_results[result].get("answerBox"):
                simplified_online_results[result] = online_results[result]["answerBox"]
            elif online_results[result].get("webpages"):
                simplified_online_results[result] = online_results[result]["webpages"]

    improve_diagram_description_prompt = prompts.improve_mermaid_js_diagram_description_prompt.format(
        query=q,
        chat_history=chat_history_str,
        location=location,
        current_date=today_date,
        references=user_references,
        online_results=simplified_online_results,
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    with timer("Chat actor: Generate better Mermaid.js diagram description", logger):
        response = await send_message_to_model_wrapper(
            improve_diagram_description_prompt,
            query_images=query_images,
            user=user,
            query_files=query_files,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )
        response = response.strip()
        if response.startswith(('"', "'")) and response.endswith(('"', "'")):
            response = response[1:-1]

    return response


async def generate_mermaidjs_diagram_from_description(
    q: str,
    user: KhojUser = None,
    agent: Agent = None,
    tracer: dict = {},
) -> str:
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    mermaidjs_diagram_generation = prompts.mermaid_js_diagram_generation_prompt.format(
        personality_context=personality_context,
        query=q,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    with timer("Chat actor: Generate Mermaid.js diagram", logger):
        raw_response = await send_message_to_model_wrapper(
            query=mermaidjs_diagram_generation, user=user, agent_chat_model=agent_chat_model, tracer=tracer
        )
        return clean_mermaidjs(raw_response.strip())


async def generate_better_image_prompt(
    q: str,
    conversation_history: str,
    location_data: LocationData,
    note_references: List[Dict[str, Any]],
    online_results: Optional[dict] = None,
    model_type: Optional[str] = None,
    query_images: Optional[List[str]] = None,
    user: KhojUser = None,
    agent: Agent = None,
    query_files: str = "",
    tracer: dict = {},
) -> str:
    """
    Generate a better image prompt from the given query
    """

    today_date = datetime.now(tz=timezone.utc).strftime("%Y-%m-%d, %A")
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )
    model_type = model_type or TextToImageModelConfig.ModelType.OPENAI

    location = f"{location_data}" if location_data else "Unknown"

    user_references = "\n\n".join([f"# {item['compiled']}" for item in note_references])

    simplified_online_results = {}

    if online_results:
        for result in online_results:
            if online_results[result].get("answerBox"):
                simplified_online_results[result] = online_results[result]["answerBox"]
            elif online_results[result].get("webpages"):
                simplified_online_results[result] = online_results[result]["webpages"]

    if model_type == TextToImageModelConfig.ModelType.OPENAI:
        image_prompt = prompts.image_generation_improve_prompt_dalle.format(
            query=q,
            chat_history=conversation_history,
            location=location,
            current_date=today_date,
            references=user_references,
            online_results=simplified_online_results,
            personality_context=personality_context,
        )
    elif model_type in [
        TextToImageModelConfig.ModelType.STABILITYAI,
        TextToImageModelConfig.ModelType.REPLICATE,
        TextToImageModelConfig.ModelType.GOOGLE,
    ]:
        image_prompt = prompts.image_generation_improve_prompt_sd.format(
            query=q,
            chat_history=conversation_history,
            location=location,
            current_date=today_date,
            references=user_references,
            online_results=simplified_online_results,
            personality_context=personality_context,
        )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    with timer("Chat actor: Generate contextual image prompt", logger):
        response = await send_message_to_model_wrapper(
            image_prompt,
            query_images=query_images,
            user=user,
            query_files=query_files,
            agent_chat_model=agent_chat_model,
            tracer=tracer,
        )
        response = response.strip()
        if response.startswith(('"', "'")) and response.endswith(('"', "'")):
            response = response[1:-1]

    return response


async def search_documents(
    q: str,
    n: int,
    d: float,
    user: KhojUser,
    chat_history: list[ChatMessageModel],
    conversation_id: str,
    conversation_commands: List[ConversationCommand] = [ConversationCommand.Default],
    location_data: LocationData = None,
    send_status_func: Optional[Callable] = None,
    query_images: Optional[List[str]] = None,
    previous_inferred_queries: Set = set(),
    agent: Agent = None,
    query_files: str = None,
    tracer: dict = {},
    filename_prefix_mode: Optional[str] = None,
    filename_prefix: Optional[str] = None,
    file_extension: Optional[str] = None,
):
    # Initialize Variables
    compiled_references: List[dict[str, str]] = []
    inferred_queries: List[str] = []

    agent_has_entries = False

    if agent:
        agent_has_entries = await sync_to_async(EntryAdapters.agent_has_entries)(agent=agent)

    if (
        not ConversationCommand.Notes in conversation_commands
        and not ConversationCommand.Default in conversation_commands
        and not agent_has_entries
    ):
        yield compiled_references, inferred_queries, q
        return

    # If Notes or Default is not in the conversation command, then the search should be restricted to the agent's knowledge base
    should_limit_to_agent_knowledge = (
        ConversationCommand.Notes not in conversation_commands
        and ConversationCommand.Default not in conversation_commands
    )

    if not await sync_to_async(EntryAdapters.user_has_entries)(user=user):
        if not agent_has_entries:
            logger.debug("No documents in knowledge base. Use a Khoj client to sync and chat with your docs.")
            yield compiled_references, inferred_queries, q
            return

    # Extract filter terms from user message
    defiltered_query = defilter_query(q)
    filters_in_query = q.replace(defiltered_query, "").strip()
    conversation = await sync_to_async(ConversationAdapters.get_conversation_by_id)(conversation_id)

    if not conversation:
        logger.error(f"Conversation with id {conversation_id} not found when extracting references.")
        yield compiled_references, inferred_queries, defiltered_query
        return

    filters_in_query += " ".join([f'file:"{filter}"' for filter in conversation.file_filters])
    if is_none_or_empty(filters_in_query):
        logger.debug(f"Filters in query: {filters_in_query}")

    personality_context = prompts.personality_context.format(personality=agent.personality) if agent else ""

    # Infer search queries from user message
    with timer("Extracting search queries took", logger):
        inferred_queries = await extract_questions(
            query=defiltered_query,
            user=user,
            personality_context=personality_context,
            chat_history=chat_history,
            location_data=location_data,
            query_images=query_images,
            query_files=query_files,
            tracer=tracer,
        )

    # Collate search results as context for the LLM
    inferred_queries = list(set(inferred_queries) - previous_inferred_queries)
    with timer("Searching knowledge base took", logger):
        search_results = []
        logger.info(f"🔍 Searching knowledge base with queries: {inferred_queries}")
        if send_status_func:
            inferred_queries_str = "\n- " + "\n- ".join(inferred_queries)
            async for event in send_status_func(f"**Searching Documents for:** {inferred_queries_str}"):
                yield {ChatEvent.STATUS: event}
        for query in inferred_queries:
            # Prepare file_extensions as a list if provided
            file_extensions_list = None
            if file_extension:
                file_extensions_list = [e.strip() for e in file_extension.split(",") if e.strip()]
            results = await execute_search(
                user if not should_limit_to_agent_knowledge else None,
                f"{query} {filters_in_query}",
                n=n,
                t=SearchType.All,
                r=True,
                max_distance=d,
                dedupe=False,
                agent=agent,
                filename_prefix_mode=filename_prefix_mode,
                filename_prefixes=[filename_prefix] if filename_prefix else None,
                file_extensions=file_extensions_list,
            )
            # Attach associated query to each search result
            for item in results:
                item.additional["query"] = query
                search_results.append(item)

        # Get search model for diversity configuration
        search_model = await sync_to_async(get_default_search_model)()
        search_results = text_search.deduplicated_search_responses(search_results, search_model)
        compiled_references = [
            {
                "query": item.additional["query"],
                "compiled": item.additional["compiled"],
                "file": item.additional["file"],
            }
            for item in search_results
        ]

    yield compiled_references, inferred_queries, defiltered_query


async def extract_questions(
    query: str,
    user: KhojUser,
    personality_context: str = "",
    chat_history: List[ChatMessageModel] = [],
    location_data: LocationData = None,
    query_images: Optional[List[str]] = None,
    query_files: str = None,
    tracer: dict = {},
):
    """
    Infer document search queries from user message and provided context
    """
    # Shared context setup
    location = f"{location_data}" if location_data else "N/A"
    username = prompts.user_name.format(name=user.get_full_name()) if user and user.get_full_name() else ""

    # Date variables for prompt formatting
    today = datetime.today()
    current_new_year = today.replace(month=1, day=1)
    last_new_year = current_new_year.replace(year=today.year - 1)
    yesterday = (today - timedelta(days=1)).strftime("%Y-%m-%d")

    # Common prompt setup for API-based models (using Anthropic prompts for consistency)
    chat_history_str = construct_question_history(chat_history, query_prefix="User", agent_name="Assistant")

    system_prompt = prompts.extract_questions_system_prompt.format(
        current_date=today.strftime("%Y-%m-%d"),
        day_of_week=today.strftime("%A"),
        current_month=today.strftime("%Y-%m"),
        last_new_year=last_new_year.strftime("%Y"),
        last_new_year_date=last_new_year.strftime("%Y-%m-%d"),
        current_new_year_date=current_new_year.strftime("%Y-%m-%d"),
        yesterday_date=yesterday,
        location=location,
        username=username,
        personality_context=personality_context,
    )

    prompt = prompts.extract_questions_user_message.format(text=query, chat_history=chat_history_str)

    class DocumentQueries(BaseModel):
        """Choose searches to run on user documents."""

        queries: List[str] = Field(..., min_items=1, description="List of search queries to run on user documents.")

    raw_response = await send_message_to_model_wrapper(
        system_message=system_prompt,
        query=prompt,
        query_images=query_images,
        query_files=query_files,
        response_type="json_object",
        response_schema=DocumentQueries,
        user=user,
        tracer=tracer,
    )

    # Extract questions from the response
    try:
        response = clean_json(raw_response)
        response = pyjson5.loads(response)
        queries = [q.strip() for q in response["queries"] if q.strip()]
        if not isinstance(queries, list) or not queries:
            logger.error(f"Invalid response for constructing subqueries: {response}")
            return [query]
        return queries
    except:
        logger.warning(f"LLM returned invalid JSON. Falling back to using user message as search query.")
        return [query]


async def execute_search(
    user: KhojUser,
    q: str,
    n: Optional[int] = 5,
    t: Optional[SearchType] = None,
    r: Optional[bool] = False,
    max_distance: Optional[Union[float, None]] = None,
    dedupe: Optional[bool] = True,
    agent: Optional[Agent] = None,
    filename_prefix_mode: Optional[str] = None,  # new
    filename_prefixes: Optional[list] = None,  # new
    file_extensions: Optional[list] = None,  # new
):
    # Run validation checks
    results: List[SearchResponse] = []

    start_time = time.time()

    # Ensure the agent, if present, is accessible by the user
    if user and agent and not await AgentAdapters.ais_agent_accessible(agent, user):
        logger.error(f"Agent {agent.slug} is not accessible by user {user}")
        return results

    if q is None or q == "":
        logger.warning(f"No query param (q) passed in API call to initiate search")
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n or 5
    t = t or state.SearchType.All
    search_tasks = []

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
    if t.value != SearchType.Image.value:
        with timer("Encoding query took", logger=logger):
            search_model = await sync_to_async(get_default_search_model)()
            encoded_asymmetric_query = state.embeddings_model[search_model.name].embed_query(defiltered_query)

    # Use asyncio to run searches in parallel
    if t.value in [
        SearchType.All.value,
        SearchType.Org.value,
        SearchType.Markdown.value,
        SearchType.Github.value,
        SearchType.Notion.value,
        SearchType.Plaintext.value,
        SearchType.Pdf.value,
    ]:
        # query markdown notes
        search_tasks.append(
            text_search.query(
                user_query,
                user,
                t,
                question_embedding=encoded_asymmetric_query,
                max_distance=max_distance,
                agent=agent,
                filename_prefix_mode=filename_prefix_mode,
                filename_prefixes=filename_prefixes,
                file_extensions=file_extensions,
            )
        )

    # Query across each requested content types in parallel
    with timer("Query took", logger):
        if search_tasks:
            hits_list = await asyncio.gather(*search_tasks)
            for hits in hits_list:
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


async def send_message_to_model_wrapper(
    query: str,
    system_message: str = "",
    response_type: str = "text",
    response_schema: BaseModel = None,
    deepthought: bool = False,
    user: KhojUser = None,
    query_images: List[str] = None,
    context: str = "",
    query_files: str = None,
    chat_history: list[ChatMessageModel] = [],
    agent_chat_model: ChatModel = None,
    tracer: dict = {},
):
    chat_model: ChatModel = await ConversationAdapters.aget_default_chat_model(user, agent_chat_model)
    vision_available = chat_model.vision_enabled
    if not vision_available and query_images:
        logger.warning(f"Vision is not enabled for default model: {chat_model.name}.")
        vision_enabled_config = await ConversationAdapters.aget_vision_enabled_config()
        if vision_enabled_config:
            chat_model = vision_enabled_config
            vision_available = True
    if vision_available and query_images:
        logger.info(f"Using {chat_model.name} model to understand {len(query_images)} images.")

    max_tokens = await ConversationAdapters.aget_max_context_size(chat_model, user)
    chat_model_name = chat_model.name
    tokenizer = chat_model.tokenizer
    model_type = chat_model.model_type
    vision_available = chat_model.vision_enabled
    api_key = chat_model.ai_model_api.api_key
    api_base_url = chat_model.ai_model_api.api_base_url
    loaded_model = None

    if model_type == ChatModel.ModelType.OFFLINE:
        if state.offline_chat_processor_config is None or state.offline_chat_processor_config.loaded_model is None:
            state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model_name, max_tokens)
        loaded_model = state.offline_chat_processor_config.loaded_model

    truncated_messages = generate_chatml_messages_with_context(
        user_message=query,
        context_message=context,
        system_message=system_message,
        chat_history=chat_history,
        model_name=chat_model_name,
        loaded_model=loaded_model,
        tokenizer_name=tokenizer,
        max_prompt_size=max_tokens,
        vision_enabled=vision_available,
        query_images=query_images,
        model_type=model_type,
        query_files=query_files,
    )

    if model_type == ChatModel.ModelType.OFFLINE:
        return send_message_to_model_offline(
            messages=truncated_messages,
            loaded_model=loaded_model,
            model_name=chat_model_name,
            max_prompt_size=max_tokens,
            streaming=False,
            response_type=response_type,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.OPENAI:
        return send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            deepthought=deepthought,
            api_base_url=api_base_url,
            tracer=tracer,
        )
    elif model_type == ChatModel.ModelType.ANTHROPIC:
        return anthropic_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            deepthought=deepthought,
            api_base_url=api_base_url,
            tracer=tracer,
        )
    elif model_type == ChatModel.ModelType.GOOGLE:
        return gemini_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            deepthought=deepthought,
            api_base_url=api_base_url,
            tracer=tracer,
        )
    else:
        raise HTTPException(status_code=500, detail="Invalid conversation config")


def send_message_to_model_wrapper_sync(
    message: str,
    system_message: str = "",
    response_type: str = "text",
    response_schema: BaseModel = None,
    user: KhojUser = None,
    query_images: List[str] = None,
    query_files: str = "",
    chat_history: List[ChatMessageModel] = [],
    tracer: dict = {},
):
    chat_model: ChatModel = ConversationAdapters.get_default_chat_model(user)

    if chat_model is None:
        raise HTTPException(status_code=500, detail="Contact the server administrator to set a default chat model.")

    max_tokens = ConversationAdapters.get_max_context_size(chat_model, user)
    chat_model_name = chat_model.name
    model_type = chat_model.model_type
    vision_available = chat_model.vision_enabled
    api_key = chat_model.ai_model_api.api_key
    api_base_url = chat_model.ai_model_api.api_base_url
    loaded_model = None

    if model_type == ChatModel.ModelType.OFFLINE:
        if state.offline_chat_processor_config is None or state.offline_chat_processor_config.loaded_model is None:
            state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model_name, max_tokens)
        loaded_model = state.offline_chat_processor_config.loaded_model

    truncated_messages = generate_chatml_messages_with_context(
        user_message=message,
        system_message=system_message,
        chat_history=chat_history,
        model_name=chat_model_name,
        loaded_model=loaded_model,
        max_prompt_size=max_tokens,
        vision_enabled=vision_available,
        model_type=model_type,
        query_images=query_images,
        query_files=query_files,
    )

    if model_type == ChatModel.ModelType.OFFLINE:
        return send_message_to_model_offline(
            messages=truncated_messages,
            loaded_model=loaded_model,
            model_name=chat_model_name,
            max_prompt_size=max_tokens,
            streaming=False,
            response_type=response_type,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.OPENAI:
        return send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            api_base_url=api_base_url,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.ANTHROPIC:
        return anthropic_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            api_base_url=api_base_url,
            model=chat_model_name,
            response_type=response_type,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.GOOGLE:
        return gemini_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            api_base_url=api_base_url,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tracer=tracer,
        )
    else:
        raise HTTPException(status_code=500, detail="Invalid conversation config")


async def agenerate_chat_response(
    q: str,
    chat_history: List[ChatMessageModel],
    conversation: Conversation,
    compiled_references: List[Dict] = [],
    online_results: Dict[str, Dict] = {},
    code_results: Dict[str, Dict] = {},
    operator_results: List[OperatorRun] = [],
    research_results: List[ResearchIteration] = [],
    user: KhojUser = None,
    location_data: LocationData = None,
    user_name: Optional[str] = None,
    query_images: Optional[List[str]] = None,
    query_files: str = None,
    raw_generated_files: List[FileAttachment] = [],
    program_execution_context: List[str] = [],
    generated_asset_results: Dict[str, Dict] = {},
    is_subscribed: bool = False,
    tracer: dict = {},
) -> Tuple[AsyncGenerator[ResponseWithThought, None], Dict[str, str]]:
    # Initialize Variables
    chat_response_generator: AsyncGenerator[ResponseWithThought, None] = None

    metadata = {}
    agent = await AgentAdapters.aget_conversation_agent_by_id(conversation.agent.id) if conversation.agent else None

    try:
        query_to_run = q
        deepthought = False
        if research_results:
            compiled_research = "".join([r.summarizedResult for r in research_results if r.summarizedResult])
            if compiled_research:
                query_to_run = f"<query>{q}</query>\n<collected_research>\n{compiled_research}\n</collected_research>"
            compiled_references = []
            online_results = {}
            code_results = {}
            operator_results = []
            deepthought = True

        chat_model = await ConversationAdapters.aget_valid_chat_model(user, conversation, is_subscribed)
        vision_available = chat_model.vision_enabled
        if not vision_available and query_images:
            vision_enabled_config = await ConversationAdapters.aget_vision_enabled_config()
            if vision_enabled_config:
                chat_model = vision_enabled_config
                vision_available = True

        if chat_model.model_type == "offline":
            loaded_model = state.offline_chat_processor_config.loaded_model
            chat_response_generator = converse_offline(
                # Query
                user_query=query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                location_data=location_data,
                user_name=user_name,
                query_files=query_files,
                chat_history=chat_history,
                # Model
                loaded_model=loaded_model,
                model_name=chat_model.name,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                tracer=tracer,
            )

        elif chat_model.model_type == ChatModel.ModelType.OPENAI:
            openai_chat_config = chat_model.ai_model_api
            api_key = openai_chat_config.api_key
            chat_model_name = chat_model.name
            chat_response_generator = converse_openai(
                # Query
                query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                query_images=query_images,
                query_files=query_files,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                program_execution_context=program_execution_context,
                location_data=location_data,
                user_name=user_name,
                chat_history=chat_history,
                # Model
                model=chat_model_name,
                api_key=api_key,
                api_base_url=openai_chat_config.api_base_url,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                vision_available=vision_available,
                deepthought=deepthought,
                tracer=tracer,
            )

        elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
            api_key = chat_model.ai_model_api.api_key
            api_base_url = chat_model.ai_model_api.api_base_url
            chat_response_generator = converse_anthropic(
                # Query
                query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                query_images=query_images,
                query_files=query_files,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                program_execution_context=program_execution_context,
                location_data=location_data,
                user_name=user_name,
                chat_history=chat_history,
                # Model
                model=chat_model.name,
                api_key=api_key,
                api_base_url=api_base_url,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                vision_available=vision_available,
                deepthought=deepthought,
                tracer=tracer,
            )
        elif chat_model.model_type == ChatModel.ModelType.GOOGLE:
            api_key = chat_model.ai_model_api.api_key
            api_base_url = chat_model.ai_model_api.api_base_url
            chat_response_generator = converse_gemini(
                # Query
                query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                query_images=query_images,
                query_files=query_files,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                program_execution_context=program_execution_context,
                location_data=location_data,
                user_name=user_name,
                chat_history=chat_history,
                # Model
                model=chat_model.name,
                api_key=api_key,
                api_base_url=api_base_url,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                vision_available=vision_available,
                deepthought=deepthought,
                tracer=tracer,
            )

        metadata.update({"chat_model": chat_model.name})

    except Exception as e:
        logger.error(e, exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

    # Return the generator directly
    return chat_response_generator, metadata


class DeleteMessageRequestBody(BaseModel):
    conversation_id: str
    turn_id: str


class FeedbackData(BaseModel):
    uquery: str
    kquery: str
    sentiment: str


class MagicLinkForm(BaseModel):
    email: EmailStr


class EmailAttemptRateLimiter:
    """Rate limiter for email attempts BEFORE get/create user with valid email address."""

    def __init__(self, requests: int, window: int, slug: str):
        self.requests = requests
        self.window = window  # Window in seconds
        self.slug = slug

    async def __call__(self, form: MagicLinkForm):
        # Disable login rate limiting in debug mode
        if in_debug_mode():
            return

        # Calculate the time window cutoff
        cutoff = django_timezone.now() - timedelta(seconds=self.window)

        # Count recent attempts for this email and slug
        count = await RateLimitRecord.objects.filter(
            identifier=form.email, slug=self.slug, created_at__gte=cutoff
        ).acount()

        if count >= self.requests:
            logger.warning(f"Email attempt rate limit exceeded for {form.email} (slug: {self.slug})")
            raise HTTPException(
                status_code=429, detail="Too many requests for your email address. Please wait before trying again."
            )

        # Record the current attempt
        await RateLimitRecord.objects.acreate(identifier=form.email, slug=self.slug)


class EmailVerificationApiRateLimiter:
    """Rate limiter for actions AFTER user with valid email address is known to exist"""

    def __init__(self, requests: int, window: int, slug: str):
        self.requests = requests
        self.window = window  # Window in seconds
        self.slug = slug

    async def __call__(self, email: str = None):
        # Disable login rate limiting in debug mode
        if in_debug_mode():
            return

        user: KhojUser = await aget_user_by_email(email)
        if not user:
            raise HTTPException(status_code=404, detail="User not found.")

        # Remove requests outside of the time window
        cutoff = django_timezone.now() - timedelta(seconds=self.window)
        count_requests = await UserRequests.objects.filter(user=user, created_at__gte=cutoff, slug=self.slug).acount()

        # Check if the user has exceeded the rate limit
        if count_requests >= self.requests:
            logger.warning(
                f"Rate limit: {count_requests}/{self.requests} requests not allowed in {self.window} seconds for email: {email}."
            )
            raise HTTPException(status_code=429, detail="Ran out of login attempts. Please wait before trying again.")

        # Add the current request to the db
        await UserRequests.objects.acreate(user=user, slug=self.slug)


class ApiUserRateLimiter:
    def __init__(self, requests: int, subscribed_requests: int, window: int, slug: str):
        self.requests = requests
        self.subscribed_requests = subscribed_requests
        self.window = window
        self.slug = slug

    def __call__(self, request: Request):
        # Rate limiting disabled if billing is disabled
        if state.billing_enabled is False:
            return

        # Rate limiting is disabled if user unauthenticated.
        # Other systems handle authentication
        if not request.user.is_authenticated:
            return

        user: KhojUser = request.user.object
        subscribed = has_required_scope(request, ["premium"])

        # Remove requests outside of the time window
        cutoff = django_timezone.now() - timedelta(seconds=self.window)
        count_requests = UserRequests.objects.filter(user=user, created_at__gte=cutoff, slug=self.slug).count()

        # Check if the user has exceeded the rate limit
        if subscribed and count_requests >= self.subscribed_requests:
            logger.info(
                f"Rate limit: {count_requests}/{self.subscribed_requests} requests not allowed in {self.window} seconds for subscribed user: {user}."
            )
            raise HTTPException(
                status_code=429,
                detail="I'm glad you're enjoying interacting with me! You've unfortunately exceeded your usage limit for today. But let's chat more tomorrow?",
            )
        if not subscribed and count_requests >= self.requests:
            if self.requests >= self.subscribed_requests:
                logger.info(
                    f"Rate limit: {count_requests}/{self.subscribed_requests} requests not allowed in {self.window} seconds for user: {user}."
                )
                raise HTTPException(
                    status_code=429,
                    detail="I'm glad you're enjoying interacting with me! You've unfortunately exceeded your usage limit for today. But let's chat more tomorrow?",
                )

            logger.info(
                f"Rate limit: {count_requests}/{self.requests} requests not allowed in {self.window} seconds for user: {user}."
            )
            raise HTTPException(
                status_code=429,
                detail="I'm glad you're enjoying interacting with me! You've unfortunately exceeded your usage limit for today. You can subscribe to increase your usage limit via [your settings](https://app.khoj.dev/settings) or we can continue our conversation tomorrow?",
            )

        # Add the current request to the cache
        UserRequests.objects.create(user=user, slug=self.slug)


class ApiImageRateLimiter:
    def __init__(self, max_images: int = 10, max_combined_size_mb: float = 10):
        self.max_images = max_images
        self.max_combined_size_mb = max_combined_size_mb

    def __call__(self, request: Request, body: ChatRequestBody):
        if state.billing_enabled is False:
            return

        # Rate limiting is disabled if user unauthenticated.
        # Other systems handle authentication
        if not request.user.is_authenticated:
            return

        if not body.images:
            return

        # Check number of images
        if len(body.images) > self.max_images:
            logger.info(f"Rate limit: {len(body.images)}/{self.max_images} images not allowed per message.")
            raise HTTPException(
                status_code=429,
                detail=f"Those are way too many images for me! I can handle up to {self.max_images} images per message.",
            )

        # Check total size of images
        total_size_mb = 0.0
        for image in body.images:
            # Unquote the image in case it's URL encoded
            image = unquote(image)
            # Assuming the image is a base64 encoded string
            # Remove the data:image/jpeg;base64, part if present
            if "," in image:
                image = image.split(",", 1)[1]

            # Decode base64 to get the actual size
            image_bytes = base64.b64decode(image)
            total_size_mb += len(image_bytes) / (1024 * 1024)  # Convert bytes to MB

        if total_size_mb > self.max_combined_size_mb:
            logger.info(f"Data limit: {total_size_mb}MB/{self.max_combined_size_mb}MB size not allowed per message.")
            raise HTTPException(
                status_code=429,
                detail=f"Those images are way too large for me! I can handle up to {self.max_combined_size_mb}MB of images per message.",
            )


class ConversationCommandRateLimiter:
    def __init__(self, trial_rate_limit: int, subscribed_rate_limit: int, slug: str):
        self.slug = slug
        self.trial_rate_limit = trial_rate_limit
        self.subscribed_rate_limit = subscribed_rate_limit
        self.restricted_commands = [ConversationCommand.Research]

    async def update_and_check_if_valid(self, request: Request, conversation_command: ConversationCommand):
        if state.billing_enabled is False:
            return

        if not request.user.is_authenticated:
            return

        if conversation_command not in self.restricted_commands:
            return

        user: KhojUser = request.user.object
        subscribed = has_required_scope(request, ["premium"])

        # Remove requests outside of the 24-hr time window
        cutoff = django_timezone.now() - timedelta(seconds=60 * 60 * 24)
        command_slug = f"{self.slug}_{conversation_command.value}"
        count_requests = await UserRequests.objects.filter(
            user=user, created_at__gte=cutoff, slug=command_slug
        ).acount()

        if subscribed and count_requests >= self.subscribed_rate_limit:
            logger.info(
                f"Rate limit: {count_requests}/{self.subscribed_rate_limit} requests not allowed in 24 hours for subscribed user: {user}."
            )
            raise HTTPException(
                status_code=429,
                detail=f"I'm glad you're enjoying interacting with me! You've unfortunately exceeded your `/{conversation_command.value}` command usage limit for today. Maybe we can talk about something else for today?",
            )
        if not subscribed and count_requests >= self.trial_rate_limit:
            logger.info(
                f"Rate limit: {count_requests}/{self.trial_rate_limit} requests not allowed in 24 hours for user: {user}."
            )
            raise HTTPException(
                status_code=429,
                detail=f"I'm glad you're enjoying interacting with me! You've unfortunately exceeded your `/{conversation_command.value}` command usage limit for today. You can subscribe to increase your usage limit via [your settings](https://app.khoj.dev/settings) or we can talk about something else for today?",
            )
        await UserRequests.objects.acreate(user=user, slug=command_slug)
        return


class ApiIndexedDataLimiter:
    def __init__(
        self,
        incoming_entries_size_limit: float,
        subscribed_incoming_entries_size_limit: float,
        total_entries_size_limit: float,
        subscribed_total_entries_size_limit: float,
    ):
        self.num_entries_size = incoming_entries_size_limit
        self.subscribed_num_entries_size = subscribed_incoming_entries_size_limit
        self.total_entries_size_limit = total_entries_size_limit
        self.subscribed_total_entries_size = subscribed_total_entries_size_limit

    def __call__(self, request: Request, files: List[UploadFile] = None):
        if state.billing_enabled is False:
            return

        subscribed = has_required_scope(request, ["premium"])
        incoming_data_size_mb = 0.0
        deletion_file_names = set()

        if not request.user.is_authenticated or not files:
            return

        user: KhojUser = request.user.object

        for file in files:
            if file.size == 0:
                deletion_file_names.add(file.filename)

            incoming_data_size_mb += file.size / 1024 / 1024

        num_deleted_entries = 0
        for file_path in deletion_file_names:
            deleted_count = EntryAdapters.delete_entry_by_file(user, file_path)
            num_deleted_entries += deleted_count

        logger.info(f"Deleted {num_deleted_entries} entries for user: {user}.")

        if subscribed and incoming_data_size_mb >= self.subscribed_num_entries_size:
            logger.info(
                f"Data limit: {incoming_data_size_mb}MB incoming will exceed {self.subscribed_num_entries_size}MB allowed for subscribed user: {user}."
            )
            raise HTTPException(status_code=429, detail="Too much data indexed.")
        if not subscribed and incoming_data_size_mb >= self.num_entries_size:
            logger.info(
                f"Data limit: {incoming_data_size_mb}MB incoming will exceed {self.num_entries_size}MB allowed for user: {user}."
            )
            raise HTTPException(
                status_code=429, detail="Too much data indexed. Subscribe to increase your data index limit."
            )

        user_size_data = EntryAdapters.get_size_of_indexed_data_in_mb(user)
        if subscribed and user_size_data + incoming_data_size_mb >= self.subscribed_total_entries_size:
            logger.info(
                f"Data limit: {incoming_data_size_mb}MB incoming + {user_size_data}MB existing will exceed {self.subscribed_total_entries_size}MB allowed for subscribed user: {user}."
            )
            raise HTTPException(status_code=429, detail="Too much data indexed.")
        if not subscribed and user_size_data + incoming_data_size_mb >= self.total_entries_size_limit:
            logger.info(
                f"Data limit: {incoming_data_size_mb}MB incoming + {user_size_data}MB existing will exceed {self.subscribed_total_entries_size}MB allowed for non subscribed user: {user}."
            )
            raise HTTPException(
                status_code=429, detail="Too much data indexed. Subscribe to increase your data index limit."
            )


class CommonQueryParamsClass:
    def __init__(
        self,
        client: Optional[str] = None,
        user_agent: Optional[str] = Header(None),
        referer: Optional[str] = Header(None),
        host: Optional[str] = Header(None),
    ):
        self.client = client
        self.user_agent = user_agent
        self.referer = referer
        self.host = host


CommonQueryParams = Annotated[CommonQueryParamsClass, Depends()]


def format_automation_response(scheduling_request: str, executed_query: str, ai_response: str, user: KhojUser) -> bool:
    """
    Format the AI response to send in automation email to user.
    """
    name = get_user_name(user)
    username = prompts.user_name.format(name=name) if name else ""

    automation_format_prompt = prompts.automation_format_prompt.format(
        original_query=scheduling_request,
        executed_query=executed_query,
        response=ai_response,
        username=username,
    )

    with timer("Chat actor: Format automation response", logger):
        return send_message_to_model_wrapper_sync(automation_format_prompt, user=user)


def should_notify(original_query: str, executed_query: str, ai_response: str, user: KhojUser) -> bool:
    """
    Decide whether to notify the user of the AI response.
    Default to notifying the user for now.
    """
    if any(is_none_or_empty(message) for message in [original_query, executed_query, ai_response]):
        return False

    to_notify_or_not = prompts.to_notify_or_not.format(
        original_query=original_query,
        executed_query=executed_query,
        response=ai_response,
    )

    with timer("Chat actor: Decide to notify user of automation response", logger):
        try:
            # TODO Replace with async call so we don't have to maintain a sync version
            raw_response = send_message_to_model_wrapper_sync(to_notify_or_not, user=user, response_type="json_object")
            response = json.loads(clean_json(raw_response))
            should_notify_result = response["decision"] == "Yes"
            reason = response.get("reason", "unknown")
            logger.info(
                f'Decided to {"not " if not should_notify_result else ""}notify user of automation response because of reason: {reason}.'
            )
            return should_notify_result
        except Exception as e:
            logger.warning(
                f"Fallback to notify user of automation response as failed to infer should notify or not. {e}",
                exc_info=True,
            )
            return True


def scheduled_chat(
    query_to_run: str,
    scheduling_request: str,
    subject: str,
    user: KhojUser,
    calling_url: URL,
    job_id: str = None,
    conversation_id: str = None,
):
    logger.info(f"Processing scheduled_chat: {query_to_run}")
    if job_id:
        # Get the job object and check whether the time is valid for it to run. This helps avoid race conditions that cause the same job to be run multiple times.
        job = AutomationAdapters.get_automation(user, job_id)
        last_run_time = AutomationAdapters.get_job_last_run(user, job)

        # Convert last_run_time from %Y-%m-%d %I:%M %p %Z to datetime object
        if last_run_time:
            last_run_time = datetime.strptime(last_run_time, "%Y-%m-%d %I:%M %p %Z").replace(tzinfo=timezone.utc)

            # If the last run time was within the last 6 hours, don't run it again. This helps avoid multithreading issues and rate limits.
            if (datetime.now(timezone.utc) - last_run_time).total_seconds() < 6 * 60 * 60:
                logger.info(f"Skipping scheduled chat {job_id} as the next run time is in the future.")
                return

    # Extract relevant params from the original URL
    scheme = "http" if not calling_url.is_secure else "https"
    query_dict = parse_qs(calling_url.query)

    # Pop the stream value from query_dict if it exists
    query_dict.pop("stream", None)

    # Replace the original scheduling query with the scheduled query
    query_dict["q"] = [query_to_run]

    # Replace the original conversation_id with the conversation_id
    if conversation_id:
        # encode the conversation_id to avoid any issues with special characters
        query_dict["conversation_id"] = [quote(str(conversation_id))]

        # validate that the conversation id exists. If not, delete the automation and exit.
        if not ConversationAdapters.get_conversation_by_id(conversation_id):
            AutomationAdapters.delete_automation(user, job_id)
            return

    # Restructure the original query_dict into a valid JSON payload for the chat API
    json_payload = {key: values[0] for key, values in query_dict.items()}

    # Construct the URL to call the chat API with the scheduled query string
    url = f"{scheme}://{calling_url.netloc}/api/chat?client=khoj"

    # Construct the Headers for the chat API
    headers = {"User-Agent": "Khoj", "Content-Type": "application/json"}
    if not state.anonymous_mode:
        # Add authorization request header in non-anonymous mode
        token = get_khoj_tokens(user)
        if is_none_or_empty(token):
            token = create_khoj_token(user).token
        else:
            token = token[0].token
        headers["Authorization"] = f"Bearer {token}"

    # Call the chat API endpoint with authenticated user token and query
    raw_response = requests.post(url, headers=headers, json=json_payload, allow_redirects=False)

    # Handle redirect manually if necessary
    if raw_response.status_code in [301, 302, 308]:
        redirect_url = raw_response.headers["Location"]
        logger.info(f"Redirecting to {redirect_url}")
        raw_response = requests.post(redirect_url, headers=headers, json=json_payload)

    # Stop if the chat API call was not successful
    if raw_response.status_code != 200:
        logger.error(f"Failed to run schedule chat: {raw_response.text}, user: {user}, query: {query_to_run}")
        return None

    # Extract the AI response from the chat API response
    cleaned_query = re.sub(r"^/automated_task\s*", "", query_to_run).strip()
    is_image = False
    if raw_response.headers.get("Content-Type") == "application/json":
        response_map = raw_response.json()
        ai_response = response_map.get("response") or response_map.get("image")
        is_image = False
        if type(ai_response) == dict:
            is_image = ai_response.get("image") is not None
    else:
        ai_response = raw_response.text

    # Notify user if the AI response is satisfactory
    if should_notify(
        original_query=scheduling_request, executed_query=cleaned_query, ai_response=ai_response, user=user
    ):
        formatted_response = format_automation_response(scheduling_request, cleaned_query, ai_response, user)

        if is_resend_enabled():
            send_task_email(user.get_short_name(), user.email, cleaned_query, formatted_response, subject, is_image)
        else:
            return formatted_response


async def create_automation(
    q: str,
    timezone: str,
    user: KhojUser,
    calling_url: URL,
    chat_history: List[ChatMessageModel] = [],
    conversation_id: str = None,
    tracer: dict = {},
):
    crontime, query_to_run, subject = await aschedule_query(q, chat_history, user, tracer=tracer)
    job = await aschedule_automation(query_to_run, subject, crontime, timezone, q, user, calling_url, conversation_id)
    return job, crontime, query_to_run, subject


def schedule_automation(
    query_to_run: str,
    subject: str,
    crontime: str,
    timezone: str,
    scheduling_request: str,
    user: KhojUser,
    calling_url: URL,
    conversation_id: str,
):
    # Disable minute level automation recurrence
    minute_value = crontime.split(" ")[0]
    if not minute_value.isdigit():
        # Run automation at some random minute (to distribute request load) instead of running every X minutes
        crontime = " ".join([str(math.floor(random() * 60))] + crontime.split(" ")[1:])

    # Convert timezone string to timezone object
    try:
        user_timezone = pytz.timezone(timezone)
    except pytz.UnknownTimeZoneError:
        logger.warning(f"Invalid timezone: {timezone}. Fallback to use UTC to schedule automation.")
        user_timezone = pytz.utc

    trigger = CronTrigger.from_crontab(crontime, user_timezone)
    trigger.jitter = 60
    # Generate id and metadata used by task scheduler and process locks for the task runs
    job_metadata = json.dumps(
        {
            "query_to_run": query_to_run,
            "scheduling_request": scheduling_request,
            "subject": subject,
            "crontime": crontime,
            "conversation_id": str(conversation_id),
        }
    )
    query_id = hashlib.md5(f"{query_to_run}_{crontime}".encode("utf-8")).hexdigest()
    job_id = f"automation_{user.uuid}_{query_id}"
    job = state.scheduler.add_job(
        run_with_process_lock,
        trigger=trigger,
        args=(
            scheduled_chat,
            f"{ProcessLock.Operation.SCHEDULED_JOB}_{user.uuid}_{query_id}",
        ),
        kwargs={
            "query_to_run": query_to_run,
            "scheduling_request": scheduling_request,
            "subject": subject,
            "user": user,
            "calling_url": calling_url,
            "job_id": job_id,
            "conversation_id": conversation_id,
        },
        id=job_id,
        name=job_metadata,
        max_instances=2,  # Allow second instance to kill any previous instance with stale lock
    )
    return job


async def aschedule_automation(
    query_to_run: str,
    subject: str,
    crontime: str,
    timezone: str,
    scheduling_request: str,
    user: KhojUser,
    calling_url: URL,
    conversation_id: str,
):
    # Disable minute level automation recurrence
    minute_value = crontime.split(" ")[0]
    if not minute_value.isdigit():
        # Run automation at some random minute (to distribute request load) instead of running every X minutes
        crontime = " ".join([str(math.floor(random() * 60))] + crontime.split(" ")[1:])

    user_timezone = pytz.timezone(timezone)
    trigger = CronTrigger.from_crontab(crontime, user_timezone)
    trigger.jitter = 60
    # Generate id and metadata used by task scheduler and process locks for the task runs
    job_metadata = json.dumps(
        {
            "query_to_run": query_to_run,
            "scheduling_request": scheduling_request,
            "subject": subject,
            "crontime": crontime,
            "conversation_id": str(conversation_id),
        }
    )
    query_id = hashlib.md5(f"{query_to_run}_{crontime}".encode("utf-8")).hexdigest()
    job_id = f"automation_{user.uuid}_{query_id}"
    job = await sync_to_async(state.scheduler.add_job)(
        run_with_process_lock,
        trigger=trigger,
        args=(
            scheduled_chat,
            f"{ProcessLock.Operation.SCHEDULED_JOB}_{user.uuid}_{query_id}",
        ),
        kwargs={
            "query_to_run": query_to_run,
            "scheduling_request": scheduling_request,
            "subject": subject,
            "user": user,
            "calling_url": calling_url,
            "job_id": job_id,
            "conversation_id": conversation_id,
        },
        id=job_id,
        name=job_metadata,
        max_instances=2,  # Allow second instance to kill any previous instance with stale lock
    )
    return job


def construct_automation_created_message(automation: Job, crontime: str, query_to_run: str, subject: str):
    # Display next run time in user timezone instead of UTC
    schedule = f'{cron_descriptor.get_description(crontime)} {automation.next_run_time.strftime("%Z")}'
    next_run_time = automation.next_run_time.strftime("%Y-%m-%d %I:%M %p %Z")
    # Remove /automated_task prefix from inferred_query
    unprefixed_query_to_run = re.sub(r"^\/automated_task\s*", "", query_to_run)
    # Create the automation response
    automation_icon_url = f"/static/assets/icons/automation.svg"
    return f"""
    ### ![]({automation_icon_url}) Created Automation
- Subject: **{subject}**
- Query to Run: "{unprefixed_query_to_run}"
- Schedule: `{schedule}`
- Next Run At: {next_run_time}

Manage your automations [here](/automations).
    """.strip()


class MessageProcessor:
    def __init__(self):
        self.references = {}
        self.usage = {}
        self.raw_response = ""
        self.generated_images = []
        self.generated_files = []
        self.generated_mermaidjs_diagram = []

    def convert_message_chunk_to_json(self, raw_chunk: str) -> Dict[str, Any]:
        if raw_chunk.startswith("{") and raw_chunk.endswith("}"):
            try:
                json_chunk = json.loads(raw_chunk)
                if "type" not in json_chunk:
                    json_chunk = {"type": "message", "data": json_chunk}
                return json_chunk
            except json.JSONDecodeError:
                return {"type": "message", "data": raw_chunk}
        elif raw_chunk:
            return {"type": "message", "data": raw_chunk}
        return {"type": "", "data": ""}

    def process_message_chunk(self, raw_chunk: str) -> None:
        chunk = self.convert_message_chunk_to_json(raw_chunk)
        if not chunk or not chunk["type"]:
            return

        chunk_type = ChatEvent(chunk["type"])
        if chunk_type == ChatEvent.REFERENCES:
            self.references = chunk["data"]
        elif chunk_type == ChatEvent.USAGE:
            self.usage = chunk["data"]
        elif chunk_type == ChatEvent.MESSAGE:
            chunk_data = chunk["data"]
            if isinstance(chunk_data, dict):
                self.raw_response = self.handle_json_response(chunk_data)
            elif (
                isinstance(chunk_data, str) and chunk_data.strip().startswith("{") and chunk_data.strip().endswith("}")
            ):
                try:
                    json_data = json.loads(chunk_data.strip())
                    self.raw_response = self.handle_json_response(json_data)
                except json.JSONDecodeError:
                    self.raw_response += chunk_data
            else:
                self.raw_response += chunk_data
        elif chunk_type == ChatEvent.GENERATED_ASSETS:
            chunk_data = chunk["data"]
            if isinstance(chunk_data, dict):
                for key in chunk_data:
                    if key == "images":
                        self.generated_images = chunk_data[key]
                    elif key == "files":
                        self.generated_files = chunk_data[key]
                    elif key == "mermaidjsDiagram":
                        self.generated_mermaidjs_diagram = chunk_data[key]

    def handle_json_response(self, json_data: Dict[str, str]) -> str | Dict[str, str]:
        if "image" in json_data or "details" in json_data:
            return json_data
        if "response" in json_data:
            return json_data["response"]
        return json_data


async def read_chat_stream(response_iterator: AsyncGenerator[str, None]) -> Dict[str, Any]:
    processor = MessageProcessor()
    event_delimiter = "␃🔚␗"
    buffer = ""

    async for chunk in response_iterator:
        # Start buffering chunks until complete event is received
        buffer += chunk

        # Once the buffer contains a complete event
        while event_delimiter in buffer:
            # Extract the event from the buffer
            event, buffer = buffer.split(event_delimiter, 1)
            # Process the event
            if event:
                processor.process_message_chunk(event)

    # Process any remaining data in the buffer
    if buffer:
        processor.process_message_chunk(buffer)

    return {
        "response": processor.raw_response,
        "references": processor.references,
        "usage": processor.usage,
        "images": processor.generated_images,
        "files": processor.generated_files,
        "mermaidjsDiagram": processor.generated_mermaidjs_diagram,
    }


def get_user_config(user: KhojUser, request: Request, is_detailed: bool = False):
    user_picture = request.session.get("user", {}).get("picture")
    is_active = has_required_scope(request, ["premium"])
    has_documents = EntryAdapters.user_has_entries(user=user)

    if not is_detailed:
        return {
            "request": request,
            "username": user.username if user else None,
            "user_photo": user_picture,
            "is_active": is_active,
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        }

    user_subscription_state = get_user_subscription_state(user.email)
    user_subscription = adapters.get_user_subscription(user.email)

    subscription_renewal_date = (
        user_subscription.renewal_date.strftime("%d %b %Y")
        if user_subscription and user_subscription.renewal_date
        else None
    )
    subscription_enabled_trial_at = (
        user_subscription.enabled_trial_at.strftime("%d %b %Y")
        if user_subscription and user_subscription.enabled_trial_at
        else None
    )
    given_name = get_user_name(user)

    enabled_content_sources_set = set(EntryAdapters.get_unique_file_sources(user))
    enabled_content_sources = {
        "computer": ("computer" in enabled_content_sources_set),
        "github": ("github" in enabled_content_sources_set),
        "notion": ("notion" in enabled_content_sources_set),
    }

    notion_oauth_url = get_notion_auth_url(user)
    current_notion_config = get_user_notion_config(user)
    notion_token = current_notion_config.token if current_notion_config else ""

    selected_chat_model_config = ConversationAdapters.get_chat_model(
        user
    ) or ConversationAdapters.get_default_chat_model(user)
    chat_models = ConversationAdapters.get_conversation_processor_options().all()
    chat_model_options = list()
    for chat_model in chat_models:
        chat_model_options.append(
            {
                "name": chat_model.name,
                "id": chat_model.id,
                "strengths": chat_model.strengths,
                "description": chat_model.description,
                "tier": chat_model.price_tier,
            }
        )

    selected_paint_model_config = ConversationAdapters.get_user_text_to_image_model_config(user)
    paint_model_options = ConversationAdapters.get_text_to_image_model_options().all()
    all_paint_model_options = list()
    for paint_model in paint_model_options:
        all_paint_model_options.append(
            {
                "name": paint_model.model_name,
                "id": paint_model.id,
                "tier": paint_model.price_tier,
            }
        )

    voice_models = ConversationAdapters.get_voice_model_options()
    voice_model_options = list()
    for voice_model in voice_models:
        voice_model_options.append(
            {
                "name": voice_model.name,
                "id": voice_model.model_id,
                "tier": voice_model.price_tier,
            }
        )

    if len(voice_model_options) == 0:
        eleven_labs_enabled = False
    else:
        eleven_labs_enabled = is_eleven_labs_enabled()

    selected_voice_model_config = ConversationAdapters.get_voice_model_config(user)

    return {
        "request": request,
        # user info
        "username": user.username if user else None,
        "user_photo": user_picture,
        "is_active": is_active,
        "given_name": given_name,
        "phone_number": str(user.phone_number) if user.phone_number else "",
        "is_phone_number_verified": user.verified_phone_number,
        # user content settings
        "enabled_content_source": enabled_content_sources,
        "has_documents": has_documents,
        "notion_token": notion_token,
        # user model settings
        "chat_model_options": chat_model_options,
        "selected_chat_model_config": selected_chat_model_config.id if selected_chat_model_config else None,
        "paint_model_options": all_paint_model_options,
        "selected_paint_model_config": selected_paint_model_config.id if selected_paint_model_config else None,
        "voice_model_options": voice_model_options,
        "selected_voice_model_config": selected_voice_model_config.model_id if selected_voice_model_config else None,
        # user billing info
        "subscription_state": user_subscription_state,
        "subscription_renewal_date": subscription_renewal_date,
        "subscription_enabled_trial_at": subscription_enabled_trial_at,
        # server settings
        "khoj_cloud_subscription_url": os.getenv("KHOJ_CLOUD_SUBSCRIPTION_URL"),
        "billing_enabled": state.billing_enabled,
        "is_eleven_labs_enabled": eleven_labs_enabled,
        "is_twilio_enabled": is_twilio_enabled(),
        "khoj_version": state.khoj_version,
        "anonymous_mode": state.anonymous_mode,
        "notion_oauth_url": notion_oauth_url,
        "length_of_free_trial": LENGTH_OF_FREE_TRIAL,
    }


def configure_content(
    user: KhojUser,
    files: Optional[dict[str, dict[str, str]]],
    regenerate: bool = False,
    t: Optional[state.SearchType] = state.SearchType.All,
) -> bool:
    success = True
    if t == None:
        t = state.SearchType.All

    if t is not None and t in [type.value for type in state.SearchType]:
        t = state.SearchType(t)

    if t is not None and not t.value in [type.value for type in state.SearchType]:
        logger.warning(f"🚨 Invalid search type: {t}")
        return False

    search_type = t.value if t else None

    no_documents = all([not files.get(file_type) for file_type in files])

    if files is None:
        logger.warning(f"🚨 No files to process for {search_type} search.")
        return True

    try:
        # Initialize Org Notes Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Org.value) and files["org"]:
            logger.info("🦄 Setting up search for orgmode notes")
            # Extract Entries, Generate Notes Embeddings
            text_search.setup(
                OrgToEntries,
                files.get("org"),
                regenerate=regenerate,
                user=user,
            )
    except Exception as e:
        logger.error(f"🚨 Failed to setup org: {e}", exc_info=True)
        success = False

    try:
        # Initialize Markdown Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Markdown.value) and files[
            "markdown"
        ]:
            logger.info("💎 Setting up search for markdown notes")
            # Extract Entries, Generate Markdown Embeddings
            text_search.setup(
                MarkdownToEntries,
                files.get("markdown"),
                regenerate=regenerate,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup markdown: {e}", exc_info=True)
        success = False

    try:
        # Initialize PDF Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Pdf.value) and files["pdf"]:
            logger.info("🖨️ Setting up search for pdf")
            # Extract Entries, Generate PDF Embeddings
            text_search.setup(
                PdfToEntries,
                files.get("pdf"),
                regenerate=regenerate,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup PDF: {e}", exc_info=True)
        success = False

    try:
        # Initialize Plaintext Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Plaintext.value) and files[
            "plaintext"
        ]:
            logger.info("📄 Setting up search for plaintext")
            # Extract Entries, Generate Plaintext Embeddings
            text_search.setup(
                PlaintextToEntries,
                files.get("plaintext"),
                regenerate=regenerate,
                user=user,
            )

    except Exception as e:
        logger.error(f"🚨 Failed to setup plaintext: {e}", exc_info=True)
        success = False

    try:
        if no_documents:
            github_config = GithubConfig.objects.filter(user=user).prefetch_related("githubrepoconfig").first()
            if (
                search_type == state.SearchType.All.value or search_type == state.SearchType.Github.value
            ) and github_config is not None:
                logger.info("🐙 Setting up search for github")
                # Extract Entries, Generate Github Embeddings
                text_search.setup(
                    GithubToEntries,
                    None,
                    regenerate=regenerate,
                    user=user,
                    config=github_config,
                )

    except Exception as e:
        logger.error(f"🚨 Failed to setup GitHub: {e}", exc_info=True)
        success = False

    try:
        if no_documents:
            # Initialize Notion Search
            notion_config = NotionConfig.objects.filter(user=user).first()
            if (
                search_type == state.SearchType.All.value or search_type == state.SearchType.Notion.value
            ) and notion_config:
                logger.info("🔌 Setting up search for notion")
                text_search.setup(
                    NotionToEntries,
                    None,
                    regenerate=regenerate,
                    user=user,
                    config=notion_config,
                )

    except Exception as e:
        logger.error(f"🚨 Failed to setup Notion: {e}", exc_info=True)
        success = False

    try:
        # Initialize Image Search
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Image.value) and files[
            "image"
        ]:
            logger.info("🖼️ Setting up search for images")
            # Extract Entries, Generate Image Embeddings
            text_search.setup(
                ImageToEntries,
                files.get("image"),
                regenerate=regenerate,
                user=user,
            )
    except Exception as e:
        logger.error(f"🚨 Failed to setup images: {e}", exc_info=True)
        success = False
    try:
        if (search_type == state.SearchType.All.value or search_type == state.SearchType.Docx.value) and files["docx"]:
            logger.info("📄 Setting up search for docx")
            text_search.setup(
                DocxToEntries,
                files.get("docx"),
                regenerate=regenerate,
                user=user,
            )
    except Exception as e:
        logger.error(f"🚨 Failed to setup docx: {e}", exc_info=True)
        success = False

    # Invalidate Query Cache
    if user:
        state.query_cache[user.uuid] = LRU()

    return success


def get_notion_auth_url(user: KhojUser):
    if not NOTION_OAUTH_CLIENT_ID or not NOTION_OAUTH_CLIENT_SECRET or not NOTION_REDIRECT_URI:
        return None
    return f"https://api.notion.com/v1/oauth/authorize?client_id={NOTION_OAUTH_CLIENT_ID}&redirect_uri={NOTION_REDIRECT_URI}&response_type=code&state={user.uuid}"
