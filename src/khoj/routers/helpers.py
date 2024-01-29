import asyncio
import json
import logging
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
from functools import partial
from time import time
from typing import Annotated, Any, Dict, Iterator, List, Optional, Tuple, Union

import openai
from fastapi import Depends, Header, HTTPException, Request, UploadFile
from starlette.authentication import has_required_scope

from khoj.database.adapters import ConversationAdapters, EntryAdapters
from khoj.database.models import (
    ChatModelOptions,
    ClientApplication,
    KhojUser,
    Subscription,
    TextToImageModelConfig,
    UserRequests,
)
from khoj.processor.conversation import prompts
from khoj.processor.conversation.offline.chat_model import (
    converse_offline,
    send_message_to_model_offline,
)
from khoj.processor.conversation.openai.gpt import converse, send_message_to_model
from khoj.processor.conversation.utils import (
    ThreadedGenerator,
    generate_chatml_messages_with_context,
    save_to_conversation_log,
)
from khoj.utils import state
from khoj.utils.config import GPT4AllProcessorModel
from khoj.utils.helpers import ConversationCommand, log_telemetry

logger = logging.getLogger(__name__)

executor = ThreadPoolExecutor(max_workers=1)


def validate_conversation_config():
    if (
        ConversationAdapters.has_valid_offline_conversation_config()
        or ConversationAdapters.has_valid_openai_conversation_config()
    ):
        if ConversationAdapters.get_default_conversation_config() is None:
            raise HTTPException(status_code=500, detail="Contact the server administrator to set a default chat model.")
        return

    raise HTTPException(status_code=500, detail="Set your OpenAI API key or enable Local LLM via Khoj settings.")


async def is_ready_to_chat(user: KhojUser):
    has_offline_config = await ConversationAdapters.ahas_offline_chat()
    has_openai_config = await ConversationAdapters.has_openai_chat()
    user_conversation_config = await ConversationAdapters.aget_user_conversation_config(user)

    if has_offline_config and user_conversation_config and user_conversation_config.model_type == "offline":
        chat_model = user_conversation_config.chat_model
        if state.gpt4all_processor_config is None:
            logger.info("Loading Offline Chat Model...")
            state.gpt4all_processor_config = GPT4AllProcessorModel(chat_model=chat_model)
        return True

    ready = has_openai_config or has_offline_config

    if not ready:
        raise HTTPException(status_code=500, detail="Set your OpenAI API key or enable Local LLM via Khoj settings.")


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
            telemetry_type=telemetry_type, api=api, client=client, app_config=state.config.app, properties=user_state
        )
    ]


def get_conversation_command(query: str, any_references: bool = False) -> ConversationCommand:
    if query.startswith("/notes"):
        return ConversationCommand.Notes
    elif query.startswith("/help"):
        return ConversationCommand.Help
    elif query.startswith("/general"):
        return ConversationCommand.General
    elif query.startswith("/online"):
        return ConversationCommand.Online
    elif query.startswith("/image"):
        return ConversationCommand.Image
    # If no relevant notes found for the given query
    elif not any_references:
        return ConversationCommand.General
    else:
        return ConversationCommand.Default


async def agenerate_chat_response(*args):
    loop = asyncio.get_event_loop()
    return await loop.run_in_executor(executor, generate_chat_response, *args)


async def generate_online_subqueries(q: str, conversation_history: dict) -> List[str]:
    """
    Generate subqueries from the given query
    """
    chat_history = ""
    for chat in conversation_history.get("chat", [])[-4:]:
        if chat["by"] == "khoj" and chat["intent"].get("type") == "remember":
            chat_history += f"User: {chat['intent']['query']}\n"
            chat_history += f"Khoj: {chat['message']}\n"

    utc_date = datetime.utcnow().strftime("%Y-%m-%d")
    online_queries_prompt = prompts.online_search_conversation_subqueries.format(
        current_date=utc_date,
        query=q,
        chat_history=chat_history,
    )

    response = await send_message_to_model_wrapper(online_queries_prompt)

    # Validate that the response is a non-empty, JSON-serializable list
    try:
        response = response.strip()
        response = json.loads(response)
        response = [q.strip() for q in response if q.strip()]
        if not isinstance(response, list) or not response or len(response) == 0:
            logger.error(f"Invalid response for constructing subqueries: {response}")
            return [q]
        return response
    except Exception as e:
        logger.error(f"Invalid response for constructing subqueries: {response}")
        return [q]


async def extract_relevant_info(q: str, corpus: dict) -> List[str]:
    """
    Given a target corpus, extract the most relevant info given a query
    """

    key = list(corpus.keys())[0]
    extract_relevant_information = prompts.extract_relevant_information.format(
        query=q,
        corpus=corpus[key],
    )

    response = await send_message_to_model_wrapper(
        extract_relevant_information, prompts.system_prompt_extract_relevant_information
    )

    return response.strip()


async def generate_better_image_prompt(q: str, conversation_history: str) -> str:
    """
    Generate a better image prompt from the given query
    """

    image_prompt = prompts.image_generation_improve_prompt.format(
        query=q,
        chat_history=conversation_history,
    )

    response = await send_message_to_model_wrapper(image_prompt)

    return response.strip()


async def send_message_to_model_wrapper(
    message: str,
    system_message: str = "",
):
    conversation_config: ChatModelOptions = await ConversationAdapters.aget_default_conversation_config()

    if conversation_config is None:
        raise HTTPException(status_code=500, detail="Contact the server administrator to set a default chat model.")

    truncated_messages = generate_chatml_messages_with_context(
        user_message=message, system_message=system_message, model_name=conversation_config.chat_model
    )

    if conversation_config.model_type == "offline":
        if state.gpt4all_processor_config is None or state.gpt4all_processor_config.loaded_model is None:
            state.gpt4all_processor_config = GPT4AllProcessorModel(conversation_config.chat_model)

        loaded_model = state.gpt4all_processor_config.loaded_model
        return send_message_to_model_offline(
            message=truncated_messages[-1].content,
            loaded_model=loaded_model,
            model=conversation_config.chat_model,
            streaming=False,
            system_message=truncated_messages[0].content,
        )

    elif conversation_config.model_type == "openai":
        openai_chat_config = await ConversationAdapters.aget_openai_conversation_config()
        api_key = openai_chat_config.api_key
        chat_model = conversation_config.chat_model
        openai_response = send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model,
        )

        return openai_response
    else:
        raise HTTPException(status_code=500, detail="Invalid conversation config")


def generate_chat_response(
    q: str,
    meta_log: dict,
    compiled_references: List[str] = [],
    online_results: Dict[str, Any] = {},
    inferred_queries: List[str] = [],
    conversation_command: ConversationCommand = ConversationCommand.Default,
    user: KhojUser = None,
    client_application: ClientApplication = None,
) -> Tuple[Union[ThreadedGenerator, Iterator[str]], Dict[str, str]]:
    # Initialize Variables
    chat_response = None
    logger.debug(f"Conversation Type: {conversation_command.name}")

    metadata = {}

    try:
        partial_completion = partial(
            save_to_conversation_log,
            q,
            user=user,
            meta_log=meta_log,
            compiled_references=compiled_references,
            online_results=online_results,
            inferred_queries=inferred_queries,
            client_application=client_application,
        )

        conversation_config = ConversationAdapters.get_valid_conversation_config(user)
        if conversation_config.model_type == "offline":
            if state.gpt4all_processor_config is None or state.gpt4all_processor_config.loaded_model is None:
                state.gpt4all_processor_config = GPT4AllProcessorModel(conversation_config.chat_model)

            loaded_model = state.gpt4all_processor_config.loaded_model
            chat_response = converse_offline(
                references=compiled_references,
                online_results=online_results,
                user_query=q,
                loaded_model=loaded_model,
                conversation_log=meta_log,
                completion_func=partial_completion,
                conversation_command=conversation_command,
                model=conversation_config.chat_model,
                max_prompt_size=conversation_config.max_prompt_size,
                tokenizer_name=conversation_config.tokenizer,
            )

        elif conversation_config.model_type == "openai":
            openai_chat_config = ConversationAdapters.get_openai_conversation_config()
            api_key = openai_chat_config.api_key
            chat_model = conversation_config.chat_model
            chat_response = converse(
                compiled_references,
                q,
                online_results=online_results,
                conversation_log=meta_log,
                model=chat_model,
                api_key=api_key,
                completion_func=partial_completion,
                conversation_command=conversation_command,
                max_prompt_size=conversation_config.max_prompt_size,
                tokenizer_name=conversation_config.tokenizer,
            )

        metadata.update({"chat_model": conversation_config.chat_model})

    except Exception as e:
        logger.error(e, exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

    return chat_response, metadata


async def text_to_image(message: str, conversation_log: dict) -> Tuple[Optional[str], int, Optional[str]]:
    status_code = 200
    image = None

    text_to_image_config = await ConversationAdapters.aget_text_to_image_model_config()
    if not text_to_image_config:
        # If the user has not configured a text to image model, return an unsupported on server error
        status_code = 501
    elif state.openai_client and text_to_image_config.model_type == TextToImageModelConfig.ModelType.OPENAI:
        text2image_model = text_to_image_config.model_name
        chat_history = ""
        for chat in conversation_log.get("chat", [])[-4:]:
            if chat["by"] == "khoj" and chat["intent"].get("type") == "remember":
                chat_history += f"Q: {chat['intent']['query']}\n"
                chat_history += f"A: {chat['message']}\n"
        improved_image_prompt = await generate_better_image_prompt(message, chat_history)
        try:
            response = state.openai_client.images.generate(
                prompt=improved_image_prompt, model=text2image_model, response_format="b64_json"
            )
            image = response.data[0].b64_json
        except openai.OpenAIError as e:
            logger.error(f"Image Generation failed with {e}", exc_info=True)
            status_code = 500

    return image, status_code, improved_image_prompt


class ApiUserRateLimiter:
    def __init__(self, requests: int, subscribed_requests: int, window: int, slug: str):
        self.requests = requests
        self.subscribed_requests = subscribed_requests
        self.window = window
        self.slug = slug

    def __call__(self, request: Request):
        # Rate limiting is disabled if user unauthenticated.
        # Other systems handle authentication
        if not request.user.is_authenticated:
            return

        user: KhojUser = request.user.object
        subscribed = has_required_scope(request, ["premium"])

        # Remove requests outside of the time window
        cutoff = datetime.now(tz=timezone.utc) - timedelta(seconds=self.window)
        count_requests = UserRequests.objects.filter(user=user, created_at__gte=cutoff, slug=self.slug).count()

        # Check if the user has exceeded the rate limit
        if subscribed and count_requests >= self.subscribed_requests:
            raise HTTPException(status_code=429, detail="Slow down! Too Many Requests")
        if not subscribed and count_requests >= self.requests:
            if self.subscribed_requests == self.requests:
                raise HTTPException(
                    status_code=429,
                    detail="Slow down! Too Many Requests",
                )
            raise HTTPException(
                status_code=429,
                detail="We're glad you're enjoying Khoj! You've exceeded your usage limit for today. Come back tomorrow or subscribe to increase your rate limit via [your settings](https://app.khoj.dev/config).",
            )

        # Add the current request to the cache
        UserRequests.objects.create(user=user, slug=self.slug)


class ConversationCommandRateLimiter:
    def __init__(self, trial_rate_limit: int, subscribed_rate_limit: int, slug: str):
        self.slug = slug
        self.trial_rate_limit = trial_rate_limit
        self.subscribed_rate_limit = subscribed_rate_limit
        self.restricted_commands = [ConversationCommand.Online, ConversationCommand.Image]

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
        cutoff = datetime.now(tz=timezone.utc) - timedelta(seconds=60 * 60 * 24)
        command_slug = f"{self.slug}_{conversation_command.value}"
        count_requests = await UserRequests.objects.filter(
            user=user, created_at__gte=cutoff, slug=command_slug
        ).acount()

        if subscribed and count_requests >= self.subscribed_rate_limit:
            raise HTTPException(status_code=429, detail="Slow down! Too Many Requests")
        if not subscribed and count_requests >= self.trial_rate_limit:
            raise HTTPException(
                status_code=429,
                detail=f"We're glad you're enjoying Khoj! You've exceeded your `/{conversation_command.value}` command usage limit for today. You can increase your rate limit via [your settings](https://app.khoj.dev/config).",
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

    def __call__(self, request: Request, files: List[UploadFile]):
        if state.billing_enabled is False:
            return
        subscribed = has_required_scope(request, ["premium"])
        incoming_data_size_mb = 0.0
        deletion_file_names = set()

        if not request.user.is_authenticated:
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
            raise HTTPException(status_code=429, detail="Too much data indexed.")
        if not subscribed and incoming_data_size_mb >= self.num_entries_size:
            raise HTTPException(
                status_code=429, detail="Too much data indexed. Subscribe to increase your data index limit."
            )

        user_size_data = EntryAdapters.get_size_of_indexed_data_in_mb(user)
        if subscribed and user_size_data + incoming_data_size_mb >= self.subscribed_total_entries_size:
            raise HTTPException(status_code=429, detail="Too much data indexed.")
        if not subscribed and user_size_data + incoming_data_size_mb >= self.total_entries_size_limit:
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
