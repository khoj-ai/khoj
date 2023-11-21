# Standard Packages
import asyncio
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from functools import partial
import logging
from time import time
from typing import Annotated, Iterator, List, Optional, Union, Tuple, Dict

# External Packages
from fastapi import HTTPException, Header, Request, Depends

# Internal Packages
from khoj.utils import state
from khoj.utils.config import GPT4AllProcessorModel
from khoj.utils.helpers import ConversationCommand, log_telemetry
from khoj.processor.conversation.openai.gpt import converse
from khoj.processor.conversation.gpt4all.chat_model import converse_offline
from khoj.processor.conversation.utils import message_to_log, ThreadedGenerator
from khoj.database.models import KhojUser, Subscription
from khoj.database.adapters import ConversationAdapters


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
    subscription: Subscription = user.subscription if user and user.subscription else None
    user_state = {
        "client_host": request.client.host if request.client else None,
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
        "server_id": str(user.uuid) if user else None,
        "subscription_type": subscription.type if subscription else None,
        "is_recurring": subscription.is_recurring if subscription else None,
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
    # If no relevant notes found for the given query
    elif not any_references:
        return ConversationCommand.General
    else:
        return ConversationCommand.Default


async def construct_conversation_logs(user: KhojUser):
    return (await ConversationAdapters.aget_conversation_by_user(user)).conversation_log


async def agenerate_chat_response(*args):
    loop = asyncio.get_event_loop()
    return await loop.run_in_executor(executor, generate_chat_response, *args)


def generate_chat_response(
    q: str,
    meta_log: dict,
    compiled_references: List[str] = [],
    inferred_queries: List[str] = [],
    conversation_command: ConversationCommand = ConversationCommand.Default,
    user: KhojUser = None,
) -> Tuple[Union[ThreadedGenerator, Iterator[str]], Dict[str, str]]:
    def _save_to_conversation_log(
        q: str,
        chat_response: str,
        user_message_time: str,
        compiled_references: List[str],
        inferred_queries: List[str],
        meta_log,
    ):
        updated_conversation = message_to_log(
            user_message=q,
            chat_response=chat_response,
            user_message_metadata={"created": user_message_time},
            khoj_message_metadata={"context": compiled_references, "intent": {"inferred-queries": inferred_queries}},
            conversation_log=meta_log.get("chat", []),
        )
        ConversationAdapters.save_conversation(user, {"chat": updated_conversation})

    # Initialize Variables
    user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    chat_response = None
    logger.debug(f"Conversation Type: {conversation_command.name}")

    metadata = {}

    try:
        partial_completion = partial(
            _save_to_conversation_log,
            q,
            user_message_time=user_message_time,
            compiled_references=compiled_references,
            inferred_queries=inferred_queries,
            meta_log=meta_log,
        )

        offline_chat_config = ConversationAdapters.get_offline_chat_conversation_config()
        conversation_config = ConversationAdapters.get_conversation_config(user)
        if conversation_config is None:
            conversation_config = ConversationAdapters.get_default_conversation_config()
        openai_chat_config = ConversationAdapters.get_openai_conversation_config()
        if offline_chat_config and offline_chat_config.enabled and conversation_config.model_type == "offline":
            if state.gpt4all_processor_config is None or state.gpt4all_processor_config.loaded_model is None:
                state.gpt4all_processor_config = GPT4AllProcessorModel(conversation_config.chat_model)

            loaded_model = state.gpt4all_processor_config.loaded_model
            chat_response = converse_offline(
                references=compiled_references,
                user_query=q,
                loaded_model=loaded_model,
                conversation_log=meta_log,
                completion_func=partial_completion,
                conversation_command=conversation_command,
                model=conversation_config.chat_model,
                max_prompt_size=conversation_config.max_prompt_size,
                tokenizer_name=conversation_config.tokenizer,
            )

        elif openai_chat_config and conversation_config.model_type == "openai":
            api_key = openai_chat_config.api_key
            chat_model = conversation_config.chat_model
            chat_response = converse(
                compiled_references,
                q,
                meta_log,
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


class ApiUserRateLimiter:
    def __init__(self, requests: int, window: int):
        self.requests = requests
        self.window = window
        self.cache: dict[str, list[float]] = defaultdict(list)

    def __call__(self, request: Request):
        user: KhojUser = request.user.object
        user_requests = self.cache[user.uuid]

        # Remove requests outside of the time window
        cutoff = time() - self.window
        while user_requests and user_requests[0] < cutoff:
            user_requests.pop(0)

        # Check if the user has exceeded the rate limit
        if len(user_requests) >= self.requests:
            raise HTTPException(status_code=429, detail="Too Many Requests")

        # Add the current request to the cache
        user_requests.append(time())


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
