import logging
import asyncio
from datetime import datetime
from functools import partial
from typing import Iterator, List, Optional, Union
from concurrent.futures import ThreadPoolExecutor

from fastapi import HTTPException, Request

from khoj.utils import state
from khoj.utils.config import GPT4AllProcessorModel
from khoj.utils.helpers import ConversationCommand, log_telemetry
from khoj.processor.conversation.openai.gpt import converse
from khoj.processor.conversation.gpt4all.chat_model import converse_offline
from khoj.processor.conversation.utils import message_to_log, ThreadedGenerator
from database.models import KhojUser
from database.adapters import ConversationAdapters

logger = logging.getLogger(__name__)

executor = ThreadPoolExecutor(max_workers=1)


def perform_chat_checks(user: KhojUser):
    if ConversationAdapters.has_valid_offline_conversation_config(
        user
    ) or ConversationAdapters.has_valid_openai_conversation_config(user):
        return

    raise HTTPException(status_code=500, detail="Set your OpenAI API key or enable Local LLM via Khoj settings.")


async def is_ready_to_chat(user: KhojUser):
    has_offline_config = await ConversationAdapters.has_offline_chat(user=user)
    has_openai_config = await ConversationAdapters.has_openai_chat(user=user)

    if has_offline_config:
        offline_chat = await ConversationAdapters.get_offline_chat(user)
        chat_model = offline_chat.chat_model
        if state.gpt4all_processor_config is None:
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
    user_state = {
        "client_host": request.client.host if request.client else None,
        "user_agent": user_agent or "unknown",
        "referer": referer or "unknown",
        "host": host or "unknown",
        "server_id": str(user.uuid) if user else None,
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
) -> Union[ThreadedGenerator, Iterator[str]]:
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

    try:
        partial_completion = partial(
            _save_to_conversation_log,
            q,
            user_message_time=user_message_time,
            compiled_references=compiled_references,
            inferred_queries=inferred_queries,
            meta_log=meta_log,
        )

        offline_chat_config = ConversationAdapters.get_offline_chat_conversation_config(user=user)
        conversation_config = ConversationAdapters.get_conversation_config(user)
        openai_chat_config = ConversationAdapters.get_openai_conversation_config(user)
        if offline_chat_config:
            if state.gpt4all_processor_config.loaded_model is None:
                state.gpt4all_processor_config = GPT4AllProcessorModel(offline_chat_config.chat_model)

            loaded_model = state.gpt4all_processor_config.loaded_model
            chat_response = converse_offline(
                references=compiled_references,
                user_query=q,
                loaded_model=loaded_model,
                conversation_log=meta_log,
                completion_func=partial_completion,
                conversation_command=conversation_command,
                model=offline_chat_config.chat_model,
                max_prompt_size=conversation_config.max_prompt_size,
                tokenizer_name=conversation_config.tokenizer,
            )

        elif openai_chat_config:
            api_key = openai_chat_config.api_key
            chat_model = openai_chat_config.chat_model
            chat_response = converse(
                compiled_references,
                q,
                meta_log,
                model=chat_model,
                api_key=api_key,
                completion_func=partial_completion,
                conversation_command=conversation_command,
                max_prompt_size=conversation_config.max_prompt_size if conversation_config else None,
                tokenizer_name=conversation_config.tokenizer if conversation_config else None,
            )

    except Exception as e:
        logger.error(e, exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

    return chat_response
