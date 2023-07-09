from fastapi import HTTPException
import logging
from datetime import datetime
from functools import partial
from typing import List

from khoj.utils import state
from khoj.utils.helpers import timer
from khoj.processor.conversation.gpt import converse
from khoj.processor.conversation.utils import message_to_log, reciprocal_conversation_to_chatml


logger = logging.getLogger(__name__)


def perform_chat_checks():
    if (
        state.processor_config is None
        or state.processor_config.conversation is None
        or state.processor_config.conversation.openai_api_key is None
    ):
        raise HTTPException(
            status_code=500, detail="Set your OpenAI API key via Khoj settings and restart it to use Khoj Chat."
        )


def generate_chat_response(
    q: str,
    meta_log: dict,
    compiled_references: List[str] = [],
    inferred_queries: List[str] = [],
):
    def _save_to_conversation_log(
        q: str,
        gpt_response: str,
        user_message_time: str,
        compiled_references: List[str],
        inferred_queries: List[str],
        meta_log,
    ):
        state.processor_config.conversation.chat_session += reciprocal_conversation_to_chatml([q, gpt_response])
        state.processor_config.conversation.meta_log["chat"] = message_to_log(
            q,
            gpt_response,
            user_message_metadata={"created": user_message_time},
            khoj_message_metadata={"context": compiled_references, "intent": {"inferred-queries": inferred_queries}},
            conversation_log=meta_log.get("chat", []),
        )

    # Load Conversation History
    meta_log = state.processor_config.conversation.meta_log

    # Initialize Variables
    api_key = state.processor_config.conversation.openai_api_key
    chat_model = state.processor_config.conversation.chat_model
    user_message_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    conversation_type = "general" if q.startswith("@general") else "notes"

    # Switch to general conversation type if no relevant notes found for the given query
    conversation_type = "notes" if compiled_references else "general"
    logger.debug(f"Conversation Type: {conversation_type}")

    try:
        with timer("Generating chat response took", logger):
            partial_completion = partial(
                _save_to_conversation_log,
                q,
                user_message_time=user_message_time,
                compiled_references=compiled_references,
                inferred_queries=inferred_queries,
                meta_log=meta_log,
            )

            gpt_response = converse(
                compiled_references, q, meta_log, model=chat_model, api_key=api_key, completion_func=partial_completion
            )

    except Exception as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))

    return gpt_response
