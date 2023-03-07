# Standard Packages
import json
import logging
from typing import Optional

# External Packages
import schedule
from fastapi import APIRouter

# Internal Packages
from khoj.routers.api import search
from khoj.processor.conversation.gpt import (
    answer,
    converse,
    extract_search_type,
    message_to_log,
    message_to_prompt,
    summarize,
)
from khoj.utils.state import SearchType
from khoj.utils.helpers import get_from_dict, resolve_absolute_path
from khoj.utils import state


# Initialize Router
api_beta = APIRouter()
logger = logging.getLogger(__name__)


# Create Routes
@api_beta.get("/search")
def search_beta(q: str, n: Optional[int] = 1):
    # Initialize Variables
    model = state.processor_config.conversation.model
    api_key = state.processor_config.conversation.openai_api_key

    # Extract Search Type using GPT
    try:
        metadata = extract_search_type(q, model=model, api_key=api_key, verbose=state.verbose)
        search_type = get_from_dict(metadata, "search-type")
    except Exception as e:
        return {"status": "error", "result": [str(e)], "type": None}

    # Search
    search_results = search(q, n=n, t=SearchType(search_type))

    # Return response
    return {"status": "ok", "result": search_results, "type": search_type}


@api_beta.get("/answer")
def answer_beta(q: str):
    # Initialize Variables
    model = state.processor_config.conversation.model
    api_key = state.processor_config.conversation.openai_api_key

    # Collate context for GPT
    result_list = search(q, n=2, r=True, score_threshold=0, dedupe=False)
    collated_result = "\n\n".join([f"# {item.additional['compiled']}" for item in result_list])
    logger.debug(f"Reference Context:\n{collated_result}")

    # Make GPT respond to user query using provided context
    try:
        gpt_response = answer(collated_result, user_query=q, model=model, api_key=api_key)
        status = "ok"
    except Exception as e:
        gpt_response = str(e)
        status = "error"

    return {"status": status, "response": gpt_response}


@api_beta.get("/chat")
def chat(q: Optional[str] = None):
    # Initialize Variables
    api_key = state.processor_config.conversation.openai_api_key

    # Load Conversation History
    chat_session = state.processor_config.conversation.chat_session
    meta_log = state.processor_config.conversation.meta_log

    # If user query is empty, return chat history
    if not q:
        if meta_log.get("chat"):
            return {"status": "ok", "response": meta_log["chat"]}
        else:
            return {"status": "ok", "response": []}

    # Collate context for GPT
    result_list = search(q, n=2, r=True, score_threshold=0, dedupe=False)
    collated_result = "\n\n".join([f"# {item.additional['compiled']}" for item in result_list])
    logger.debug(f"Reference Context:\n{collated_result}")

    try:
        gpt_response = converse(collated_result, q, meta_log, api_key=api_key)
        status = "ok"
    except Exception as e:
        gpt_response = str(e)
        status = "error"

    # Update Conversation History
    state.processor_config.conversation.chat_session = message_to_prompt(q, chat_session, gpt_message=gpt_response)
    state.processor_config.conversation.meta_log["chat"] = message_to_log(
        q, gpt_response, user_message_metadata={"context": collated_result}, conversation_log=meta_log.get("chat", [])
    )

    return {"status": status, "response": gpt_response}


@schedule.repeat(schedule.every(5).minutes)
def save_chat_session():
    # No need to create empty log file
    if not (
        state.processor_config
        and state.processor_config.conversation
        and state.processor_config.conversation.meta_log
        and state.processor_config.conversation.chat_session
    ):
        return

    # Summarize Conversation Logs for this Session
    chat_session = state.processor_config.conversation.chat_session
    openai_api_key = state.processor_config.conversation.openai_api_key
    conversation_log = state.processor_config.conversation.meta_log
    model = state.processor_config.conversation.model
    session = {
        "summary": summarize(chat_session, summary_type="chat", model=model, api_key=openai_api_key),
        "session-start": conversation_log.get("session", [{"session-end": 0}])[-1]["session-end"],
        "session-end": len(conversation_log["chat"]),
    }
    if "session" in conversation_log:
        conversation_log["session"].append(session)
    else:
        conversation_log["session"] = [session]

    # Save Conversation Metadata Logs to Disk
    conversation_logfile = resolve_absolute_path(state.processor_config.conversation.conversation_logfile)
    conversation_logfile.parent.mkdir(parents=True, exist_ok=True)  # create conversation directory if doesn't exist
    with open(conversation_logfile, "w+", encoding="utf-8") as logfile:
        json.dump(conversation_log, logfile)

    state.processor_config.conversation.chat_session = None
    logger.info("📩 Saved current chat session to conversation logs")
