# Standard Packages
import json
import logging
from typing import Optional

# External Packages
from fastapi import APIRouter

# Internal Packages
from src.routers.api_v1_0 import search
from src.processor.conversation.gpt import converse, extract_search_type, message_to_log, message_to_prompt, understand, summarize
from src.utils.config import SearchType
from src.utils.helpers import get_absolute_path, get_from_dict
from src.utils import state


api_beta = APIRouter()
logger = logging.getLogger(__name__)


@api_beta.get('/search')
def search_beta(q: str, n: Optional[int] = 1):
    # Extract Search Type using GPT
    metadata = extract_search_type(q, api_key=state.processor_config.conversation.openai_api_key, verbose=state.verbose)
    search_type = get_from_dict(metadata, "search-type")

    # Search
    search_results = search(q, n=n, t=SearchType(search_type))

    # Return response
    return {'status': 'ok', 'result': search_results, 'type': search_type}


@api_beta.get('/chat')
def chat(q: str):
    # Load Conversation History
    chat_session = state.processor_config.conversation.chat_session
    meta_log = state.processor_config.conversation.meta_log

    # Converse with OpenAI GPT
    metadata = understand(q, api_key=state.processor_config.conversation.openai_api_key, verbose=state.verbose)
    if state.verbose > 1:
        print(f'Understood: {get_from_dict(metadata, "intent")}')

    if get_from_dict(metadata, "intent", "memory-type") == "notes":
        query = get_from_dict(metadata, "intent", "query")
        result_list = search(query, n=1, t=SearchType.Org)
        collated_result = "\n".join([item["entry"] for item in result_list])
        if state.verbose > 1:
            print(f'Semantically Similar Notes:\n{collated_result}')
        gpt_response = summarize(collated_result, summary_type="notes", user_query=q, api_key=state.processor_config.conversation.openai_api_key)
    else:
        gpt_response = converse(q, chat_session, api_key=state.processor_config.conversation.openai_api_key)

    # Update Conversation History
    state.processor_config.conversation.chat_session = message_to_prompt(q, chat_session, gpt_message=gpt_response)
    state.processor_config.conversation.meta_log['chat'] = message_to_log(q, metadata, gpt_response, meta_log.get('chat', []))

    return {'status': 'ok', 'response': gpt_response}


@api_beta.on_event('shutdown')
def shutdown_event():
    # No need to create empty log file
    if not (state.processor_config and state.processor_config.conversation and state.processor_config.conversation.meta_log):
        return
    elif state.processor_config.conversation.verbose:
        print('INFO:\tSaving conversation logs to disk...')

    # Summarize Conversation Logs for this Session
    chat_session = state.processor_config.conversation.chat_session
    openai_api_key = state.processor_config.conversation.openai_api_key
    conversation_log = state.processor_config.conversation.meta_log
    session = {
        "summary": summarize(chat_session, summary_type="chat", api_key=openai_api_key),
        "session-start": conversation_log.get("session", [{"session-end": 0}])[-1]["session-end"],
        "session-end": len(conversation_log["chat"])
        }
    if 'session' in conversation_log:
        conversation_log['session'].append(session)
    else:
        conversation_log['session'] = [session]

    # Save Conversation Metadata Logs to Disk
    conversation_logfile = get_absolute_path(state.processor_config.conversation.conversation_logfile)
    with open(conversation_logfile, "w+", encoding='utf-8') as logfile:
        json.dump(conversation_log, logfile)

    print('INFO:\tConversation logs saved to disk.')
