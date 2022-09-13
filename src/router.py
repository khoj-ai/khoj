# Standard Packages
import yaml
import json
import time
import logging
from typing import Optional

# External Packages
from fastapi import APIRouter
from fastapi import Request
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.templating import Jinja2Templates

# Internal Packages
from src.configure import configure_search
from src.search_type import image_search, text_search
from src.processor.conversation.gpt import converse, extract_search_type, message_to_log, message_to_prompt, understand, summarize
from src.utils.rawconfig import FullConfig
from src.utils.config import SearchType
from src.utils.helpers import LRU, get_absolute_path, get_from_dict
from src.utils import state, constants


router = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)
logger = logging.getLogger(__name__)
query_cache = LRU()


@router.get("/", response_class=FileResponse)
def index():
    return FileResponse(constants.web_directory / "index.html")

@router.get('/config', response_class=HTMLResponse)
def config_page(request: Request):
    return templates.TemplateResponse("config.html", context={'request': request})

@router.get('/config/data', response_model=FullConfig)
def config_data():
    return state.config

@router.post('/config/data')
async def config_data(updated_config: FullConfig):
    state.config = updated_config
    with open(state.config_file, 'w') as outfile:
        yaml.dump(yaml.safe_load(state.config.json(by_alias=True)), outfile)
        outfile.close()
    return state.config

@router.get('/search')
def search(q: str, n: Optional[int] = 5, t: Optional[SearchType] = None, r: Optional[bool] = False):
    if q is None or q == '':
        logger.info(f'No query param (q) passed in API call to initiate search')
        return {}

    # initialize variables
    user_query = q.strip()
    results_count = n
    results = {}
    query_start, query_end, collate_start, collate_end = None, None, None, None

    # return cached results, if available
    query_cache_key = f'{user_query}-{n}-{t}-{r}'
    if query_cache_key in state.query_cache:
        logger.info(f'Return response from query cache')
        return state.query_cache[query_cache_key]

    if (t == SearchType.Org or t == None) and state.model.orgmode_search:
        # query org-mode notes
        query_start = time.time()
        hits, entries = text_search.query(user_query, state.model.orgmode_search, rank_results=r)
        query_end = time.time()

        # collate and return results
        collate_start = time.time()
        results = text_search.collate_results(hits, entries, results_count)
        collate_end = time.time()

    if (t == SearchType.Music or t == None) and state.model.music_search:
        # query music library
        query_start = time.time()
        hits, entries = text_search.query(user_query, state.model.music_search, rank_results=r)
        query_end = time.time()

        # collate and return results
        collate_start = time.time()
        results = text_search.collate_results(hits, entries, results_count)
        collate_end = time.time()

    if (t == SearchType.Markdown or t == None) and state.model.markdown_search:
        # query markdown files
        query_start = time.time()
        hits, entries = text_search.query(user_query, state.model.markdown_search, rank_results=r)
        query_end = time.time()

        # collate and return results
        collate_start = time.time()
        results = text_search.collate_results(hits, entries, results_count)
        collate_end = time.time()

    if (t == SearchType.Ledger or t == None) and state.model.ledger_search:
        # query transactions
        query_start = time.time()
        hits, entries = text_search.query(user_query, state.model.ledger_search, rank_results=r)
        query_end = time.time()

        # collate and return results
        collate_start = time.time()
        results = text_search.collate_results(hits, entries, results_count)
        collate_end = time.time()

    if (t == SearchType.Image or t == None) and state.model.image_search:
        # query images
        query_start = time.time()
        hits = image_search.query(user_query, results_count, state.model.image_search)
        output_directory = constants.web_directory / 'images'
        query_end = time.time()

        # collate and return results
        collate_start = time.time()
        results = image_search.collate_results(
            hits,
            image_names=state.model.image_search.image_names,
            output_directory=output_directory,
            image_files_url='/static/images',
            count=results_count)
        collate_end = time.time()

    # Cache results
    state.query_cache[query_cache_key] = results

    if query_start and query_end:
        logger.debug(f"Query took {query_end - query_start:.3f} seconds")
    if collate_start and collate_end:
        logger.debug(f"Collating results took {collate_end - collate_start:.3f} seconds")

    return results


@router.get('/reload')
def reload(t: Optional[SearchType] = None):
    state.model = configure_search(state.model, state.config, regenerate=False, t=t)
    return {'status': 'ok', 'message': 'reload completed'}


@router.get('/regenerate')
def regenerate(t: Optional[SearchType] = None):
    state.model = configure_search(state.model, state.config, regenerate=True, t=t)
    return {'status': 'ok', 'message': 'regeneration completed'}


@router.get('/beta/search')
def search_beta(q: str, n: Optional[int] = 1):
    # Extract Search Type using GPT
    metadata = extract_search_type(q, api_key=state.processor_config.conversation.openai_api_key, verbose=state.verbose)
    search_type = get_from_dict(metadata, "search-type")

    # Search
    search_results = search(q, n=n, t=SearchType(search_type))

    # Return response
    return {'status': 'ok', 'result': search_results, 'type': search_type}


@router.get('/beta/chat')
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


@router.on_event('shutdown')
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
