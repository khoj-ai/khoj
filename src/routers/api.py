# Standard Packages
import yaml
import logging
from typing import List, Optional

# External Packages
from fastapi import APIRouter
from fastapi import HTTPException

# Internal Packages
from src.configure import configure_processor, configure_search
from src.search_type import image_search, text_search
from src.utils.helpers import timer
from src.utils.rawconfig import FullConfig, SearchResponse
from src.utils.config import SearchType
from src.utils import state, constants


# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)


# Create Routes
@api.get('/config/data/default')
def get_default_config_data():
    return constants.default_config

@api.get('/config/data', response_model=FullConfig)
def get_config_data():
    return state.config

@api.post('/config/data')
async def set_config_data(updated_config: FullConfig):
    state.config = updated_config
    with open(state.config_file, 'w') as outfile:
        yaml.dump(yaml.safe_load(state.config.json(by_alias=True)), outfile)
        outfile.close()
    return state.config

@api.get('/search', response_model=List[SearchResponse])
def search(q: str, n: Optional[int] = 5, t: Optional[SearchType] = None, r: Optional[bool] = False):
    results: List[SearchResponse] = []
    if q is None or q == '':
        logger.info(f'No query param (q) passed in API call to initiate search')
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n

    # return cached results, if available
    query_cache_key = f'{user_query}-{n}-{t}-{r}'
    if query_cache_key in state.query_cache:
        logger.info(f'Return response from query cache')
        return state.query_cache[query_cache_key]

    if (t == SearchType.Org or t == None) and state.model.orgmode_search:
        # query org-mode notes
        with timer("Query took", logger):
            hits, entries = text_search.query(user_query, state.model.orgmode_search, rank_results=r)

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Markdown or t == None) and state.model.markdown_search:
        # query markdown files
        with timer("Query took", logger):
            hits, entries = text_search.query(user_query, state.model.markdown_search, rank_results=r)

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Ledger or t == None) and state.model.ledger_search:
        # query transactions
        with timer("Query took", logger):
            hits, entries = text_search.query(user_query, state.model.ledger_search, rank_results=r)

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Music or t == None) and state.model.music_search:
        # query music library
        with timer("Query took", logger):
            hits, entries = text_search.query(user_query, state.model.music_search, rank_results=r)

        # collate and return results
        with timer("Collating results took", logger):
            results = text_search.collate_results(hits, entries, results_count)

    elif (t == SearchType.Image or t == None) and state.model.image_search:
        # query images
        with timer("Query took", logger):
            hits = image_search.query(user_query, results_count, state.model.image_search)
            output_directory = constants.web_directory / 'images'

        # collate and return results
        with timer("Collating results took", logger):
            results = image_search.collate_results(
                hits,
                image_names=state.model.image_search.image_names,
                output_directory=output_directory,
                image_files_url='/static/images',
                count=results_count)

    # Cache results
    state.query_cache[query_cache_key] = results

    return results


@api.get('/update')
def update(t: Optional[SearchType] = None, force: Optional[bool] = False):
    try:
        state.search_index_lock.acquire()
        state.model = configure_search(state.model, state.config, regenerate=force, t=t)
        state.search_index_lock.release()
    except ValueError as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))
    else:
        logger.info("Search Index updated via API call")

    try:
        state.processor_config = configure_processor(state.config.processor)
    except ValueError as e:
        logger.error(e)
        raise HTTPException(status_code=500, detail=str(e))
    else:
        logger.info("Processor reconfigured via API call")

    return {'status': 'ok', 'message': 'khoj reloaded'}
