# Standard Packages
import yaml
import time
import logging
from typing import Optional

# External Packages
from fastapi import APIRouter

# Internal Packages
from src.configure import configure_search
from src.search_type import image_search, text_search
from src.utils.rawconfig import FullConfig, SearchResponse
from src.utils.config import SearchType
from src.utils import state, constants


# Initialize Router
api = APIRouter()
logger = logging.getLogger(__name__)


# Create Routes
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

@api.get('/search', response_model=list[SearchResponse])
def search(q: str, n: Optional[int] = 5, t: Optional[SearchType] = None, r: Optional[bool] = False):
    results: list[SearchResponse] = []
    if q is None or q == '':
        logger.info(f'No query param (q) passed in API call to initiate search')
        return results

    # initialize variables
    user_query = q.strip()
    results_count = n
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


@api.get('/update')
def update(t: Optional[SearchType] = None, force: Optional[bool] = False):
    state.model = configure_search(state.model, state.config, regenerate=force, t=t)
    return {'status': 'ok', 'message': 'index updated'}
