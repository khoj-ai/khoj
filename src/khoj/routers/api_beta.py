# Standard Packages
import logging
from typing import Optional

# External Packages
from fastapi import APIRouter

# Internal Packages
from khoj.routers.api import search
from khoj.processor.conversation.gpt import (
    answer,
    extract_search_type,
)
from khoj.utils.state import SearchType
from khoj.utils.helpers import get_from_dict
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
