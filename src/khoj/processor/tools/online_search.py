import json
import logging
import os
from typing import Dict, List, Union

import requests

from khoj.routers.helpers import extract_relevant_info, generate_online_subqueries
from khoj.utils.helpers import is_none_or_empty

logger = logging.getLogger(__name__)

SERPER_DEV_API_KEY = os.getenv("SERPER_DEV_API_KEY")
OLOSTEP_API_KEY = os.getenv("OLOSTEP_API_KEY")

SERPER_DEV_URL = "https://google.serper.dev/search"

OLOSTEP_API_URL = "https://agent.olostep.com/olostep-p2p-incomingAPI"

OLOSTEP_QUERY_PARAMS = {
    "timeout": 35,  # seconds
    "waitBeforeScraping": 1,  # seconds
    "saveHtml": False,
    "saveMarkdown": True,
    "removeCSSselectors": "default",
    "htmlTransformer": "none",
    "removeImages": True,
    "fastLane": True,
    # Similar to Stripe's API, the expand parameters avoid the need to make a second API call
    # to retrieve the dataset (from the dataset API) if you only need the markdown or html.
    "expandMarkdown": True,
    "expandHtml": False,
}


async def search_with_google(query: str, conversation_history: dict):
    def _search_with_google(subquery: str):
        payload = json.dumps(
            {
                "q": subquery,
            }
        )

        headers = {"X-API-KEY": SERPER_DEV_API_KEY, "Content-Type": "application/json"}

        response = requests.request("POST", SERPER_DEV_URL, headers=headers, data=payload)

        if response.status_code != 200:
            logger.error(response.text)
            return {}

        json_response = response.json()
        sub_response_dict = {}
        sub_response_dict["knowledgeGraph"] = json_response.get("knowledgeGraph", {})
        sub_response_dict["organic"] = json_response.get("organic", [])
        sub_response_dict["answerBox"] = json_response.get("answerBox", [])
        sub_response_dict["peopleAlsoAsk"] = json_response.get("peopleAlsoAsk", [])

        return sub_response_dict

    if SERPER_DEV_API_KEY is None:
        raise ValueError("SERPER_DEV_API_KEY is not set")

    # Breakdown the query into subqueries to get the correct answer
    subqueries = await generate_online_subqueries(query, conversation_history)

    response_dict = {}

    for subquery in subqueries:
        logger.info(f"Searching with Google for '{subquery}'")
        response_dict[subquery] = _search_with_google(subquery)

    extracted_content: Dict[str, List] = {}
    if is_none_or_empty(OLOSTEP_API_KEY):
        logger.warning("OLOSTEP_API_KEY is not set. Skipping web scraping.")
        return response_dict

    for subquery in response_dict:
        # If a high quality answer is not found, search the web pages of the first 3 organic results
        if is_none_or_empty(response_dict[subquery].get("answerBox")):
            extracted_content[subquery] = []
            for result in response_dict[subquery].get("organic")[:1]:
                logger.info(f"Searching web page of '{result['link']}'")
                try:
                    extracted_content[subquery].append(search_with_olostep(result["link"]).strip())
                except Exception as e:
                    logger.error(f"Error while searching web page of '{result['link']}': {e}", exc_info=True)
                    continue
            extracted_relevant_content = await extract_relevant_info(subquery, extracted_content)
            response_dict[subquery]["extracted_content"] = extracted_relevant_content

    return response_dict


def search_with_olostep(web_url: str) -> str:
    if OLOSTEP_API_KEY is None:
        raise ValueError("OLOSTEP_API_KEY is not set")

    headers = {"Authorization": f"Bearer {OLOSTEP_API_KEY}"}

    web_scraping_params: Dict[str, Union[str, int, bool]] = OLOSTEP_QUERY_PARAMS.copy()  # type: ignore
    web_scraping_params["url"] = web_url

    response = requests.request("GET", OLOSTEP_API_URL, params=web_scraping_params, headers=headers)

    if response.status_code != 200:
        logger.error(response, exc_info=True)
        return None

    return response.json()["markdown_content"]
