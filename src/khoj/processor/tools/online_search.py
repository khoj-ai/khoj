import asyncio
import json
import logging
import os
from typing import Dict, Union

import aiohttp
import requests
from bs4 import BeautifulSoup
from markdownify import markdownify

from khoj.routers.helpers import extract_relevant_info, generate_online_subqueries
from khoj.utils.helpers import is_none_or_empty, timer
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)

SERPER_DEV_API_KEY = os.getenv("SERPER_DEV_API_KEY")
OLOSTEP_API_KEY = os.getenv("OLOSTEP_API_KEY")

SERPER_DEV_URL = "https://google.serper.dev/search"

OLOSTEP_API_URL = "https://agent.olostep.com/olostep-p2p-incomingAPI"

OLOSTEP_QUERY_PARAMS = {
    "timeout": 35,  # seconds
    "waitBeforeScraping": 1,  # seconds
    "saveHtml": "False",
    "saveMarkdown": "True",
    "removeCSSselectors": "default",
    "htmlTransformer": "none",
    "removeImages": "True",
    "fastLane": "True",
    # Similar to Stripe's API, the expand parameters avoid the need to make a second API call
    # to retrieve the dataset (from the dataset API) if you only need the markdown or html.
    "expandMarkdown": "True",
    "expandHtml": "False",
}
MAX_WEBPAGES_TO_READ = 1


async def search_with_google(query: str, conversation_history: dict, location: LocationData):
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
        logger.warn("SERPER_DEV_API_KEY is not set")
        return {}

    # Breakdown the query into subqueries to get the correct answer
    subqueries = await generate_online_subqueries(query, conversation_history, location)
    response_dict = {}

    for subquery in subqueries:
        logger.info(f"Searching with Google for '{subquery}'")
        response_dict[subquery] = _search_with_google(subquery)

    # Gather distinct web pages from organic search results of each subquery without an instant answer
    webpage_links = {
        result["link"]
        for subquery in response_dict
        for result in response_dict[subquery].get("organic")[:MAX_WEBPAGES_TO_READ]
        if is_none_or_empty(response_dict[subquery].get("answerBox"))
    }

    # Read, extract relevant info from the retrieved web pages
    tasks = []
    for webpage_link in webpage_links:
        logger.info(f"Reading web page at '{webpage_link}'")
        task = read_webpage_and_extract_content(subquery, webpage_link)
        tasks.append(task)
    results = await asyncio.gather(*tasks)

    # Collect extracted info from the retrieved web pages
    for subquery, extracted_webpage_content in results:
        if extracted_webpage_content is not None:
            response_dict[subquery]["extracted_content"] = extracted_webpage_content

    return response_dict


async def read_webpage_and_extract_content(subquery, url):
    try:
        with timer(f"Reading web page at '{url}' took", logger):
            content = await read_webpage_with_olostep(url) if OLOSTEP_API_KEY else await read_webpage(url)
        with timer(f"Extracting relevant information from web page at '{url}' took", logger):
            extracted_info = await extract_relevant_info(subquery, {subquery: [content.strip()]}) if content else None
        return subquery, extracted_info
    except Exception as e:
        logger.error(f"Failed to read web page at '{url}': {e}", exc_info=True)
        return subquery, None


async def read_webpage(web_url: str) -> str:
    headers = {
        "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36",
    }

    async with aiohttp.ClientSession() as session:
        async with session.get(web_url, headers=headers, timeout=30) as response:
            response.raise_for_status()
            html = await response.text()
            parsed_html = BeautifulSoup(html, "html.parser")
            body = parsed_html.body.get_text(separator="\n", strip=True)
            return markdownify(body)


async def read_webpage_with_olostep(web_url: str) -> str:
    headers = {"Authorization": f"Bearer {OLOSTEP_API_KEY}"}
    web_scraping_params: Dict[str, Union[str, int, bool]] = OLOSTEP_QUERY_PARAMS.copy()  # type: ignore
    web_scraping_params["url"] = web_url

    async with aiohttp.ClientSession() as session:
        async with session.get(OLOSTEP_API_URL, params=web_scraping_params, headers=headers) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["markdown_content"]
