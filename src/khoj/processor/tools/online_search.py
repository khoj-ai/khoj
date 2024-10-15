import asyncio
import json
import logging
import os
import urllib.parse
from collections import defaultdict
from typing import Callable, Dict, List, Optional, Tuple, Union

import aiohttp
from bs4 import BeautifulSoup
from markdownify import markdownify

from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation import prompts
from khoj.routers.helpers import (
    ChatEvent,
    extract_relevant_info,
    generate_online_subqueries,
    infer_webpage_urls,
)
from khoj.utils.helpers import is_internet_connected, is_none_or_empty, timer
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)

SERPER_DEV_API_KEY = os.getenv("SERPER_DEV_API_KEY")
SERPER_DEV_URL = "https://google.serper.dev/search"

JINA_READER_API_URL = "https://r.jina.ai/"
JINA_SEARCH_API_URL = "https://s.jina.ai/"
JINA_API_KEY = os.getenv("JINA_API_KEY")

FIRECRAWL_API_KEY = os.getenv("FIRECRAWL_API_KEY")
FIRECRAWL_API_URL = os.getenv("FIRECRAWL_API_URL", "https://api.firecrawl.dev")
FIRECRAWL_TO_EXTRACT = os.getenv("FIRECRAWL_TO_EXTRACT", "False").lower() == "true"

OLOSTEP_API_KEY = os.getenv("OLOSTEP_API_KEY")
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


async def search_online(
    query: str,
    conversation_history: dict,
    location: LocationData,
    user: KhojUser,
    send_status_func: Optional[Callable] = None,
    custom_filters: List[str] = [],
    uploaded_image_url: str = None,
    agent: Agent = None,
):
    query += " ".join(custom_filters)
    if not is_internet_connected():
        logger.warning("Cannot search online as not connected to internet")
        yield {}
        return

    # Breakdown the query into subqueries to get the correct answer
    subqueries = await generate_online_subqueries(
        query, conversation_history, location, user, uploaded_image_url=uploaded_image_url, agent=agent
    )
    response_dict = {}

    if subqueries:
        logger.info(f"🌐 Searching the Internet for {list(subqueries)}")
        if send_status_func:
            subqueries_str = "\n- " + "\n- ".join(list(subqueries))
            async for event in send_status_func(f"**Searching the Internet for**: {subqueries_str}"):
                yield {ChatEvent.STATUS: event}

    with timer(f"Internet searches for {list(subqueries)} took", logger):
        search_func = search_with_google if SERPER_DEV_API_KEY else search_with_jina
        search_tasks = [search_func(subquery, location) for subquery in subqueries]
        search_results = await asyncio.gather(*search_tasks)
        response_dict = {subquery: search_result for subquery, search_result in search_results}

    # Gather distinct web pages from organic results for subqueries without an instant answer.
    # Content of web pages is directly available when Jina is used for search.
    webpages: Dict[str, Dict] = {}
    for subquery in response_dict:
        if "answerBox" in response_dict[subquery]:
            continue
        for organic in response_dict[subquery].get("organic", [])[:MAX_WEBPAGES_TO_READ]:
            link = organic.get("link")
            if link in webpages:
                webpages[link]["queries"].add(subquery)
            else:
                webpages[link] = {"queries": {subquery}, "content": organic.get("content")}

    # Read, extract relevant info from the retrieved web pages
    if webpages:
        logger.info(f"Reading web pages at: {webpages.keys()}")
        if send_status_func:
            webpage_links_str = "\n- " + "\n- ".join(webpages.keys())
            async for event in send_status_func(f"**Reading web pages**: {webpage_links_str}"):
                yield {ChatEvent.STATUS: event}
    tasks = [
        read_webpage_and_extract_content(data["queries"], link, data["content"], user=user, agent=agent)
        for link, data in webpages.items()
    ]
    results = await asyncio.gather(*tasks)

    # Collect extracted info from the retrieved web pages
    for subqueries, url, webpage_extract in results:
        if webpage_extract is not None:
            response_dict[subqueries.pop()]["webpages"] = {"link": url, "snippet": webpage_extract}

    yield response_dict


async def search_with_google(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    country_code = location.country_code.lower() if location and location.country_code else "us"
    payload = json.dumps({"q": query, "gl": country_code})
    headers = {"X-API-KEY": SERPER_DEV_API_KEY, "Content-Type": "application/json"}

    async with aiohttp.ClientSession() as session:
        async with session.post(SERPER_DEV_URL, headers=headers, data=payload) as response:
            if response.status != 200:
                logger.error(await response.text())
                return query, {}
            json_response = await response.json()
            extraction_fields = ["organic", "answerBox", "peopleAlsoAsk", "knowledgeGraph"]
            extracted_search_result = {
                field: json_response[field]
                for field in extraction_fields
                if not is_none_or_empty(json_response.get(field))
            }

            return query, extracted_search_result


async def read_webpages(
    query: str,
    conversation_history: dict,
    location: LocationData,
    user: KhojUser,
    send_status_func: Optional[Callable] = None,
    uploaded_image_url: str = None,
    agent: Agent = None,
):
    "Infer web pages to read from the query and extract relevant information from them"
    logger.info(f"Inferring web pages to read")
    if send_status_func:
        async for event in send_status_func(f"**Inferring web pages to read**"):
            yield {ChatEvent.STATUS: event}
    urls = await infer_webpage_urls(query, conversation_history, location, user, uploaded_image_url)

    logger.info(f"Reading web pages at: {urls}")
    if send_status_func:
        webpage_links_str = "\n- " + "\n- ".join(list(urls))
        async for event in send_status_func(f"**Reading web pages**: {webpage_links_str}"):
            yield {ChatEvent.STATUS: event}
    tasks = [read_webpage_and_extract_content({query}, url, user=user, agent=agent) for url in urls]
    results = await asyncio.gather(*tasks)

    response: Dict[str, Dict] = defaultdict(dict)
    response[query]["webpages"] = [
        {"query": qs.pop(), "link": url, "snippet": extract} for qs, url, extract in results if extract is not None
    ]
    yield response


async def read_webpage_and_extract_content(
    subqueries: set[str], url: str, content: str = None, user: KhojUser = None, agent: Agent = None
) -> Tuple[set[str], str, Union[None, str]]:
    extracted_info = None
    try:
        if is_none_or_empty(content):
            with timer(f"Reading web page at '{url}' took", logger):
                if FIRECRAWL_API_KEY:
                    if FIRECRAWL_TO_EXTRACT:
                        extracted_info = await read_webpage_and_extract_content_with_firecrawl(url, subqueries, agent)
                    else:
                        content = await read_webpage_with_firecrawl(url)
                elif OLOSTEP_API_KEY:
                    content = await read_webpage_with_olostep(url)
                else:
                    content = await read_webpage_with_jina(url)
        if is_none_or_empty(extracted_info):
            with timer(f"Extracting relevant information from web page at '{url}' took", logger):
                extracted_info = await extract_relevant_info(subqueries, content, user=user, agent=agent)
    except Exception as e:
        logger.error(f"Failed to read web page at '{url}' with {e}")

    return subqueries, url, extracted_info


async def read_webpage_at_url(web_url: str) -> str:
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


async def read_webpage_with_jina(web_url: str) -> str:
    jina_reader_api_url = f"{JINA_READER_API_URL}/{web_url}"
    headers = {"Accept": "application/json", "X-Timeout": "30"}
    if JINA_API_KEY:
        headers["Authorization"] = f"Bearer {JINA_API_KEY}"

    async with aiohttp.ClientSession() as session:
        async with session.get(jina_reader_api_url, headers=headers) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["data"]["content"]


async def read_webpage_with_firecrawl(web_url: str) -> str:
    firecrawl_api_url = f"{FIRECRAWL_API_URL}/v1/scrape"
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {FIRECRAWL_API_KEY}"}
    params = {"url": web_url, "formats": ["markdown"], "excludeTags": ["script", ".ad"]}

    async with aiohttp.ClientSession() as session:
        async with session.post(firecrawl_api_url, json=params, headers=headers) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["data"]["markdown"]


async def read_webpage_and_extract_content_with_firecrawl(web_url: str, queries: set[str], agent: Agent = None) -> str:
    firecrawl_api_url = f"{FIRECRAWL_API_URL}/v1/scrape"
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {FIRECRAWL_API_KEY}"}
    schema = {
        "type": "object",
        "properties": {
            "relevant_extract": {"type": "string"},
        },
        "required": [
            "relevant_extract",
        ],
    }

    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )
    system_prompt = f"""
{prompts.system_prompt_extract_relevant_information}

{personality_context}
User Query: {", ".join(queries)}

Collate only relevant information from the website to answer the target query and in the provided JSON schema.
""".strip()

    params = {"url": web_url, "formats": ["extract"], "extract": {"systemPrompt": system_prompt, "schema": schema}}

    async with aiohttp.ClientSession() as session:
        async with session.post(firecrawl_api_url, json=params, headers=headers) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["data"]["extract"]["relevant_extract"]


async def search_with_jina(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    encoded_query = urllib.parse.quote(query)
    jina_search_api_url = f"{JINA_SEARCH_API_URL}/{encoded_query}"
    headers = {"Accept": "application/json"}
    if JINA_API_KEY:
        headers["Authorization"] = f"Bearer {JINA_API_KEY}"

    async with aiohttp.ClientSession() as session:
        async with session.get(jina_search_api_url, headers=headers) as response:
            if response.status != 200:
                logger.error(await response.text())
                return query, {}
            response_json = await response.json()
            parsed_response = [
                {
                    "title": item["title"],
                    "content": item.get("content"),
                    # rename description -> snippet for consistency
                    "snippet": item["description"],
                    # rename url -> link for consistency
                    "link": item["url"],
                }
                for item in response_json["data"]
            ]
            return query, {"organic": parsed_response}
