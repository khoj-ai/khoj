import asyncio
import json
import logging
import os
from collections import defaultdict
from typing import Any, Callable, Dict, List, Optional, Set, Tuple, Union

import aiohttp
from bs4 import BeautifulSoup
from markdownify import markdownify

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import (
    Agent,
    ChatMessageModel,
    KhojUser,
    WebScraper,
)
from khoj.routers.helpers import (
    ChatEvent,
    extract_relevant_info,
    generate_online_subqueries,
    infer_webpage_urls,
)
from khoj.utils.helpers import (
    is_env_var_true,
    is_internal_url,
    is_internet_connected,
    is_none_or_empty,
    timer,
)
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)

# Google Search API configurations
GOOGLE_SEARCH_API_KEY = os.getenv("GOOGLE_SEARCH_API_KEY")
GOOGLE_SEARCH_ENGINE_ID = os.getenv("GOOGLE_SEARCH_ENGINE_ID")
# Serper Dev API configurations
SERPER_DEV_API_KEY = os.getenv("SERPER_DEV_API_KEY")
SERPER_DEV_URL = "https://google.serper.dev/search"
# Firecrawl API configurations
FIRECRAWL_API_KEY = os.getenv("FIRECRAWL_API_KEY")
# SearXNG API configurations
SEARXNG_URL = os.getenv("KHOJ_SEARXNG_URL")
# Exa API credentials
EXA_API_KEY = os.getenv("EXA_API_KEY")

# Whether to automatically read web pages from search results
AUTO_READ_WEBPAGE = is_env_var_true("KHOJ_AUTO_READ_WEBPAGE")
# Timeout for web search and webpage read HTTP requests
WEBPAGE_REQUEST_TIMEOUT = 60  # seconds


async def search_online(
    query: str,
    conversation_history: List[ChatMessageModel],
    location: LocationData,
    user: KhojUser,
    send_status_func: Optional[Callable] = None,
    custom_filters: List[str] = [],
    max_online_searches: int = 3,
    max_webpages_to_read: int = 1,
    query_images: List[str] = None,
    query_files: str = None,
    previous_subqueries: Set = set(),
    agent: Agent = None,
    tracer: dict = {},
):
    query += " ".join(custom_filters)
    if not is_internet_connected():
        logger.warning("Cannot search online as not connected to internet")
        yield {}
        return

    # Breakdown the query into subqueries to get the correct answer
    new_subqueries = await generate_online_subqueries(
        query,
        conversation_history,
        location,
        user,
        query_images=query_images,
        query_files=query_files,
        max_queries=max_online_searches,
        agent=agent,
        tracer=tracer,
    )
    subqueries = list(new_subqueries - previous_subqueries)
    response_dict: Dict[str, Dict[str, List[Dict] | Dict]] = {}

    if is_none_or_empty(subqueries):
        logger.info("No new subqueries to search online")
        yield response_dict
        return

    search_engines = []
    if SERPER_DEV_API_KEY:
        search_engine = "Serper"
        search_engines.append((search_engine, search_with_serper))
    if EXA_API_KEY:
        search_engine = "Exa"
        search_engines.append((search_engine, search_with_exa))
    if FIRECRAWL_API_KEY:
        search_engine = "Firecrawl"
        search_engines.append((search_engine, search_with_firecrawl))
    if GOOGLE_SEARCH_API_KEY and GOOGLE_SEARCH_ENGINE_ID:
        search_engine = "Google"
        search_engines.append((search_engine, search_with_google))
    if SEARXNG_URL:
        search_engine = "Searxng"
        search_engines.append((search_engine, search_with_searxng))

    if send_status_func:
        subqueries_str = "\n- " + "\n- ".join(subqueries)
        async for event in send_status_func(f"**Searching the web for**: {subqueries_str}"):
            yield {ChatEvent.STATUS: event}

    response_dict = {}
    for search_engine, search_func in search_engines:
        logger.info(f"🌐 Searching the Internet with {search_engine} for {subqueries}")
        with timer(f"Internet searches with {search_engine} for {subqueries} took", logger):
            try:
                search_tasks = [search_func(subquery, location) for subquery in subqueries]
                search_results = await asyncio.gather(*search_tasks)
                response_dict = {subquery: search_result for subquery, search_result in search_results if search_result}
                if not is_none_or_empty(response_dict):
                    break
            except Exception as e:
                logger.error(f"Error searching with {search_engine}: {e}")
                response_dict = {}

    if not AUTO_READ_WEBPAGE:
        yield response_dict
        return

    # Gather distinct web pages from organic results for subqueries without an instant answer.
    webpages: Dict[str, Dict] = {}
    for subquery in response_dict:
        if "answerBox" in response_dict[subquery]:
            continue
        for idx, organic in enumerate(response_dict[subquery].get("organic") or []):
            link = organic.get("link")
            if link in webpages and idx < max_webpages_to_read:
                webpages[link]["queries"].add(subquery)
            # Content of web pages can be directly available when Exa is used for search.
            elif idx < max_webpages_to_read:
                webpages[link] = {"queries": {subquery}, "content": organic.get("content")}
            # Only keep webpage content for up to max_webpages_to_read organic results.
            if idx >= max_webpages_to_read and not is_none_or_empty(organic.get("content")):
                organic["content"] = None
                response_dict[subquery]["organic"][idx] = organic

    # Read, extract relevant info from the retrieved web pages
    if webpages:
        logger.info(f"Reading web pages at: {webpages.keys()}")
        if send_status_func:
            webpage_links_str = "\n- " + "\n- ".join(webpages.keys())
            async for event in send_status_func(f"**Browsing**: {webpage_links_str}"):
                yield {ChatEvent.STATUS: event}
    tasks = [
        extract_from_webpage(link, data["queries"], data.get("content"), user=user, agent=agent, tracer=tracer)
        for link, data in webpages.items()
    ]
    results = await asyncio.gather(*tasks)

    # Collect extracted info from the retrieved web pages
    for subqueries, url, webpage_extract in results:
        if webpage_extract is not None:
            response_dict[subqueries.pop()]["webpages"] = {"link": url, "snippet": webpage_extract}

    yield response_dict


async def search_with_exa(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    """
    Search using Exa API.

    Args:
        query: The search query string
        location: Location data for geolocation-based search

    Returns:
        Tuple containing the original query and a dictionary of search results
    """
    # Set up API endpoint and headers
    exa_api_base = os.getenv("EXA_API_URL", "https://api.exa.ai")
    exa_search_api_endpoint = f"{exa_api_base}/search"
    headers = {"Content-Type": "application/json", "x-api-key": EXA_API_KEY}

    # Prepare request payload
    country_code = location.country_code.upper() if location and location.country_code else "US"
    payload = {
        "query": query,
        "type": "auto",
        "userLocation": country_code,
        "numResults": 10,
        "contents": {
            "text": False,
            "highlights": {
                "numSentences": 5,
                "highlightsPerUrl": 1,
            },
        },
    }

    async with aiohttp.ClientSession() as session:
        try:
            async with session.post(
                exa_search_api_endpoint, headers=headers, json=payload, timeout=WEBPAGE_REQUEST_TIMEOUT
            ) as response:
                if response.status != 200:
                    error_text = await response.text()
                    logger.error(f"Exa search failed: {error_text}")
                    return query, {}

                response_json = await response.json()
                results = response_json.get("results", [])

                if is_none_or_empty(results):
                    logger.error(f"Exa search failed: {response_json.get('warning', 'Unknown error')}")
                    return query, {}

                # Transform Exa response to match the expected format
                organic_results = []
                for item in results:
                    organic_results.append(
                        {
                            "title": item["title"],
                            "link": item["url"],
                            "snippet": item["highlights"][0] if "highlights" in item else None,
                            "content": item.get("text", None),
                        }
                    )

                return query, {"organic": organic_results}

        except Exception as e:
            logger.error(f"Error searching with Exa: {str(e)}")
            return query, {}


async def search_with_firecrawl(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    """
    Search using Firecrawl API.

    Args:
        query: The search query string
        location: Location data for geolocation-based search

    Returns:
        Tuple containing the original query and a dictionary of search results
    """
    # Set up API endpoint and headers
    firecrawl_api_base = os.getenv("FIRECRAWL_API_URL", "https://api.firecrawl.dev")
    firecrawl_api_url = f"{firecrawl_api_base}/v2/search"
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {FIRECRAWL_API_KEY}"}

    # Prepare request payload
    country_code = location.country_code.lower() if location and location.country_code else "us"
    payload = {
        "query": query,
        "limit": 10,  # Maximum number of results
        "country": country_code,
        "lang": "en",
        "timeout": 10000,
        "scrapeOptions": {},
    }

    # Add location parameter if available
    if location and location.city:
        payload["location"] = f"{location.city}, {location.region}, {location.country}"

    async with aiohttp.ClientSession() as session:
        try:
            async with session.post(
                firecrawl_api_url, headers=headers, json=payload, timeout=WEBPAGE_REQUEST_TIMEOUT
            ) as response:
                if response.status != 200:
                    error_text = await response.text()
                    logger.error(f"Firecrawl search failed: {error_text}")
                    return query, {}

                response_json = await response.json()

                if not response_json.get("success", False):
                    logger.error(f"Firecrawl search failed: {response_json.get('warning', 'Unknown error')}")
                    return query, {}

                # Transform Firecrawl response to match the expected format
                organic_results = []
                for item in response_json.get("data", {}).get("web", []):
                    organic_results.append(
                        {
                            "title": item["title"],
                            "link": item["url"],
                            "snippet": item["description"],
                            "content": item.get("markdown", None),
                        }
                    )

                return query, {"organic": organic_results}

        except Exception as e:
            logger.error(f"Error searching with Firecrawl: {str(e)}")
            return query, {}


async def search_with_searxng(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    """Search using local SearXNG instance."""
    # Use environment variable
    search_url = f"{SEARXNG_URL}/search"
    country_code = location.country_code.lower() if location and location.country_code else "us"

    params = {"q": query, "format": "html", "language": "en", "country": country_code, "categories": "general"}

    async with aiohttp.ClientSession() as session:
        try:
            async with session.get(search_url, params=params, timeout=WEBPAGE_REQUEST_TIMEOUT) as response:
                if response.status != 200:
                    logger.error(f"SearXNG search failed to call {SEARXNG_URL}: {await response.text()}")
                    return query, {}

                html_content = await response.text()

                soup = BeautifulSoup(html_content, "html.parser")
                organic_results = []

                for result in soup.find_all("article", class_="result"):
                    title_elem = result.find("a", rel="noreferrer")
                    if title_elem:
                        title = title_elem.text.strip()
                        link = title_elem["href"]

                        description_elem = result.find("p", class_="content")
                        description = description_elem.text.strip() if description_elem else None

                        organic_results.append({"title": title, "link": link, "description": description})

                extracted_search_result = {"organic": organic_results}

                return query, extracted_search_result

        except Exception as e:
            logger.error(f"Error searching with SearXNG: {str(e)}")
            return query, {}


async def search_with_google(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    country_code = location.country_code.lower() if location and location.country_code else "us"
    base_url = "https://www.googleapis.com/customsearch/v1"
    params = {
        "key": GOOGLE_SEARCH_API_KEY,
        "cx": GOOGLE_SEARCH_ENGINE_ID,
        "q": query,
        "cr": f"country{country_code.upper()}",  # Country restrict parameter
        "gl": country_code,  # Geolocation parameter
    }

    async with aiohttp.ClientSession() as session:
        async with session.get(base_url, params=params, timeout=WEBPAGE_REQUEST_TIMEOUT) as response:
            if response.status != 200:
                logger.error(await response.text())
                return query, {}

            json_response = await response.json()

            # Transform Google's response format to match Serper's format
            organic_results = []
            if "items" in json_response:
                organic_results = [
                    {
                        "title": item.get("title", ""),
                        "link": item.get("link", ""),
                        "snippet": item.get("snippet", ""),
                        "content": None,  # Google Search API doesn't provide full content
                    }
                    for item in json_response["items"]
                ]

            # Format knowledge graph if available
            knowledge_graph = {}
            if "knowledge_graph" in json_response:
                kg = json_response["knowledge_graph"]
                knowledge_graph = {
                    "title": kg.get("name", ""),
                    "description": kg.get("description", ""),
                    "type": kg.get("type", ""),
                }

            extracted_search_result: Dict[str, Any] = {"organic": organic_results}

            if knowledge_graph:
                extracted_search_result["knowledgeGraph"] = knowledge_graph

            return query, extracted_search_result


async def search_with_serper(query: str, location: LocationData) -> Tuple[str, Dict[str, List[Dict]]]:
    headers = {"X-API-KEY": SERPER_DEV_API_KEY, "Content-Type": "application/json"}
    country_code = location.country_code.lower() if location and location.country_code else "us"
    max_query_length = 2048
    if len(query) > max_query_length:
        logger.warning(
            f"Truncate online query. Query length {len(query)} exceeds {max_query_length} supported by Serper. Query: {query}"
        )
        query = query[:max_query_length]

    payload = json.dumps({"q": query, "gl": country_code})

    async with aiohttp.ClientSession() as session:
        async with session.post(
            SERPER_DEV_URL, headers=headers, data=payload, timeout=WEBPAGE_REQUEST_TIMEOUT
        ) as response:
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
    conversation_history: List[ChatMessageModel],
    location: LocationData,
    user: KhojUser,
    send_status_func: Optional[Callable] = None,
    query_images: List[str] = None,
    agent: Agent = None,
    max_webpages_to_read: int = 1,
    query_files: str = None,
    tracer: dict = {},
):
    "Infer web pages to read from the query and extract relevant information from them"
    logger.info("Inferring web pages to read")
    urls = await infer_webpage_urls(
        query,
        max_webpages_to_read,
        conversation_history,
        location,
        user,
        query_images,
        agent=agent,
        query_files=query_files,
        tracer=tracer,
    )
    async for result in read_webpages_content(
        query,
        urls,
        user,
        send_status_func=send_status_func,
        agent=agent,
        tracer=tracer,
    ):
        yield result


async def read_webpages_content(
    query: str,
    urls: List[str],
    user: KhojUser,
    send_status_func: Optional[Callable] = None,
    agent: Agent = None,
    tracer: dict = {},
):
    logger.info(f"Reading web pages at: {urls}")
    if send_status_func:
        webpage_links_str = "\n- " + "\n- ".join(list(urls))
        async for event in send_status_func(f"**Browsing**: {webpage_links_str}"):
            yield {ChatEvent.STATUS: event}
    tasks = [extract_from_webpage(url, {query}, user=user, agent=agent, tracer=tracer) for url in urls]
    results = await asyncio.gather(*tasks)

    response: Dict[str, Dict] = defaultdict(dict)
    response[query]["webpages"] = [
        {"query": qs.pop(), "link": url, "snippet": extract} for qs, url, extract in results if extract is not None
    ]
    yield response


async def scrape_webpage(url, scraper_type=None, api_key=None, api_url=None) -> str | None:
    if scraper_type == WebScraper.WebScraperType.FIRECRAWL:
        return await read_webpage_with_firecrawl(url, api_key, api_url)
    elif scraper_type == WebScraper.WebScraperType.OLOSTEP:
        return await read_webpage_with_olostep(url, api_key, api_url)
    elif scraper_type == WebScraper.WebScraperType.EXA:
        return await read_webpage_with_exa(url, api_key, api_url)
    else:
        return await read_webpage_at_url(url)


async def scrape_webpage_with_fallback(url: str) -> Optional[str]:
    """
    Scrape a webpage using enabled web scrapers with fallback logic.
    Tries all enabled scrapers in order until one succeeds.
    Returns the content if successful, None otherwise.
    """
    # Select the web scrapers to use for reading the web page
    web_scrapers = await ConversationAdapters.aget_enabled_webscrapers()
    # Only use the direct web scraper for internal URLs
    if is_internal_url(url):
        web_scrapers = [scraper for scraper in web_scrapers if scraper.type == WebScraper.WebScraperType.DIRECT]

    # Read the web page
    # fallback through enabled web scrapers until success
    content = None
    for scraper in web_scrapers:
        try:
            with timer(f"Reading web page with {scraper.type} at '{url}' took", logger, log_level=logging.INFO):
                content = await scrape_webpage(url, scraper.type, scraper.api_key, scraper.api_url)
            if not is_none_or_empty(content):
                break
        except Exception as e:
            logger.warning(f"Failed to read web page with {scraper.type} at '{url}' with {e}")
            # If this is the last web scraper in the list, log an error
            if scraper.name == web_scrapers[-1].name:
                logger.error(f"All web scrapers failed for '{url}'")

    return content


async def extract_from_webpage(
    url: str,
    subqueries: set[str] = None,
    content: str = None,
    user: KhojUser = None,
    agent: Agent = None,
    tracer: dict = {},
) -> Tuple[set[str], str, Union[None, str]]:
    # Read the web page
    content = None
    if is_none_or_empty(content):
        content = await scrape_webpage_with_fallback(url)

    # Extract relevant information from the web page
    extracted_info = None
    if not is_none_or_empty(content):
        with timer(f"Extracting relevant information from web page at '{url}' took", logger):
            extracted_info = await extract_relevant_info(subqueries, content, user=user, agent=agent, tracer=tracer)

    return subqueries, url, extracted_info


async def read_webpage_at_url(web_url: str) -> str:
    headers = {
        "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36",
    }

    async with aiohttp.ClientSession() as session:
        async with session.get(web_url, headers=headers, timeout=WEBPAGE_REQUEST_TIMEOUT) as response:
            response.raise_for_status()
            html = await response.text()
            parsed_html = BeautifulSoup(html, "html.parser")
            body = parsed_html.body.get_text(separator="\n", strip=True)
            return markdownify(body)


async def read_webpage_with_olostep(web_url: str, api_key: str, api_url: str) -> str:
    headers = {"Authorization": f"Bearer {api_key}"}
    web_scraping_params: Dict[str, Union[str, int, bool]] = {
        "timeout": 35,  # seconds
        "waitBeforeScraping": 0,  # seconds
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
    web_scraping_params["url"] = web_url

    async with aiohttp.ClientSession() as session:
        async with session.get(
            api_url, params=web_scraping_params, headers=headers, timeout=WEBPAGE_REQUEST_TIMEOUT
        ) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["markdown_content"]


async def read_webpage_with_exa(web_url: str, api_key: str, api_url: str) -> str:
    exa_api_url = f"{api_url}/contents"
    headers = {"Content-Type": "application/json", "x-api-key": api_key}
    params = {
        "urls": [web_url],
        "text": True,
        "livecrawl": "fallback",
        "livecrawlTimeout": 15000,
    }

    async with aiohttp.ClientSession() as session:
        async with session.post(exa_api_url, json=params, headers=headers, timeout=WEBPAGE_REQUEST_TIMEOUT) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["results"][0]["text"]


async def read_webpage_with_firecrawl(web_url: str, api_key: str, api_url: str) -> str:
    firecrawl_api_url = f"{api_url}/v2/scrape"
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {api_key}"}
    params = {
        "url": web_url,
        "formats": ["markdown"],
        "excludeTags": ["script", ".ad"],
        "removeBase64Images": True,
        "maxAge": 86400000,  # accept upto 1 day old cached content for speed
    }

    async with aiohttp.ClientSession() as session:
        async with session.post(
            firecrawl_api_url, json=params, headers=headers, timeout=WEBPAGE_REQUEST_TIMEOUT
        ) as response:
            response.raise_for_status()
            response_json = await response.json()
            return response_json["data"]["markdown"]


def deduplicate_organic_results(online_results: dict) -> dict:
    """Deduplicate organic search results based on links across all queries."""
    # Keep track of seen links to filter out duplicates across queries
    seen_links = set()
    deduplicated_results = {}

    # Process each query's results
    for query, results in online_results.items():
        # Filter organic results keeping only first occurrence of each link
        filtered_organic = []
        for result in results.get("organic") or []:
            link = result.get("link")
            if link and link not in seen_links:
                seen_links.add(link)
                filtered_organic.append(result)

        # Update results with deduplicated organic entries
        deduplicated_results[query] = {**results, "organic": filtered_organic}

    return deduplicated_results
