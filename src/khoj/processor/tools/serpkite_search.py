import logging
import os
from typing import TYPE_CHECKING, Dict, List, Optional, Tuple

import aiohttp

if TYPE_CHECKING:
    from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)
SERPKITE_SEARCH_URL = "https://api.serpkite.com/v1/search"
SERPKITE_REQUEST_TIMEOUT = 60


async def search_with_serpkite(query: str, location: Optional["LocationData"]) -> Tuple[str, Dict[str, List[Dict]]]:
    """Use SerpKite's v1 API and map its results to Khoj's search format."""
    api_key = os.getenv("SERPKITE_API_KEY")
    if not api_key:
        return query, {}
    payload = {"q": query[:2048]}
    if location and location.country_code:
        payload["country"] = location.country_code.lower()
    headers = {"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"}
    async with aiohttp.ClientSession() as session:
        async with session.post(
            SERPKITE_SEARCH_URL,
            headers=headers,
            json=payload,
            timeout=SERPKITE_REQUEST_TIMEOUT,
        ) as response:
            if response.status != 200:
                logger.warning("SerpKite search returned HTTP %s", response.status)
                return query, {}
            data = await response.json()
    organic = [
        {"title": result.get("title", ""), "link": result["link"], "snippet": result.get("snippet", "")}
        for result in data.get("results") or []
        if result.get("link")
    ]
    return query, {"organic": organic} if organic else {}
