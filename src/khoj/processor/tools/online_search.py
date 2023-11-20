import requests
import json
import os
import logging

from khoj.routers.helpers import generate_online_subqueries

logger = logging.getLogger(__name__)

SERPER_DEV_API_KEY = os.getenv("SERPER_DEV_API_KEY")

url = "https://google.serper.dev/search"


async def search_with_google(query: str):
    def _search_with_google(subquery: str):
        payload = json.dumps(
            {
                "q": subquery,
            }
        )

        headers = {"X-API-KEY": SERPER_DEV_API_KEY, "Content-Type": "application/json"}

        response = requests.request("POST", url, headers=headers, data=payload)

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
    subqueries = await generate_online_subqueries(query)

    response_dict = {}

    for subquery in subqueries:
        logger.info(f"Searching with Google for '{subquery}'")
        response_dict[subquery] = _search_with_google(subquery)

    return response_dict
