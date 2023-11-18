import requests
import json
import os
import logging

logger = logging.getLogger(__name__)

SERPER_DEV_API_KEY = os.getenv("SERPER_DEV_API_KEY")

url = "https://google.serper.dev/search"


def search_with_google(query: str):
    if SERPER_DEV_API_KEY is None:
        raise ValueError("SERPER_DEV_API_KEY is not set")

    payload = json.dumps(
        {
            "q": query,
        }
    )

    headers = {"X-API-KEY": SERPER_DEV_API_KEY, "Content-Type": "application/json"}

    response = requests.request("POST", url, headers=headers, data=payload)

    if response.status_code != 200:
        logger.error(response.text)
        return {}

    response = response.json()

    response_dict = {}
    response_dict["knowledgeGraph"] = response.get("knowledgeGraph", {})
    response_dict["organic"] = response.get("organic", [])
    response_dict["answerBox"] = response.get("answerBox", [])
    response_dict["peopleAlsoAsk"] = response.get("peopleAlsoAsk", [])
    return response_dict
