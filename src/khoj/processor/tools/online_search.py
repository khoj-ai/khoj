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

    json_response = response.json()

    response_dict = {}
    response_dict["knowledgeGraph"] = json_response.get("knowledgeGraph", {})
    response_dict["organic"] = json_response.get("organic", [])
    response_dict["answerBox"] = json_response.get("answerBox", [])
    response_dict["peopleAlsoAsk"] = json_response.get("peopleAlsoAsk", [])
    return response_dict
