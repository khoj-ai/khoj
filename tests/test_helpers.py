import os
import secrets
from unittest.mock import AsyncMock, MagicMock, patch

import numpy as np
import psutil
import pytest
from scipy.stats import linregress

from khoj.processor.embeddings import EmbeddingsModel
from khoj.processor.tools.online_search import (
    read_webpage_at_url,
    read_webpage_with_crw,
    read_webpage_with_olostep,
    search_with_crw,
)
from khoj.utils import helpers


def _mock_aiohttp_post(response_json, status=200):
    """Build a patch for aiohttp.ClientSession.post returning the given JSON payload."""
    response = MagicMock()
    response.status = status
    response.raise_for_status = MagicMock()
    response.json = AsyncMock(return_value=response_json)
    response.text = AsyncMock(return_value="")

    post_context = MagicMock()
    post_context.__aenter__ = AsyncMock(return_value=response)
    post_context.__aexit__ = AsyncMock(return_value=False)

    return patch("aiohttp.ClientSession.post", return_value=post_context)


def test_get_from_null_dict():
    # null handling
    assert helpers.get_from_dict(dict()) == dict()
    assert helpers.get_from_dict(dict(), None) == None

    # key present in nested dictionary
    # 1-level dictionary
    assert helpers.get_from_dict({"a": 1, "b": 2}, "a") == 1
    assert helpers.get_from_dict({"a": 1, "b": 2}, "c") == None

    # 2-level dictionary
    assert helpers.get_from_dict({"a": {"a_a": 1}, "b": 2}, "a") == {"a_a": 1}
    assert helpers.get_from_dict({"a": {"a_a": 1}, "b": 2}, "a", "a_a") == 1

    # key not present in nested dictionary
    # 2-level_dictionary
    assert helpers.get_from_dict({"a": {"a_a": 1}, "b": 2}, "b", "b_a") == None


def test_merge_dicts():
    # basic merge of dicts with non-overlapping keys
    assert helpers.merge_dicts(priority_dict={"a": 1}, default_dict={"b": 2}) == {"a": 1, "b": 2}

    # use default dict items when not present in priority dict
    assert helpers.merge_dicts(priority_dict={}, default_dict={"b": 2}) == {"b": 2}

    # do not override existing key in priority_dict with default dict
    assert helpers.merge_dicts(priority_dict={"a": 1}, default_dict={"a": 2}) == {"a": 1}


def test_lru_cache():
    # Test initializing cache
    cache = helpers.LRU({"a": 1, "b": 2}, capacity=2)
    assert cache == {"a": 1, "b": 2}

    # Test capacity overflow
    cache["c"] = 3
    assert cache == {"b": 2, "c": 3}

    # Test delete least recently used item from LRU cache on capacity overflow
    cache["b"]  # accessing 'b' makes it the most recently used item
    cache["d"] = 4  # so 'c' is deleted from the cache instead of 'b'
    assert cache == {"b": 2, "d": 4}


@pytest.mark.skip(reason="Memory leak exists on GPU, MPS devices")
def test_encode_docs_memory_leak():
    # Arrange
    iterations = 50
    batch_size = 20
    embeddings_model = EmbeddingsModel()
    memory_usage_trend = []
    device = f"{helpers.get_device()}".upper()

    # Act
    # Encode random strings repeatedly and record memory usage trend
    for iteration in range(iterations):
        random_docs = [" ".join(secrets.token_hex(5) for _ in range(10)) for _ in range(batch_size)]
        a = [embeddings_model.embed_documents(random_docs)]
        memory_usage_trend += [psutil.Process().memory_info().rss / (1024 * 1024)]
        print(f"{iteration:02d}, {memory_usage_trend[-1]:.2f}", flush=True)

    # Calculate slope of line fitting memory usage history
    memory_usage_trend = np.array(memory_usage_trend)
    slope, _, _, _, _ = linregress(np.arange(len(memory_usage_trend)), memory_usage_trend)
    print(f"Memory usage increased at ~{slope:.2f} MB per iteration on {device}")

    # Assert
    # If slope is positive memory utilization is increasing
    # Positive threshold of 2, from observing memory usage trend on MPS vs CPU device
    assert slope < 2, f"Memory leak suspected on {device}. Memory usage increased at ~{slope:.2f} MB per iteration"


@pytest.mark.asyncio
async def test_reading_webpage():
    # Arrange
    website = "https://en.wikipedia.org/wiki/Great_Chicago_Fire"

    # Act
    response = await read_webpage_at_url(website)

    # Assert
    assert (
        "An alarm sent from the area near the fire also failed to register at the courthouse where the fire watchmen were"
        in response
    )


@pytest.mark.skipif(os.getenv("OLOSTEP_API_KEY") is None, reason="OLOSTEP_API_KEY is not set")
@pytest.mark.asyncio
async def test_reading_webpage_with_olostep():
    # Arrange
    website = "https://en.wikipedia.org/wiki/Great_Chicago_Fire"

    # Act
    response = await read_webpage_with_olostep(website)

    # Assert
    assert (
        "An alarm sent from the area near the fire also failed to register at the courthouse where the fire watchmen were"
        in response
    )


@pytest.mark.asyncio
async def test_reading_webpage_with_crw():
    # Arrange
    website = "https://en.wikipedia.org/wiki/Great_Chicago_Fire"
    crw_response = {"success": True, "data": {"markdown": "# Great Chicago Fire\n\nThe fire watchmen were alerted."}}

    # Act
    with _mock_aiohttp_post(crw_response) as mock_post:
        response = await read_webpage_with_crw(website, "test-key", "https://fastcrw.com/api")

    # Assert
    assert response == "# Great Chicago Fire\n\nThe fire watchmen were alerted."
    mock_post.assert_called_once()
    assert mock_post.call_args.args[0] == "https://fastcrw.com/api/v1/scrape"
    assert mock_post.call_args.kwargs["headers"]["Authorization"] == "Bearer test-key"
    assert mock_post.call_args.kwargs["json"]["url"] == website


@pytest.mark.asyncio
async def test_search_with_crw():
    # Arrange
    query = "great chicago fire"
    crw_response = {
        "success": True,
        "data": [
            {
                "title": "Great Chicago Fire",
                "url": "https://en.wikipedia.org/wiki/Great_Chicago_Fire",
                "description": "The Great Chicago Fire was a conflagration that burned in 1871.",
                "markdown": "# Great Chicago Fire",
            }
        ],
    }

    # Act
    with patch.dict(os.environ, {"CRW_API_KEY": "test-key"}), patch(
        "khoj.processor.tools.online_search.CRW_API_KEY", "test-key"
    ):
        with _mock_aiohttp_post(crw_response) as mock_post:
            returned_query, results = await search_with_crw(query, None)

    # Assert
    assert returned_query == query
    assert results["organic"][0]["title"] == "Great Chicago Fire"
    assert results["organic"][0]["link"] == "https://en.wikipedia.org/wiki/Great_Chicago_Fire"
    assert results["organic"][0]["content"] == "# Great Chicago Fire"
    assert mock_post.call_args.args[0] == "https://fastcrw.com/api/v1/search"
    assert mock_post.call_args.kwargs["json"]["query"] == query
