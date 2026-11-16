import os
import secrets

import numpy as np
import psutil
import pytest
from scipy.stats import linregress

from khoj.processor.embeddings import EmbeddingsModel
from khoj.processor.tools.online_search import (
    read_webpage_at_url,
    read_webpage_with_olostep,
)
from khoj.utils import helpers


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


def test_cost_of_anthropic_chat_uses_provider_cache_prices():
    # Expected prices in USD per 1M tokens from https://platform.claude.com/docs/en/about-claude/pricing
    # Claude Opus 4.5: cache hits $0.50
    assert helpers.get_cost_of_chat_message("claude-opus-4-5", cache_read_tokens=1_000_000) == pytest.approx(0.50)
    assert helpers.get_cost_of_chat_message("claude-opus-4-5-20251101", cache_read_tokens=1_000_000) == pytest.approx(
        0.50
    )

    # Claude Haiku 4.5: cache hits $0.10, 5m cache writes $1.25
    cost = helpers.get_cost_of_chat_message(
        "claude-haiku-4-5-20251001", cache_read_tokens=1_000_000, cache_write_tokens=1_000_000
    )
    assert cost == pytest.approx(0.10 + 1.25)

    # Claude Sonnet 4.5: $3 input, $15 output, $0.30 cache hits, $3.75 5m cache writes
    cost = helpers.get_cost_of_chat_message(
        "claude-sonnet-4-5",
        input_tokens=2_048,
        output_tokens=1_024,
        cache_read_tokens=45_000,
        cache_write_tokens=3_500,
    )
    assert cost == pytest.approx((2_048 * 3 + 1_024 * 15 + 45_000 * 0.30 + 3_500 * 3.75) / 1e6)


def test_cost_of_gemini_chat_includes_cache_reads_and_thoughts():
    # Expected prices in USD per 1M tokens from https://ai.google.dev/gemini-api/docs/pricing
    # Gemini 2.5 Pro (prompts <= 200k tokens): $1.25 input, $10 output including thinking tokens, $0.125 context caching
    cost = helpers.get_cost_of_chat_message(
        "gemini-2.5-pro", input_tokens=10_000, output_tokens=1_000, thought_tokens=2_000, cache_read_tokens=100_000
    )
    assert cost == pytest.approx((10_000 * 1.25 + 3_000 * 10 + 100_000 * 0.125) / 1e6)

    # Gemini 2.5 Flash: $0.03 context caching
    assert helpers.get_cost_of_chat_message("gemini-2.5-flash", cache_read_tokens=1_000_000) == pytest.approx(0.03)


def test_cost_of_chat_falls_back_to_khoj_prices():
    # Models not in the llm-prices table use the prices in constants.model_to_cost
    assert helpers.get_cost_of_chat_message("grok-4", input_tokens=1_000_000, cache_read_tokens=1_000_000) == (
        pytest.approx(3.0 + 0.75)
    )
    # Gemini cache reads are priced in the fallback table too
    assert helpers.get_cost_of_chat_message("gemini-3-pro-preview", cache_read_tokens=1_000_000) == pytest.approx(0.20)
    # Open-weight models served by a third party host keep their configured prices
    assert helpers.get_cost_of_chat_message("MiniMax-M3", input_tokens=1_000_000, output_tokens=1_000_000) == (
        pytest.approx(0.6 + 2.4)
    )
    # Unknown and local models cost nothing
    assert helpers.get_cost_of_chat_message("llama3.1:8b", input_tokens=1_000_000, output_tokens=1_000_000) == 0
    assert helpers.get_cost_of_chat_message("", input_tokens=1_000_000) == 0


def test_cost_of_chat_adds_previous_cost():
    usage = helpers.get_chat_usage_metrics("claude-opus-4-5", input_tokens=1_000_000)
    usage = helpers.get_chat_usage_metrics("claude-opus-4-5", output_tokens=1_000_000, usage=usage)
    assert usage["input_tokens"] == 1_000_000
    assert usage["output_tokens"] == 1_000_000
    assert usage["cost"] == pytest.approx(5.0 + 25.0)


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
