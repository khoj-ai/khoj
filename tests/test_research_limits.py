import json

import pytest

from khoj.processor.conversation.utils import ResearchIteration, ResponseWithThought, ToolCall
from khoj.routers import research as research_module
from khoj.routers.research import research


def planner_response(tool_calls: list[dict]):
    async def fake_send_message_to_model_wrapper(*args, **kwargs):
        return ResponseWithThought(text=json.dumps(tool_calls), thought="")

    return fake_send_message_to_model_wrapper


async def run_research(user, query="Compare these items", previous_iterations=None):
    return [
        result
        async for result in research(
            user=user,
            query=query,
            conversation_id=None,
            conversation_history=[],
            previous_iterations=previous_iterations or [],
            query_images=[],
        )
    ]


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "iterations_done, iterations_left",
    [(0, 5), (3, 2)],
)
async def test_parallel_tool_calls_do_not_exceed_max_research_iterations(
    default_user, monkeypatch, iterations_done, iterations_left
):
    # Arrange: the model asks for 12 web searches in a single response
    monkeypatch.delenv("KHOJ_RESEARCH_ITERATIONS", raising=False)
    tool_calls = [{"name": "search_web", "args": {"query": f"item {i}"}, "id": f"call_{i}"} for i in range(12)]
    monkeypatch.setattr(research_module, "send_message_to_model_wrapper", planner_response(tool_calls))
    previous_iterations = [
        ResearchIteration(
            query=ToolCall(name="search_web", args={"query": f"earlier {i}"}, id=f"earlier_{i}"),
            summarizedResult=f"<iteration_{i + 1}_results>done</iteration_{i + 1}_results>",
        )
        for i in range(iterations_done)
    ]

    executed_searches = []

    async def fake_search_online(query, *args, **kwargs):
        executed_searches.append(query)
        yield {query: {"organic": [{"title": query, "link": "https://example.com", "snippet": query}]}}

    monkeypatch.setattr(research_module, "search_online", fake_search_online)

    # Act
    await run_research(default_user, previous_iterations=previous_iterations)

    # Assert: no more tool calls run than the research iterations left of the default limit of 5
    assert len(executed_searches) == iterations_left


@pytest.mark.asyncio
async def test_read_webpage_reads_at_most_max_webpages_per_call(default_user, monkeypatch):
    # Arrange: the model asks to read 8 webpages in a single read_webpage call
    urls = [f"https://example.com/page{i}" for i in range(8)]
    tool_calls = [{"name": "read_webpage", "args": {"urls": urls, "query": "pricing"}, "id": "call_0"}]
    responses = iter([tool_calls, []])

    async def fake_send_message_to_model_wrapper(*args, **kwargs):
        calls = next(responses, [])
        return ResponseWithThought(text=json.dumps(calls) if calls else "done", thought="")

    monkeypatch.setattr(research_module, "send_message_to_model_wrapper", fake_send_message_to_model_wrapper)

    read_urls = []

    async def fake_read_webpages_content(query, urls, *args, **kwargs):
        read_urls.extend(urls)
        yield {query: {"webpages": [{"query": query, "link": url, "snippet": "content"} for url in urls]}}

    monkeypatch.setattr(research_module, "read_webpages_content", fake_read_webpages_content)

    # Act
    await run_research(default_user)

    # Assert: only the advertised max of 1 webpage is read per read_webpage call
    assert read_urls == urls[:1]
