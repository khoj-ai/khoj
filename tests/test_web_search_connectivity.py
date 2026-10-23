import asyncio
import threading
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest
import requests
from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient

from khoj.processor.tools import online_search
from khoj.utils import helpers


@pytest.mark.parametrize("status_code, expected", [(200, True), (403, False), (503, False)])
def test_connectivity_probe_has_a_timeout(monkeypatch, status_code, expected):
    head = Mock(return_value=SimpleNamespace(status_code=status_code))
    monkeypatch.setattr(helpers.requests, "head", head)

    assert helpers.is_internet_connected() is expected
    head.assert_called_once_with("https://www.google.com", timeout=5)


@pytest.mark.parametrize("error", [requests.Timeout, requests.ConnectionError])
def test_connectivity_probe_handles_network_errors(monkeypatch, error):
    monkeypatch.setattr(helpers.requests, "head", Mock(side_effect=error))

    assert helpers.is_internet_connected() is False


@pytest.mark.anyio
async def test_pending_connectivity_probe_does_not_block_other_requests(monkeypatch):
    probe_started = threading.Event()
    release_probe = threading.Event()
    probe_timed_out = threading.Event()

    def check_connection():
        probe_started.set()
        if not release_probe.wait(timeout=5):
            probe_timed_out.set()
        return False

    monkeypatch.setattr(online_search, "is_internet_connected", check_connection)
    generate_subqueries = AsyncMock()
    monkeypatch.setattr(online_search, "generate_online_subqueries", generate_subqueries)
    app = FastAPI()

    @app.get("/search")
    async def search():
        return [result async for result in online_search.search_online("test query", [], None, None)]

    @app.get("/health")
    async def health():
        release_probe.set()
        return {"ok": True}

    async with AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
        search_request = asyncio.create_task(client.get("/search"))
        try:
            assert await asyncio.to_thread(probe_started.wait, 5)
            health_response = await asyncio.wait_for(client.get("/health"), timeout=5)
            search_response = await asyncio.wait_for(search_request, timeout=5)
        finally:
            release_probe.set()
            await asyncio.gather(search_request, return_exceptions=True)

    assert not probe_timed_out.is_set()
    assert health_response.status_code == 200
    assert health_response.json() == {"ok": True}
    assert search_response.status_code == 200
    assert search_response.json() == [{}]
    generate_subqueries.assert_not_awaited()


@pytest.mark.anyio
async def test_online_search_still_returns_results_when_connected(monkeypatch):
    monkeypatch.setattr(online_search, "is_internet_connected", Mock(return_value=True))
    monkeypatch.setattr(online_search, "generate_online_subqueries", AsyncMock(return_value={"test query"}))
    for key in ["SERPER_DEV_API_KEY", "EXA_API_KEY", "FIRECRAWL_API_KEY", "GOOGLE_SEARCH_API_KEY"]:
        monkeypatch.setattr(online_search, key, None)
    monkeypatch.setattr(online_search, "SEARXNG_URL", "http://search.test")
    monkeypatch.setattr(online_search, "AUTO_READ_WEBPAGE", False)
    search_result = {"organic": [{"title": "Test result", "link": "https://example.com"}]}
    search_provider = AsyncMock(return_value=("test query", search_result))
    monkeypatch.setattr(online_search, "search_with_searxng", search_provider)

    results = [result async for result in online_search.search_online("test query", [], None, None)]

    assert results == [{"test query": search_result}]
    search_provider.assert_awaited_once_with("test query", None)
