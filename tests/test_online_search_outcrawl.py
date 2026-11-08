import pytest
import pytest_asyncio
from aiohttp import web

from khoj.database.models import WebScraper
from khoj.processor.tools import online_search
from khoj.processor.tools.online_search import (
    read_webpage_with_outcrawl,
    scrape_webpage,
    search_with_outcrawl,
)
from khoj.utils.helpers import is_web_search_enabled


@pytest_asyncio.fixture
async def outcrawl_server():
    """Fake Outcrawl API that records requests and replies with canned payloads."""
    seen = []

    async def search(request):
        seen.append(("search", request.headers.get("Authorization"), await request.json()))
        query = seen[-1][2]["query"]
        if query == "no results":
            return web.json_response({"hits": []})
        if query == "server error":
            return web.json_response({"code": "internal"}, status=500)
        return web.json_response(
            {
                "hits": [
                    {"url": "https://example.com/a", "title": "A", "snippet": "first hit", "rank": 1},
                    {"url": "https://example.com/b", "title": "B", "snippet": "second hit", "rank": 2},
                ],
                "usage": {"credits": "1"},
            }
        )

    async def scrape(request):
        seen.append(("scrape", request.headers.get("Authorization"), await request.json()))
        if seen[-1][2]["url"].endswith("/missing"):
            return web.json_response({"code": "not_found"}, status=404)
        return web.json_response({"url": seen[-1][2]["url"], "statusCode": 200, "markdown": "# Page\n\nBody text"})

    app = web.Application()
    app.router.add_post("/v1/search", search)
    app.router.add_post("/v1/scrape", scrape)
    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, "127.0.0.1", 0)
    await site.start()
    port = site._server.sockets[0].getsockname()[1]
    try:
        yield f"http://127.0.0.1:{port}", seen
    finally:
        await runner.cleanup()


@pytest.mark.asyncio
async def test_search_with_outcrawl_maps_hits_to_organic_results(monkeypatch, outcrawl_server):
    # Arrange
    base_url, seen = outcrawl_server
    monkeypatch.setattr(online_search, "OUTCRAWL_API_KEY", "oc_test_key")
    monkeypatch.setenv("OUTCRAWL_API_URL", base_url)

    # Act
    query, results = await search_with_outcrawl("khoj", None)

    # Assert
    assert query == "khoj"
    assert results == {
        "organic": [
            {"title": "A", "link": "https://example.com/a", "snippet": "first hit"},
            {"title": "B", "link": "https://example.com/b", "snippet": "second hit"},
        ]
    }
    assert seen == [("search", "Bearer oc_test_key", {"query": "khoj", "limit": 10})]


@pytest.mark.asyncio
@pytest.mark.parametrize("query", ["no results", "server error"])
async def test_search_with_outcrawl_returns_empty_on_failure(monkeypatch, outcrawl_server, query):
    # Arrange
    base_url, _ = outcrawl_server
    monkeypatch.setattr(online_search, "OUTCRAWL_API_KEY", "oc_test_key")
    monkeypatch.setenv("OUTCRAWL_API_URL", base_url)

    # Act
    returned_query, results = await search_with_outcrawl(query, None)

    # Assert
    assert returned_query == query
    assert results == {}


@pytest.mark.asyncio
async def test_read_webpage_with_outcrawl_returns_markdown(outcrawl_server):
    # Arrange
    base_url, seen = outcrawl_server

    # Act
    content = await read_webpage_with_outcrawl("https://example.com/page", "oc_test_key", base_url)

    # Assert
    assert content == "# Page\n\nBody text"
    assert seen == [("scrape", "Bearer oc_test_key", {"url": "https://example.com/page", "formats": ["markdown"]})]


@pytest.mark.asyncio
async def test_read_webpage_with_outcrawl_raises_on_http_error(outcrawl_server):
    # Arrange
    base_url, _ = outcrawl_server

    # Act / Assert
    with pytest.raises(Exception):
        await read_webpage_with_outcrawl("https://example.com/missing", "oc_test_key", base_url)


@pytest.mark.asyncio
async def test_scrape_webpage_routes_outcrawl_scraper_type(outcrawl_server):
    # Arrange
    base_url, seen = outcrawl_server

    # Act
    content = await scrape_webpage(
        "https://example.com/page", WebScraper.WebScraperType.OUTCRAWL, "oc_test_key", base_url
    )

    # Assert
    assert content == "# Page\n\nBody text"
    assert [call[0] for call in seen] == ["scrape"]


def test_outcrawl_api_key_enables_web_search(monkeypatch):
    # Arrange
    for name in ["GOOGLE_SEARCH_API_KEY", "SERPER_DEV_API_KEY", "EXA_API_KEY", "FIRECRAWL_API_KEY", "KHOJ_SEARXNG_URL"]:
        monkeypatch.delenv(name, raising=False)
    monkeypatch.delenv("OUTCRAWL_API_KEY", raising=False)
    assert not is_web_search_enabled()

    # Act
    monkeypatch.setenv("OUTCRAWL_API_KEY", "oc_test_key")

    # Assert
    assert is_web_search_enabled()
