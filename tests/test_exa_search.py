import json
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from khoj.processor.tools.online_search import read_webpage_with_exa, search_with_exa
from khoj.utils.rawconfig import LocationData


# ----------------------------------------------------------------------------------------------------
# Fixtures
# ----------------------------------------------------------------------------------------------------
EXA_SEARCH_RESPONSE = {
    "results": [
        {
            "title": "Example Article",
            "url": "https://example.com/article",
            "highlights": ["This is a highlighted sentence from the article."],
            "highlightScores": [0.95],
        },
        {
            "title": "Another Result",
            "url": "https://example.com/another",
            "highlights": ["Another relevant snippet."],
            "highlightScores": [0.88],
            "text": "Full text content of the page.",
        },
        {
            "title": "No Highlights Result",
            "url": "https://example.com/no-highlights",
        },
    ]
}

EXA_CONTENTS_RESPONSE = {
    "results": [
        {
            "title": "Example Page",
            "url": "https://example.com/page",
            "text": "Full text content of the web page for reading.",
        }
    ]
}

LOCATION = LocationData(city="San Francisco", region="California", country="United States", country_code="us")


# ----------------------------------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------------------------------
def _mock_aiohttp_response(status: int, json_data: dict):
    """Create a mock aiohttp response with the given status and JSON data."""
    response = AsyncMock()
    response.status = status
    response.json = AsyncMock(return_value=json_data)
    response.text = AsyncMock(return_value=json.dumps(json_data))
    response.raise_for_status = MagicMock()
    return response


def _mock_aiohttp_session(response):
    """Create a mock aiohttp.ClientSession that returns the given response on post."""
    session = AsyncMock()
    session.post = MagicMock(return_value=AsyncMock(__aenter__=AsyncMock(return_value=response)))
    ctx = AsyncMock()
    ctx.__aenter__ = AsyncMock(return_value=session)
    ctx.__aexit__ = AsyncMock(return_value=False)
    return ctx


# ----------------------------------------------------------------------------------------------------
# Tests: search_with_exa
# ----------------------------------------------------------------------------------------------------
class TestSearchWithExa:
    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_returns_organic_results(self, mock_session_cls):
        """Exa search should return results in the expected organic format."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        query, results = await search_with_exa("test query", LOCATION)

        assert query == "test query"
        assert "organic" in results
        assert len(results["organic"]) == 3

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_result_fields(self, mock_session_cls):
        """Each organic result should have title, link, snippet, and content fields."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        _, results = await search_with_exa("test query", LOCATION)
        first = results["organic"][0]

        assert first["title"] == "Example Article"
        assert first["link"] == "https://example.com/article"
        assert first["snippet"] == "This is a highlighted sentence from the article."
        assert first["content"] is None  # text was not requested

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_result_with_text_content(self, mock_session_cls):
        """Results that include text should populate the content field."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        _, results = await search_with_exa("test query", LOCATION)
        second = results["organic"][1]

        assert second["content"] == "Full text content of the page."

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_snippet_fallback_when_no_highlights(self, mock_session_cls):
        """Results without highlights should have None snippet."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        _, results = await search_with_exa("test query", LOCATION)
        third = results["organic"][2]

        assert third["snippet"] is None

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_empty_results(self, mock_session_cls):
        """Empty results from Exa should return empty dict."""
        response = _mock_aiohttp_response(200, {"results": []})
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        query, results = await search_with_exa("test query", LOCATION)

        assert query == "test query"
        assert results == {}

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_api_error_returns_empty(self, mock_session_cls):
        """Non-200 response should return empty dict."""
        response = _mock_aiohttp_response(500, {"error": "Internal server error"})
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        query, results = await search_with_exa("test query", LOCATION)

        assert query == "test query"
        assert results == {}

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_sends_integration_header(self, mock_session_cls):
        """Request should include the x-exa-integration tracking header."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session = AsyncMock()
        mock_post_ctx = AsyncMock()
        mock_post_ctx.__aenter__ = AsyncMock(return_value=response)
        mock_post_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session.post = MagicMock(return_value=mock_post_ctx)
        mock_session_ctx = AsyncMock()
        mock_session_ctx.__aenter__ = AsyncMock(return_value=mock_session)
        mock_session_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session_cls.return_value = mock_session_ctx

        await search_with_exa("test query", LOCATION)

        call_kwargs = mock_session.post.call_args
        headers = call_kwargs.kwargs.get("headers") or call_kwargs[1].get("headers")
        assert headers["x-exa-integration"] == "khoj"

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_uses_maxcharacters_not_deprecated_params(self, mock_session_cls):
        """Payload should use maxCharacters for highlights, not deprecated numSentences/highlightsPerUrl."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session = AsyncMock()
        mock_post_ctx = AsyncMock()
        mock_post_ctx.__aenter__ = AsyncMock(return_value=response)
        mock_post_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session.post = MagicMock(return_value=mock_post_ctx)
        mock_session_ctx = AsyncMock()
        mock_session_ctx.__aenter__ = AsyncMock(return_value=mock_session)
        mock_session_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session_cls.return_value = mock_session_ctx

        await search_with_exa("test query", LOCATION)

        call_kwargs = mock_session.post.call_args
        payload = call_kwargs.kwargs.get("json") or call_kwargs[1].get("json")
        highlights_config = payload["contents"]["highlights"]
        assert "maxCharacters" in highlights_config
        assert "numSentences" not in highlights_config
        assert "highlightsPerUrl" not in highlights_config

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.EXA_API_KEY", "test-api-key")
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_search_location_defaults_to_us(self, mock_session_cls):
        """When location has no country_code, should default to US."""
        response = _mock_aiohttp_response(200, EXA_SEARCH_RESPONSE)
        mock_session = AsyncMock()
        mock_post_ctx = AsyncMock()
        mock_post_ctx.__aenter__ = AsyncMock(return_value=response)
        mock_post_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session.post = MagicMock(return_value=mock_post_ctx)
        mock_session_ctx = AsyncMock()
        mock_session_ctx.__aenter__ = AsyncMock(return_value=mock_session)
        mock_session_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session_cls.return_value = mock_session_ctx

        empty_location = LocationData(city=None, region=None, country=None, country_code=None)
        await search_with_exa("test query", empty_location)

        call_kwargs = mock_session.post.call_args
        payload = call_kwargs.kwargs.get("json") or call_kwargs[1].get("json")
        assert payload["userLocation"] == "US"


# ----------------------------------------------------------------------------------------------------
# Tests: read_webpage_with_exa
# ----------------------------------------------------------------------------------------------------
class TestReadWebpageWithExa:
    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_read_webpage_returns_text(self, mock_session_cls):
        """Should return the text content of the page."""
        response = _mock_aiohttp_response(200, EXA_CONTENTS_RESPONSE)
        mock_session_cls.return_value = _mock_aiohttp_session(response)

        result = await read_webpage_with_exa("https://example.com/page", "test-key", "https://api.exa.ai")

        assert result == "Full text content of the web page for reading."

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_read_webpage_sends_integration_header(self, mock_session_cls):
        """Request should include the x-exa-integration tracking header."""
        response = _mock_aiohttp_response(200, EXA_CONTENTS_RESPONSE)
        mock_session = AsyncMock()
        mock_post_ctx = AsyncMock()
        mock_post_ctx.__aenter__ = AsyncMock(return_value=response)
        mock_post_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session.post = MagicMock(return_value=mock_post_ctx)
        mock_session_ctx = AsyncMock()
        mock_session_ctx.__aenter__ = AsyncMock(return_value=mock_session)
        mock_session_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session_cls.return_value = mock_session_ctx

        await read_webpage_with_exa("https://example.com/page", "test-key", "https://api.exa.ai")

        call_kwargs = mock_session.post.call_args
        headers = call_kwargs.kwargs.get("headers") or call_kwargs[1].get("headers")
        assert headers["x-exa-integration"] == "khoj"

    @pytest.mark.anyio
    @patch("khoj.processor.tools.online_search.aiohttp.ClientSession")
    async def test_read_webpage_does_not_send_deprecated_livecrawl(self, mock_session_cls):
        """Payload should not include the deprecated livecrawl field."""
        response = _mock_aiohttp_response(200, EXA_CONTENTS_RESPONSE)
        mock_session = AsyncMock()
        mock_post_ctx = AsyncMock()
        mock_post_ctx.__aenter__ = AsyncMock(return_value=response)
        mock_post_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session.post = MagicMock(return_value=mock_post_ctx)
        mock_session_ctx = AsyncMock()
        mock_session_ctx.__aenter__ = AsyncMock(return_value=mock_session)
        mock_session_ctx.__aexit__ = AsyncMock(return_value=False)
        mock_session_cls.return_value = mock_session_ctx

        await read_webpage_with_exa("https://example.com/page", "test-key", "https://api.exa.ai")

        call_kwargs = mock_session.post.call_args
        payload = call_kwargs.kwargs.get("json") or call_kwargs[1].get("json")
        assert "livecrawl" not in payload
        assert payload["text"] is True
