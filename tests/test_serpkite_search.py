import os
import unittest
from types import SimpleNamespace
from unittest.mock import AsyncMock, MagicMock, patch

import aiohttp

from khoj.processor.tools.serpkite_search import search_with_serpkite


class SerpKiteSearchTests(unittest.IsolatedAsyncioTestCase):
    def session(self, body=None, status=200):
        response = MagicMock(status=status)
        response.json = AsyncMock(return_value=body or {})
        response_context = MagicMock()
        response_context.__aenter__ = AsyncMock(return_value=response)
        response_context.__aexit__ = AsyncMock(return_value=False)
        session = MagicMock()
        session.post.return_value = response_context
        session.__aenter__ = AsyncMock(return_value=session)
        session.__aexit__ = AsyncMock(return_value=False)
        return session, response

    async def test_auth_payload_and_result_mapping(self):
        session, _ = self.session(
            {"results": [{"title": "Example", "link": "https://example.com", "snippet": "A result"}]}
        )
        with (
            patch.dict(os.environ, {"SERPKITE_API_KEY": "test-key"}),
            patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession", return_value=session),
        ):
            query, result = await search_with_serpkite("test query", SimpleNamespace(country_code="GB"))
        self.assertEqual(query, "test query")
        self.assertEqual(
            result, {"organic": [{"title": "Example", "link": "https://example.com", "snippet": "A result"}]}
        )
        args, kwargs = session.post.call_args
        self.assertEqual(args[0], "https://api.serpkite.com/v1/search")
        self.assertEqual(kwargs["headers"]["Authorization"], "Bearer test-key")
        self.assertEqual(kwargs["json"], {"q": "test query", "country": "gb"})
        self.assertEqual(kwargs["timeout"], 60)

    async def test_missing_key_does_not_send_request(self):
        with (
            patch.dict(os.environ, {}, clear=True),
            patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession") as session,
        ):
            self.assertEqual(await search_with_serpkite("test", None), ("test", {}))
            session.assert_not_called()

    async def test_optional_fields_and_link_filter(self):
        session, _ = self.session({"results": [{"title": "No link"}, {"link": "https://example.com"}]})
        with (
            patch.dict(os.environ, {"SERPKITE_API_KEY": "test-key"}),
            patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession", return_value=session),
        ):
            _, result = await search_with_serpkite("test", None)
        self.assertEqual(result, {"organic": [{"title": "", "link": "https://example.com", "snippet": ""}]})
        self.assertEqual(session.post.call_args.kwargs["json"], {"q": "test"})

    async def test_empty_results_allow_provider_fallback(self):
        for body in ({"results": []}, {"results": None}):
            session, _ = self.session(body)
            with (
                patch.dict(os.environ, {"SERPKITE_API_KEY": "test-key"}),
                patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession", return_value=session),
            ):
                self.assertEqual(await search_with_serpkite("test", None), ("test", {}))

    async def test_http_error_logs_only_status(self):
        session, response = self.session({"error": {"message": "private data"}}, status=401)
        with (
            patch.dict(os.environ, {"SERPKITE_API_KEY": "test-key"}),
            patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession", return_value=session),
            self.assertLogs("khoj.processor.tools.serpkite_search", level="WARNING") as logs,
        ):
            self.assertEqual(await search_with_serpkite("private query", None), ("private query", {}))
        response.json.assert_not_called()
        self.assertEqual(
            logs.output, ["WARNING:khoj.processor.tools.serpkite_search:SerpKite search returned HTTP 401"]
        )

    async def test_query_limit_preserves_original_result_key(self):
        query = "a" * 2050
        session, _ = self.session({"results": []})
        with (
            patch.dict(os.environ, {"SERPKITE_API_KEY": "test-key"}),
            patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession", return_value=session),
        ):
            returned_query, _ = await search_with_serpkite(query, SimpleNamespace(country_code=None))
        self.assertEqual(returned_query, query)
        self.assertEqual(session.post.call_args.kwargs["json"], {"q": "a" * 2048})

    async def test_network_error_reaches_existing_fallback_handler(self):
        session, _ = self.session()
        session.post.side_effect = aiohttp.ClientConnectionError("connection failed")
        with (
            patch.dict(os.environ, {"SERPKITE_API_KEY": "test-key"}),
            patch("khoj.processor.tools.serpkite_search.aiohttp.ClientSession", return_value=session),
        ):
            with self.assertRaises(aiohttp.ClientConnectionError):
                await search_with_serpkite("test", None)
