import pytest

from khoj.processor.tools import online_search


class FakeResponse:
    def __init__(self, html: str):
        self.html = html

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc_info):
        return False

    def raise_for_status(self):
        pass

    async def text(self):
        return self.html


class FakeSession:
    def __init__(self, html: str):
        self.html = html

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc_info):
        return False

    def get(self, url, **kwargs):
        return FakeResponse(self.html)


@pytest.mark.asyncio
async def test_read_webpage_at_url_keeps_sentences_and_identifiers(monkeypatch):
    "Ensure the direct webpage reader neither splits sentences at inline tags nor escapes the text"
    # Arrange
    html = (
        "<html><head><title>Guide</title></head><body>"
        "<h1>Setup</h1>"
        '<p>Read the <a href="/docs">setup guide</a> before you start.</p>'
        "<p>Set <code>MAX_TOKENS</code> in your <em>config_file</em>.</p>"
        "<script>var hidden = 1;</script>"
        "</body></html>"
    )
    monkeypatch.setattr(online_search.aiohttp, "ClientSession", lambda *args, **kwargs: FakeSession(html))

    # Act
    text = await online_search.read_webpage_at_url("https://example.com/guide")

    # Assert
    assert text == "Setup\nRead the setup guide before you start.\nSet MAX_TOKENS in your config_file."
