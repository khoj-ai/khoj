import pytest


@pytest.fixture(autouse=True)
def _no_db(monkeypatch):
    """Skip database access for these pure unit tests."""
    pass


class TestIsGroqApi:
    def test_groq_api_url(self):
        from khoj.processor.conversation.openai.utils import is_groq_api

        assert is_groq_api("https://api.groq.com/openai/v1") is True

    def test_groq_api_base_url(self):
        from khoj.processor.conversation.openai.utils import is_groq_api

        assert is_groq_api("https://api.groq.com") is True

    def test_non_groq_api_url(self):
        from khoj.processor.conversation.openai.utils import is_groq_api

        assert is_groq_api("https://api.openai.com/v1") is False

    def test_none_url(self):
        from khoj.processor.conversation.openai.utils import is_groq_api

        assert is_groq_api(None) is False

    def test_empty_url(self):
        from khoj.processor.conversation.openai.utils import is_groq_api

        assert is_groq_api("") is False


class TestMaxCompletionTokensConstants:
    def test_groq_max_tokens_less_than_default(self):
        """Groq API max_completion_tokens should be less than the default"""
        from khoj.processor.conversation.openai.utils import (
            GROQ_MAX_COMPLETION_TOKENS,
            MAX_COMPLETION_TOKENS,
        )

        assert GROQ_MAX_COMPLETION_TOKENS < MAX_COMPLETION_TOKENS

    def test_groq_max_tokens_is_8192(self):
        """Groq API max_completion_tokens should be 8192 based on API limits"""
        from khoj.processor.conversation.openai.utils import GROQ_MAX_COMPLETION_TOKENS

        assert GROQ_MAX_COMPLETION_TOKENS == 8192
