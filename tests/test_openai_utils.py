import pytest

from khoj.processor.conversation.openai.utils import (
    GROQ_DEFAULT_MAX_COMPLETION_TOKENS,
    MAX_COMPLETION_TOKENS,
    groq_max_completion_tokens,
    is_groq_api,
)


class TestIsGroqApi:
    def test_groq_api_url(self):
        assert is_groq_api("https://api.groq.com/openai/v1") is True

    def test_groq_api_url_without_path(self):
        assert is_groq_api("https://api.groq.com") is True

    def test_openai_api_url(self):
        assert is_groq_api("https://api.openai.com/v1") is False

    def test_other_api_url(self):
        assert is_groq_api("https://api.example.com/v1") is False

    def test_none_url(self):
        assert is_groq_api(None) is False


class TestMaxCompletionTokensConstants:
    def test_max_completion_tokens_value(self):
        assert MAX_COMPLETION_TOKENS == 16000

    def test_groq_default_max_completion_tokens_value(self):
        assert GROQ_DEFAULT_MAX_COMPLETION_TOKENS == 8192

    def test_groq_max_is_less_than_default(self):
        assert GROQ_DEFAULT_MAX_COMPLETION_TOKENS < MAX_COMPLETION_TOKENS


class TestGroqMaxCompletionTokens:
    def test_groq_max_from_model_name_8192(self):
        assert groq_max_completion_tokens("llama3-8b-8192") == 8192

    def test_groq_max_from_model_name_32768(self):
        assert groq_max_completion_tokens("model-32768") == 32768

    def test_groq_max_fallback(self):
        assert groq_max_completion_tokens("llama-3.1-70b-versatile") == GROQ_DEFAULT_MAX_COMPLETION_TOKENS
