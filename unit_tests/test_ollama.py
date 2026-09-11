import pytest
import requests

from khoj.utils.ollama import (
    _safe_base_url,
    format_ollama_error,
    get_ollama_models,
)


def test_safe_base_url_removes_openai_v1_suffix_and_credentials():
    assert (
        _safe_base_url("http://user:secret@localhost:11434/v1/")
        == "http://localhost:11434"
    )


def test_safe_base_url_removes_query_and_fragment():
    assert (
        _safe_base_url("http://localhost:11434/v1/?foo=bar#test")
        == "http://localhost:11434"
    )


def test_get_ollama_models_returns_sorted_unique_names(monkeypatch):
    class Response:
        def raise_for_status(self):
            pass

        def json(self):
            return {
                "models": [
                    {"name": "llama3.2"},
                    {"name": "Qwen3"},
                    {"name": "llama3.2"},
                    {"name": ""},
                    {},
                ]
            }

    captured = {}

    def fake_get(url, timeout):
        captured["url"] = url
        captured["timeout"] = timeout
        return Response()

    monkeypatch.setattr("khoj.utils.ollama.requests.get", fake_get)

    assert get_ollama_models("http://localhost:11434/v1/") == [
        "llama3.2",
        "Qwen3",
    ]

    assert captured == {
        "url": "http://localhost:11434/api/tags",
        "timeout": 5,
    }


def test_get_ollama_models_rejects_non_http_urls():
    with pytest.raises(ValueError, match="absolute HTTP"):
        get_ollama_models("file:///etc/passwd")


def test_get_ollama_models_rejects_malformed_payload(monkeypatch):
    class Response:
        def raise_for_status(self):
            pass

        def json(self):
            return {"unexpected": "value"}

    monkeypatch.setattr(
        "khoj.utils.ollama.requests.get",
        lambda url, timeout: Response(),
    )

    with pytest.raises(ValueError, match="unexpected /api/tags response"):
        get_ollama_models("http://localhost:11434")


def test_get_ollama_models_rejects_invalid_json(monkeypatch):
    class Response:
        def raise_for_status(self):
            pass

        def json(self):
            raise ValueError("invalid json")

    monkeypatch.setattr(
        "khoj.utils.ollama.requests.get",
        lambda url, timeout: Response(),
    )

    with pytest.raises(ValueError, match="JSON"):
        get_ollama_models("http://localhost:11434")


def test_format_ollama_error_connection_error():
    error = format_ollama_error(
        "http://localhost:11434",
        requests.ConnectionError("connection refused"),
    )

    assert "Unable to connect to Ollama" in error
    assert "localhost:11434" in error


def test_format_ollama_error_timeout():
    error = format_ollama_error(
        "http://localhost:11434",
        requests.Timeout("timed out"),
    )

    assert "timed out" in error.lower()


def test_format_ollama_error_does_not_expose_credentials():
    error = format_ollama_error(
        "http://user:secret@localhost:11434",
        RuntimeError("connection refused"),
    )

    assert error == (
        "Unable to connect to Ollama at "
        "http://localhost:11434: connection refused"
    )
    assert "secret" not in error