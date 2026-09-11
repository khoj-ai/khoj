import json
from types import SimpleNamespace

import pytest
from django.test import RequestFactory

from khoj.database.admin import ChatModelAdmin
from khoj.database.models import AiModelApi


@pytest.mark.parametrize(
    "payload, expected_status, expected_error",
    [
        ({}, 400, "Select an AI Model API first."),
        ({"api_id": "missing"}, 404, "AI Model API configuration not found."),
    ],
)
def test_ollama_test_connection_validation(monkeypatch, payload, expected_status, expected_error):
    factory = RequestFactory()
    request = factory.post(
        "/server/admin/database/chatmodel/ollama-test-connection/",
        data=json.dumps(payload),
        content_type="application/json",
    )

    class FakeManager:
        def get(self, pk):
            if pk == "missing":
                raise AiModelApi.DoesNotExist
            return SimpleNamespace(
                name="Ollama",
                api_base_url="http://localhost:11434/v1/",
            )

    monkeypatch.setattr(AiModelApi, "objects", FakeManager())

    admin = ChatModelAdmin.__new__(ChatModelAdmin)
    response = admin.ollama_test_connection(request)

    assert response.status_code == expected_status
    assert json.loads(response.content)["error"] == expected_error


def test_ollama_test_connection_returns_models(monkeypatch):
    factory = RequestFactory()
    request = factory.post(
        "/server/admin/database/chatmodel/ollama-test-connection/",
        data=json.dumps(
            {
                "api_id": "1",
                "model": "llama3.2:latest",
            }
        ),
        content_type="application/json",
    )

    api_config = SimpleNamespace(
        name="Ollama",
        api_base_url="http://localhost:11434/v1/",
    )

    class FakeManager:
        def get(self, pk):
            assert pk == "1"
            return api_config

    monkeypatch.setattr(AiModelApi, "objects", FakeManager())
    monkeypatch.setattr(
        "khoj.database.admin.get_ollama_models",
        lambda url: ["llama3.2:latest", "qwen3"],
    )

    admin = ChatModelAdmin.__new__(ChatModelAdmin)
    response = admin.ollama_test_connection(request)

    assert response.status_code == 200
    assert json.loads(response.content) == {
        "ok": True,
        "models": ["llama3.2:latest", "qwen3"],
        "model_available": True,
        "message": "Connected — 2 models available.",
    }