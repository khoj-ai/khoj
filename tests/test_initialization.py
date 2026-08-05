import pytest

from khoj.database.models import AiModelApi, ChatModel
from khoj.utils.initialization import initialization


@pytest.mark.django_db
def test_initialization_adds_minimax_openai_and_anthropic_providers(monkeypatch):
    for variable in [
        "ANTHROPIC_API_KEY",
        "GEMINI_API_KEY",
        "MINIMAX_ANTHROPIC_API_KEY",
        "OPENAI_API_KEY",
        "OPENAI_BASE_URL",
    ]:
        monkeypatch.delenv(variable, raising=False)

    monkeypatch.setenv("KHOJ_ADMIN_EMAIL", "admin@example.com")
    monkeypatch.setenv("KHOJ_ADMIN_PASSWORD", "test-password")
    monkeypatch.setenv("MINIMAX_API_KEY", "test-key")
    monkeypatch.setenv("MINIMAX_BASE_URL", "https://api.minimaxi.com/v1")
    monkeypatch.setenv("MINIMAX_ANTHROPIC_BASE_URL", "https://api.minimaxi.com/anthropic")

    initialization(interactive=False)

    openai_provider = AiModelApi.objects.get(name="MiniMax")
    anthropic_provider = AiModelApi.objects.get(name="MiniMax Anthropic")
    assert openai_provider.api_base_url == "https://api.minimaxi.com/v1"
    assert anthropic_provider.api_base_url == "https://api.minimaxi.com/anthropic"

    expected_models = {
        "MiniMax-M3": {"max_prompt_size": 1_000_000, "vision_enabled": True},
        "MiniMax-M2.7": {"max_prompt_size": 204_800, "vision_enabled": False},
    }
    for provider, model_type in [
        (openai_provider, ChatModel.ModelType.OPENAI),
        (anthropic_provider, ChatModel.ModelType.ANTHROPIC),
    ]:
        models = ChatModel.objects.filter(ai_model_api=provider, model_type=model_type)
        assert set(models.values_list("name", flat=True)) == set(expected_models)
        for model_name, expected in expected_models.items():
            model = models.get(name=model_name)
            assert model.max_prompt_size == expected["max_prompt_size"]
            assert model.vision_enabled is expected["vision_enabled"]
