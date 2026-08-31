from khoj.utils.initialization import _openai_compatible_provider_name


def test_names_local_runners_by_their_default_port():
    assert _openai_compatible_provider_name("http://localhost:11434/v1/") == "Ollama"
    assert _openai_compatible_provider_name("http://localhost:17434/v1/") == "llmman"
    assert _openai_compatible_provider_name("http://127.0.0.1:17434/v1/") == "llmman"


def test_falls_back_to_openai():
    # Anything else, including no base URL at all, is OpenAI itself.
    assert _openai_compatible_provider_name(None) == "OpenAI"
    assert _openai_compatible_provider_name("") == "OpenAI"
    assert _openai_compatible_provider_name("https://api.openai.com/v1/") == "OpenAI"
    # A matching port but a different path must not match.
    assert _openai_compatible_provider_name("http://localhost:17434/") == "OpenAI"
