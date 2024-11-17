from pathlib import Path

app_root_directory = Path(__file__).parent.parent.parent
web_directory = app_root_directory / "khoj/interface/web/"
next_js_directory = app_root_directory / "khoj/interface/built/"
pypi_static_directory = app_root_directory / "khoj/interface/compiled/"
empty_escape_sequences = "\n|\r|\t| "
app_env_filepath = "~/.khoj/env"
telemetry_server = "https://khoj.beta.haletic.com/v1/telemetry"
content_directory = "~/.khoj/content/"
default_offline_chat_models = [
    "bartowski/Meta-Llama-3.1-8B-Instruct-GGUF",
    "bartowski/gemma-2-9b-it-GGUF",
    "bartowski/gemma-2-2b-it-GGUF",
    "bartowski/Phi-3.5-mini-instruct-GGUF",
]
default_openai_chat_models = ["gpt-4o-mini", "gpt-4o"]
default_gemini_chat_models = ["gemini-1.5-flash", "gemini-1.5-pro"]
default_anthropic_chat_models = ["claude-3-5-sonnet-20241022", "claude-3-5-haiku-20241022"]

empty_config = {
    "search-type": {
        "image": {"encoder": "sentence-transformers/clip-ViT-B-32", "model_directory": "~/.khoj/search/image/"},
    },
}

# default app config to use
default_config = {
    "search-type": {
        "image": {"encoder": "sentence-transformers/clip-ViT-B-32", "model_directory": "~/.khoj/search/image/"},
    },
}
