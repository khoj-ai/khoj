from pathlib import Path

app_root_directory = Path(__file__).parent.parent.parent
web_directory = app_root_directory / "khoj/interface/web/"
empty_escape_sequences = "\n|\r|\t| "
app_env_filepath = "~/.khoj/env"
telemetry_server = "https://khoj.beta.haletic.com/v1/telemetry"
content_directory = "~/.khoj/content/"
default_offline_chat_model = "mistral-7b-instruct-v0.1.Q4_0.gguf"
default_online_chat_model = "gpt-4"

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
