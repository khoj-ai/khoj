from pathlib import Path
from typing import Dict

app_root_directory = Path(__file__).parent.parent.parent
web_directory = app_root_directory / "khoj/interface/web/"
next_js_directory = app_root_directory / "khoj/interface/built/"
pypi_static_directory = app_root_directory / "khoj/interface/compiled/"
assetlinks_file_path = web_directory / ".well-known/assetlinks.json"
empty_escape_sequences = "\n|\r|\t| "
app_env_filepath = "~/.khoj/env"
telemetry_server = "https://khoj.beta.haletic.com/v1/telemetry"
content_directory = "~/.khoj/content/"
default_openai_chat_models = ["gpt-4o-mini", "gpt-4.1", "o3", "o4-mini"]
default_gemini_chat_models = ["gemini-2.0-flash", "gemini-2.5-flash", "gemini-2.5-pro", "gemini-2.5-flash-lite"]
default_anthropic_chat_models = ["claude-sonnet-4-0", "claude-3-5-haiku-latest"]

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

model_to_cost: Dict[str, Dict[str, float]] = {
    # OpenAI Pricing: https://openai.com/api/pricing/
    "gpt-4o": {"input": 2.50, "output": 10.00},
    "gpt-4o-mini": {"input": 0.15, "output": 0.60},
    "gpt-4.1": {"input": 2.00, "output": 8.00},
    "gpt-4.1-mini": {"input": 0.40, "output": 1.60},
    "gpt-4.1-nano": {"input": 0.10, "output": 0.40},
    "o1-mini": {"input": 3.0, "output": 12.0},
    "o1": {"input": 15.0, "output": 60.00},
    "o3-mini": {"input": 1.10, "output": 4.40},
    "o3": {"input": 2.0, "output": 8.00},
    "o3-pro": {"input": 20.0, "output": 80.00},
    "o4-mini": {"input": 1.10, "output": 4.40},
    "gpt-5-2025-08-07": {"input": 1.25, "output": 10.00, "cache_read": 0.125},
    "gpt-5-mini-2025-08-07": {"input": 0.25, "output": 2.00, "cache_read": 0.025},
    "gpt-5-nano-2025-08-07": {"input": 0.05, "output": 0.40, "cache_read": 0.005},
    # Gemini Pricing: https://ai.google.dev/pricing
    "gemini-1.5-flash": {"input": 0.075, "output": 0.30},
    "gemini-1.5-flash-002": {"input": 0.075, "output": 0.30},
    "gemini-1.5-pro": {"input": 1.25, "output": 5.00},
    "gemini-1.5-pro-002": {"input": 1.25, "output": 5.00},
    "gemini-2.0-flash": {"input": 0.10, "output": 0.40},
    "gemini-2.0-flash-lite": {"input": 0.0075, "output": 0.30},
    "gemini-2.5-flash-lite": {"input": 0.10, "output": 0.40},
    "gemini-2.5-flash": {"input": 0.30, "cache_read_tokens": 0.03, "output": 2.50},
    "gemini-2.5-pro": {"input": 1.25, "cache_read_tokens": 0.125, "output": 10.0},
    "gemini-3-pro-preview": {"input": 2.00, "cache_read_tokens": 0.20, "output": 12.0},
    # Anthropic Pricing: https://www.anthropic.com/pricing#anthropic-api
    "claude-3-5-haiku-20241022": {"input": 1.0, "output": 5.0, "cache_read": 0.08, "cache_write": 1.0},
    "claude-3-5-haiku@20241022": {"input": 1.0, "output": 5.0, "cache_read": 0.08, "cache_write": 1.0},
    "claude-3-5-sonnet-20241022": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-3-5-sonnet-latest": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-3-7-sonnet-20250219": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-3-7-sonnet@20250219": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-3-7-sonnet-latest": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-sonnet-4-0": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-sonnet-4-20250514": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-sonnet-4@20250514": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-opus-4-0": {"input": 15.0, "output": 75.0, "cache_read": 1.50, "cache_write": 18.75},
    "claude-opus-4-20250514": {"input": 15.0, "output": 75.0, "cache_read": 1.50, "cache_write": 18.75},
    "claude-opus-4@20250514": {"input": 15.0, "output": 75.0, "cache_read": 1.50, "cache_write": 18.75},
    "claude-sonnet-4-5": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-sonnet-4-5-20250929": {"input": 3.0, "output": 15.0, "cache_read": 0.3, "cache_write": 3.75},
    "claude-haiku-4-5": {"input": 1.0, "output": 5.0, "cache_read": 0.08, "cache_write": 1.0},
    "claude-haiku-4-5-20251001": {"input": 1.0, "output": 5.0, "cache_read": 0.08, "cache_write": 1.0},
    # Grok pricing: https://docs.x.ai/docs/models
    "grok-3": {"input": 3.0, "output": 15.0},
    "grok-3-latest": {"input": 3.0, "output": 15.0},
    "grok-3-mini": {"input": 0.30, "output": 0.50},
    "grok-3-mini-latest": {"input": 0.30, "output": 0.50},
    "grok-4": {"input": 3.0, "cache_read": 0.75, "output": 15.0},
    "grok-4-fast": {"input": 0.20, "cache_read": 0.05, "output": 0.50},
    # Groq pricing
    "moonshotai/kimi-k2-instruct-0905": {"input": 1.00, "output": 3.00},
    "openai/gpt-oss-120b": {"input": 0.15, "output": 0.75},
    "openai/gpt-oss-20b": {"input": 0.10, "output": 0.50},
    # Miscellaneous
    # Moonshot AI, Baseten pricing for Kimi-K2-Thinking
    "moonshotai/kimi-k2-thinking": {"input": 0.60, "output": 2.50},
}
