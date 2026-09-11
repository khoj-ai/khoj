import re
from urllib.parse import urlparse, urlunparse

import requests


OLLAMA_TAGS_PATH = "/api/tags"
OLLAMA_TIMEOUT_SECONDS = 5


def _safe_base_url(base_url: str) -> str:
    """Normalize an Ollama URL and remove credentials before displaying it."""
    parsed = urlparse(base_url.strip())
    if parsed.scheme not in {"http", "https"} or not parsed.netloc:
        raise ValueError("Ollama base URL must be an absolute HTTP(S) URL")

    # Ollama's OpenAI-compatible endpoint is commonly configured as /v1.
    path = parsed.path.rstrip("/")
    if path.endswith("/v1"):
        path = path[:-3]

    hostname = parsed.hostname
    if not hostname:
        raise ValueError("Ollama base URL must include a host")

    host = f"[{hostname}]" if ":" in hostname and not hostname.startswith("[") else hostname
    if parsed.port:
        host = f"{host}:{parsed.port}"

    safe = parsed._replace(
        netloc=host,
        path=path,
        params="",
        query="",
        fragment="",
    )
    return urlunparse(safe).rstrip("/")


def get_ollama_models(base_url: str, timeout: float = OLLAMA_TIMEOUT_SECONDS) -> list[str]:
    """Return model names reported by Ollama's native /api/tags endpoint."""
    normalized_url = _safe_base_url(base_url)
    response = requests.get(f"{normalized_url}{OLLAMA_TAGS_PATH}", timeout=timeout)
    response.raise_for_status()

    try:
        payload = response.json()
    except ValueError as exc:
        raise ValueError("Ollama returned an invalid JSON response") from exc

    models = payload.get("models")
    if not isinstance(models, list):
        raise ValueError("Ollama returned an unexpected /api/tags response")

    model_names = []
    for model in models:
        if isinstance(model, dict) and isinstance(model.get("name"), str) and model["name"].strip():
            model_names.append(model["name"].strip())

    return sorted(set(model_names), key=str.casefold)


def format_ollama_error(base_url: str, exc: Exception) -> str:
    """Create a short, user-facing error without exposing URL credentials."""
    try:
        safe_url = _safe_base_url(base_url)
    except Exception:
        safe_url = base_url.strip()

    message = str(exc).strip() or exc.__class__.__name__
    message = re.sub(r"(?i)(https?://)([^/@\s]+)@", r"\1", message)
    return f"Unable to connect to Ollama at {safe_url}: {message}"
