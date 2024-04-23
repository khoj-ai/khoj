import glob
import logging
import math
import os
from typing import Any, Dict

from huggingface_hub.constants import HF_HUB_CACHE

from khoj.utils import state
from khoj.utils.helpers import get_device_memory

logger = logging.getLogger(__name__)


def download_model(repo_id: str, filename: str = "*Q4_K_M.gguf", max_tokens: int = None):
    # Initialize Model Parameters
    # Use n_ctx=0 to get context size from the model
    kwargs: Dict[str, Any] = {"n_threads": 4, "n_ctx": 0, "verbose": False}

    # Decide whether to load model to GPU or CPU
    device = "gpu" if state.chat_on_gpu and state.device != "cpu" else "cpu"
    kwargs["n_gpu_layers"] = -1 if device == "gpu" else 0

    # Add chat format if known
    if "llama-3" in repo_id.lower():
        kwargs["chat_format"] = "llama-3"

    # Check if the model is already downloaded
    model_path = load_model_from_cache(repo_id, filename)
    chat_model = None
    try:
        chat_model = load_model(model_path, repo_id, filename, kwargs)
    except:
        # Load model on CPU if GPU is not available
        kwargs["n_gpu_layers"], device = 0, "cpu"
        chat_model = load_model(model_path, repo_id, filename, kwargs)

    # Now load the model with context size set based on:
    # 1. context size supported by model and
    # 2. configured size or machine (V)RAM
    kwargs["n_ctx"] = infer_max_tokens(chat_model.n_ctx(), max_tokens)
    chat_model = load_model(model_path, repo_id, filename, kwargs)

    logger.debug(
        f"{'Loaded' if model_path else 'Downloaded'} chat model to {device.upper()} with {kwargs['n_ctx']} token context window."
    )
    return chat_model


def load_model(model_path: str, repo_id: str, filename: str = "*Q4_K_M.gguf", kwargs: dict = {}):
    from llama_cpp.llama import Llama

    if model_path:
        return Llama(model_path, **kwargs)
    else:
        return Llama.from_pretrained(repo_id=repo_id, filename=filename, **kwargs)


def load_model_from_cache(repo_id: str, filename: str, repo_type="models"):
    # Construct the path to the model file in the cache directory
    repo_org, repo_name = repo_id.split("/")
    object_id = "--".join([repo_type, repo_org, repo_name])
    model_path = os.path.sep.join([HF_HUB_CACHE, object_id, "snapshots", "**", filename])

    # Check if the model file exists
    paths = glob.glob(model_path)
    if paths:
        return paths[0]
    else:
        return None


def infer_max_tokens(model_context_window: int, configured_max_tokens=None) -> int:
    """Infer max prompt size based on device memory and max context window supported by the model"""
    configured_max_tokens = math.inf if configured_max_tokens is None else configured_max_tokens
    vram_based_n_ctx = int(get_device_memory() / 2e6)  # based on heuristic
    configured_max_tokens = configured_max_tokens or math.inf  # do not use if set to None
    return min(configured_max_tokens, vram_based_n_ctx, model_context_window)
