import glob
import logging
import os

from huggingface_hub.constants import HF_HUB_CACHE

from khoj.utils import state

logger = logging.getLogger(__name__)


def download_model(repo_id: str, filename: str = "*Q4_K_M.gguf"):
    from llama_cpp.llama import Llama

    # Initialize Model Parameters. Use n_ctx=0 to get context size from the model
    kwargs = {"n_threads": 4, "n_ctx": 0, "verbose": False}

    # Decide whether to load model to GPU or CPU
    device = "gpu" if state.chat_on_gpu and state.device != "cpu" else "cpu"
    kwargs["n_gpu_layers"] = -1 if device == "gpu" else 0

    # Check if the model is already downloaded
    model_path = load_model_from_cache(repo_id, filename)
    chat_model = None
    try:
        if model_path:
            chat_model = Llama(model_path, **kwargs)
        else:
            Llama.from_pretrained(repo_id=repo_id, filename=filename, **kwargs)
    except:
        # Load model on CPU if GPU is not available
        kwargs["n_gpu_layers"], device = 0, "cpu"
        if model_path:
            chat_model = Llama(model_path, **kwargs)
        else:
            chat_model = Llama.from_pretrained(repo_id=repo_id, filename=filename, **kwargs)

    logger.debug(f"{'Loaded' if model_path else 'Downloaded'} chat model to {device.upper()}")

    return chat_model


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
