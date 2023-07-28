import os
import logging
import requests
from gpt4all import GPT4All
import tqdm

from khoj.processor.conversation.gpt4all import model_metadata

logger = logging.getLogger(__name__)


def download_model(model_name):
    url = model_metadata.model_name_to_url.get(model_name)
    if not url:
        logger.debug(f"Model {model_name} not found in model metadata. Skipping download.")
        return GPT4All(model_name)

    filename = os.path.expanduser(f"~/.cache/gpt4all/{model_name}")
    if os.path.exists(filename):
        return GPT4All(model_name)

    try:
        os.makedirs(os.path.dirname(filename), exist_ok=True)
        logger.debug(f"Downloading model {model_name} from {url} to {filename}...")
        with requests.get(url, stream=True) as r:
            r.raise_for_status()
            with open(filename, "wb") as f:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)
        return GPT4All(model_name)
    except Exception as e:
        logger.error(f"Failed to download model {model_name} from {url} to {filename}. Error: {e}")
        return None
