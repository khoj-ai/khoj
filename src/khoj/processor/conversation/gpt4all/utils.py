import os
import logging
import requests
from gpt4all import GPT4All
from tqdm import tqdm

from khoj.processor.conversation.gpt4all import model_metadata

logger = logging.getLogger(__name__)


def download_model(model_name: str):
    url = model_metadata.model_name_to_url.get(model_name)
    if not url:
        logger.debug(f"Model {model_name} not found in model metadata. Skipping download.")
        return GPT4All(model_name)

    filename = os.path.expanduser(f"~/.cache/gpt4all/{model_name}")
    if os.path.exists(filename):
        return GPT4All(model_name)

    # Download the model to a tmp file. Once the download is completed, move the tmp file to the actual file
    tmp_filename = filename + ".tmp"

    try:
        os.makedirs(os.path.dirname(tmp_filename), exist_ok=True)
        logger.debug(f"Downloading model {model_name} from {url} to {tmp_filename}...")
        with requests.get(url, stream=True) as r:
            r.raise_for_status()
            total_size = int(r.headers.get("content-length", 0))
            with open(tmp_filename, "wb") as f, tqdm(
                unit="B",  # unit string to be displayed.
                unit_scale=True,  # let tqdm to determine the scale in kilo, mega..etc.
                unit_divisor=1024,  # is used when unit_scale is true
                total=total_size,  # the total iteration.
                desc=filename.split("/")[-1],  # prefix to be displayed on progress bar.
            ) as progress_bar:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)
                    progress_bar.update(len(chunk))

        logger.debug(f"Successfully downloaded model {model_name} from {url} to {tmp_filename}")
        # Move the tmp file to the actual file
        os.rename(tmp_filename, filename)
        return GPT4All(model_name)
    except Exception as e:
        logger.error(f"Failed to download model {model_name} from {url} to {tmp_filename}. Error: {e}")
        # Remove the tmp file if it exists
        if os.path.exists(tmp_filename):
            os.remove(tmp_filename)
        return None
