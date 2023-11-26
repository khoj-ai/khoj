import logging

from khoj.utils import state

logger = logging.getLogger(__name__)


def download_model(model_name: str):
    try:
        import gpt4all
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    # Download the chat model
    chat_model_config = gpt4all.GPT4All.retrieve_model(model_name=model_name, allow_download=True)

    # Decide whether to load model to GPU or CPU
    try:
        # Try load chat model to GPU if:
        # 1. Loading chat model to GPU isn't disabled via CLI and
        # 2. Machine has GPU
        # 3. GPU has enough free memory to load the chat model
        device = (
            "gpu" if state.chat_on_gpu and gpt4all.pyllmodel.LLModel().list_gpu(chat_model_config["path"]) else "cpu"
        )
    except ValueError:
        device = "cpu"

    # Now load the downloaded chat model onto appropriate device
    chat_model = gpt4all.GPT4All(model_name=model_name, device=device, allow_download=False)
    logger.debug(f"Loaded chat model to {device.upper()}.")

    return chat_model
