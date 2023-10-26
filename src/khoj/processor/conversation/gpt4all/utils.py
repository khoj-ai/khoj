import logging

from khoj.utils import state


logger = logging.getLogger(__name__)


def download_model(model_name: str):
    try:
        from gpt4all import GPT4All
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    # Use GPU for Chat Model, if available
    try:
        device = "gpu" if state.chat_on_gpu else "cpu"
        model = GPT4All(model_name=model_name, device=device)
        logger.debug(f"Loaded {model_name} chat model to {device.upper()}")
    except ValueError:
        model = GPT4All(model_name=model_name)
        logger.debug(f"Loaded {model_name} chat model to CPU.")

    return model
