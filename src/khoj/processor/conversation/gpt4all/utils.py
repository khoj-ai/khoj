import logging


logger = logging.getLogger(__name__)


def download_model(model_name: str):
    try:
        from gpt4all import GPT4All
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    # Use GPU for Chat Model, if available
    try:
        model = GPT4All(model_name=model_name, device="gpu")
        logger.debug(f"Loaded {model_name} chat model to GPU.")
    except ValueError:
        model = GPT4All(model_name=model_name)
        logger.debug(f"Loaded {model_name} chat model to CPU.")

    return model
