import logging


logger = logging.getLogger(__name__)


def download_model(model_name: str):
    try:
        from gpt4all import GPT4All
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    return GPT4All(model_name=model_name)
