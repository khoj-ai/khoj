# Standard Packages
import os
import logging
from typing import Any
from threading import Thread

# External Packages
from langchain.chat_models import ChatOpenAI
from langchain.callbacks.streaming_stdout import StreamingStdOutCallbackHandler
from langchain.callbacks.base import BaseCallbackManager
import openai
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

# Internal Packages
from khoj.processor.conversation.utils import ThreadedGenerator


logger = logging.getLogger(__name__)


class StreamingChatCallbackHandler(StreamingStdOutCallbackHandler):
    def __init__(self, gen: ThreadedGenerator):
        super().__init__()
        self.gen = gen

    def on_llm_new_token(self, token: str, **kwargs) -> Any:
        self.gen.send(token)


@retry(
    retry=(
        retry_if_exception_type(openai.error.Timeout)
        | retry_if_exception_type(openai.error.APIError)
        | retry_if_exception_type(openai.error.APIConnectionError)
        | retry_if_exception_type(openai.error.RateLimitError)
        | retry_if_exception_type(openai.error.ServiceUnavailableError)
    ),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def completion_with_backoff(**kwargs):
    messages = kwargs.pop("messages")
    if not "openai_api_key" in kwargs:
        kwargs["openai_api_key"] = os.getenv("OPENAI_API_KEY")
    llm = ChatOpenAI(**kwargs, request_timeout=20, max_retries=1)
    return llm(messages=messages)


@retry(
    retry=(
        retry_if_exception_type(openai.error.Timeout)
        | retry_if_exception_type(openai.error.APIError)
        | retry_if_exception_type(openai.error.APIConnectionError)
        | retry_if_exception_type(openai.error.RateLimitError)
        | retry_if_exception_type(openai.error.ServiceUnavailableError)
    ),
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def chat_completion_with_backoff(
    messages, compiled_references, model_name, temperature, openai_api_key=None, completion_func=None, model_kwargs=None
):
    g = ThreadedGenerator(compiled_references, completion_func=completion_func)
    t = Thread(target=llm_thread, args=(g, messages, model_name, temperature, openai_api_key, model_kwargs))
    t.start()
    return g


def llm_thread(g, messages, model_name, temperature, openai_api_key=None, model_kwargs=None):
    callback_handler = StreamingChatCallbackHandler(g)
    chat = ChatOpenAI(
        streaming=True,
        verbose=True,
        callback_manager=BaseCallbackManager([callback_handler]),
        model_name=model_name,  # type: ignore
        temperature=temperature,
        openai_api_key=openai_api_key or os.getenv("OPENAI_API_KEY"),
        model_kwargs=model_kwargs,
        request_timeout=20,
        max_retries=1,
        client=None,
    )

    chat(messages=messages)

    g.close()


def extract_summaries(metadata):
    """Extract summaries from metadata"""
    return "".join([f'\n{session["summary"]}' for session in metadata])
