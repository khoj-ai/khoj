import logging
from threading import Thread
from typing import Dict, List

import anthropic
from tenacity import (
    before_sleep_log,
    retry,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import ThreadedGenerator

logger = logging.getLogger(__name__)

anthropic_clients: Dict[str, anthropic.Anthropic] = {}


DEFAULT_MAX_TOKENS_ANTHROPIC = 3000


@retry(
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def anthropic_completion_with_backoff(
    messages, system_prompt, model_name, temperature=0, api_key=None, model_kwargs=None, max_tokens=None
) -> str:
    if api_key not in anthropic_clients:
        client: anthropic.Anthropic = anthropic.Anthropic(api_key=api_key)
        anthropic_clients[api_key] = client
    else:
        client = anthropic_clients[api_key]

    formatted_messages = [{"role": message.role, "content": message.content} for message in messages]

    aggregated_response = ""
    max_tokens = max_tokens or DEFAULT_MAX_TOKENS_ANTHROPIC

    model_kwargs = model_kwargs or dict()
    if system_prompt:
        model_kwargs["system"] = system_prompt

    with client.messages.stream(
        messages=formatted_messages,
        model=model_name,  # type: ignore
        temperature=temperature,
        timeout=20,
        max_tokens=max_tokens,
        **(model_kwargs),
    ) as stream:
        for text in stream.text_stream:
            aggregated_response += text

    return aggregated_response


@retry(
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def anthropic_chat_completion_with_backoff(
    messages,
    compiled_references,
    online_results,
    model_name,
    temperature,
    api_key,
    system_prompt,
    max_prompt_size=None,
    completion_func=None,
    model_kwargs=None,
):
    g = ThreadedGenerator(compiled_references, online_results, completion_func=completion_func)
    t = Thread(
        target=anthropic_llm_thread,
        args=(g, messages, system_prompt, model_name, temperature, api_key, max_prompt_size, model_kwargs),
    )
    t.start()
    return g


def anthropic_llm_thread(
    g, messages, system_prompt, model_name, temperature, api_key, max_prompt_size=None, model_kwargs=None
):
    if api_key not in anthropic_clients:
        client: anthropic.Anthropic = anthropic.Anthropic(api_key=api_key)
        anthropic_clients[api_key] = client
    else:
        client: anthropic.Anthropic = anthropic_clients[api_key]

    formatted_messages: List[anthropic.types.MessageParam] = [
        anthropic.types.MessageParam(role=message.role, content=message.content) for message in messages
    ]

    with client.messages.stream(
        messages=formatted_messages,
        model=model_name,  # type: ignore
        temperature=temperature,
        system=system_prompt,
        timeout=20,
        max_tokens=DEFAULT_MAX_TOKENS_ANTHROPIC,
        **(model_kwargs or dict()),
    ) as stream:
        for text in stream.text_stream:
            g.send(text)

    g.close()
