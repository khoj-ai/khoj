import logging
from threading import Thread
from typing import Dict, List

import anthropic
from langchain.schema import ChatMessage
from tenacity import (
    before_sleep_log,
    retry,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import ThreadedGenerator, get_image_from_url
from khoj.utils.helpers import is_none_or_empty

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
    try:
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
    except Exception as e:
        logger.error(f"Error in anthropic_llm_thread: {e}", exc_info=True)
    finally:
        g.close()


def format_messages_for_anthropic(messages: list[ChatMessage], system_prompt=None):
    """
    Format messages for Anthropic
    """
    # Extract system prompt
    system_prompt = system_prompt or ""
    for message in messages.copy():
        if message.role == "system":
            system_prompt += message.content
            messages.remove(message)
    system_prompt = None if is_none_or_empty(system_prompt) else system_prompt

    # Anthropic requires the first message to be a 'user' message
    if len(messages) == 1:
        messages[0].role = "user"
    elif len(messages) > 1 and messages[0].role == "assistant":
        messages = messages[1:]

    # Convert image urls to base64 encoded images in Anthropic message format
    for message in messages:
        if isinstance(message.content, list):
            content = []
            # Sort the content. Anthropic models prefer that text comes after images.
            message.content.sort(key=lambda x: 0 if x["type"] == "image_url" else 1)
            for idx, part in enumerate(message.content):
                if part["type"] == "text":
                    content.append({"type": "text", "text": part["text"]})
                elif part["type"] == "image_url":
                    image = get_image_from_url(part["image_url"]["url"], type="b64")
                    # Prefix each image with text block enumerating the image number
                    # This helps the model reference the image in its response. Recommended by Anthropic
                    content.extend(
                        [
                            {
                                "type": "text",
                                "text": f"Image {idx + 1}:",
                            },
                            {
                                "type": "image",
                                "source": {"type": "base64", "media_type": image.type, "data": image.content},
                            },
                        ]
                    )
            message.content = content

    return messages, system_prompt
