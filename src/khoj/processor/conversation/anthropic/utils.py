import logging
from time import perf_counter
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

from khoj.processor.conversation.utils import (
    commit_conversation_trace,
    get_image_from_base64,
    get_image_from_url,
)
from khoj.utils.helpers import (
    get_anthropic_async_client,
    get_anthropic_client,
    get_chat_usage_metrics,
    is_none_or_empty,
    is_promptrace_enabled,
)

logger = logging.getLogger(__name__)

anthropic_clients: Dict[str, anthropic.Anthropic | anthropic.AnthropicVertex] = {}
anthropic_async_clients: Dict[str, anthropic.AsyncAnthropic | anthropic.AsyncAnthropicVertex] = {}

DEFAULT_MAX_TOKENS_ANTHROPIC = 8000
MAX_REASONING_TOKENS_ANTHROPIC = 12000


@retry(
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def anthropic_completion_with_backoff(
    messages: list[ChatMessage],
    system_prompt,
    model_name: str,
    temperature=0.4,
    api_key=None,
    api_base_url: str = None,
    model_kwargs=None,
    max_tokens=None,
    response_type="text",
    deepthought=False,
    tracer={},
) -> str:
    client = anthropic_clients.get(api_key)
    if not client:
        client = get_anthropic_client(api_key, api_base_url)
        anthropic_clients[api_key] = client

    formatted_messages, system_prompt = format_messages_for_anthropic(messages, system_prompt)

    aggregated_response = ""
    if response_type == "json_object" and not deepthought:
        # Prefill model response with '{' to make it output a valid JSON object. Not supported with extended thinking.
        formatted_messages.append(anthropic.types.MessageParam(role="assistant", content="{"))
        aggregated_response += "{"

    final_message = None
    model_kwargs = model_kwargs or dict()
    if system_prompt:
        model_kwargs["system"] = system_prompt

    max_tokens = max_tokens or DEFAULT_MAX_TOKENS_ANTHROPIC
    if deepthought and model_name.startswith("claude-3-7"):
        model_kwargs["thinking"] = {"type": "enabled", "budget_tokens": MAX_REASONING_TOKENS_ANTHROPIC}
        max_tokens += MAX_REASONING_TOKENS_ANTHROPIC
        # Temperature control not supported when using extended thinking
        temperature = 1.0

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
        final_message = stream.get_final_message()

    # Calculate cost of chat
    input_tokens = final_message.usage.input_tokens
    output_tokens = final_message.usage.output_tokens
    cache_read_tokens = final_message.usage.cache_read_input_tokens
    cache_write_tokens = final_message.usage.cache_creation_input_tokens
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, cache_read_tokens, cache_write_tokens, usage=tracer.get("usage")
    )

    # Save conversation trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, aggregated_response, tracer)

    return aggregated_response


@retry(
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
async def anthropic_chat_completion_with_backoff(
    messages: list[ChatMessage],
    model_name,
    temperature,
    api_key,
    api_base_url,
    system_prompt: str,
    max_prompt_size=None,
    deepthought=False,
    model_kwargs=None,
    tracer={},
):
    try:
        client = anthropic_async_clients.get(api_key)
        if not client:
            client = get_anthropic_async_client(api_key, api_base_url)
            anthropic_async_clients[api_key] = client

        model_kwargs = model_kwargs or dict()
        max_tokens = DEFAULT_MAX_TOKENS_ANTHROPIC
        if deepthought and model_name.startswith("claude-3-7"):
            model_kwargs["thinking"] = {"type": "enabled", "budget_tokens": MAX_REASONING_TOKENS_ANTHROPIC}
            max_tokens += MAX_REASONING_TOKENS_ANTHROPIC
            # Temperature control not supported when using extended thinking
            temperature = 1.0

        formatted_messages, system_prompt = format_messages_for_anthropic(messages, system_prompt)

        aggregated_response = ""
        final_message = None
        start_time = perf_counter()
        async with client.messages.stream(
            messages=formatted_messages,
            model=model_name,  # type: ignore
            temperature=temperature,
            system=system_prompt,
            timeout=20,
            max_tokens=max_tokens,
            **model_kwargs,
        ) as stream:
            async for text in stream.text_stream:
                # Log the time taken to start response
                if aggregated_response == "":
                    logger.info(f"First response took: {perf_counter() - start_time:.3f} seconds")
                # Handle streamed response chunk
                aggregated_response += text
                yield text
            final_message = await stream.get_final_message()

        # Log the time taken to stream the entire response
        logger.info(f"Chat streaming took: {perf_counter() - start_time:.3f} seconds")

        # Calculate cost of chat
        input_tokens = final_message.usage.input_tokens
        output_tokens = final_message.usage.output_tokens
        cache_read_tokens = final_message.usage.cache_read_input_tokens
        cache_write_tokens = final_message.usage.cache_creation_input_tokens
        tracer["usage"] = get_chat_usage_metrics(
            model_name, input_tokens, output_tokens, cache_read_tokens, cache_write_tokens, usage=tracer.get("usage")
        )

        # Save conversation trace
        tracer["chat_model"] = model_name
        tracer["temperature"] = temperature
        if is_promptrace_enabled():
            commit_conversation_trace(messages, aggregated_response, tracer)
    except Exception as e:
        logger.error(f"Error in anthropic_chat_completion_with_backoff stream: {e}", exc_info=True)


def format_messages_for_anthropic(messages: list[ChatMessage], system_prompt: str = None):
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
                    image_data = part["image_url"]["url"]
                    if image_data.startswith("http"):
                        image = get_image_from_url(image_data, type="b64")
                    else:
                        image = get_image_from_base64(image_data, type="b64")
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

        if is_none_or_empty(message.content):
            logger.error(f"Drop message with empty content as not supported:\n{message}")
            messages.remove(message)
            continue

    formatted_messages: List[anthropic.types.MessageParam] = [
        anthropic.types.MessageParam(role=message.role, content=message.content) for message in messages
    ]

    return formatted_messages, system_prompt
