import logging
import os
from copy import deepcopy
from functools import partial
from time import perf_counter
from typing import AsyncGenerator, Dict, Generator, List, Literal, Optional, Union
from urllib.parse import urlparse

import httpx
import openai
from langchain_core.messages.chat import ChatMessage
from openai.lib.streaming.chat import (
    ChatCompletionStream,
    ChatCompletionStreamEvent,
    ContentDeltaEvent,
)
from openai.types.chat.chat_completion import ChatCompletion
from openai.types.chat.chat_completion_chunk import (
    ChatCompletionChunk,
    Choice,
    ChoiceDelta,
)
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import (
    JsonSupport,
    ResponseWithThought,
    commit_conversation_trace,
)
from khoj.utils.helpers import (
    convert_image_data_uri,
    get_chat_usage_metrics,
    get_openai_async_client,
    get_openai_client,
    is_none_or_empty,
    is_promptrace_enabled,
)

logger = logging.getLogger(__name__)

openai_clients: Dict[str, openai.OpenAI] = {}
openai_async_clients: Dict[str, openai.AsyncOpenAI] = {}


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.APIError)
        | retry_if_exception_type(openai._exceptions.APIConnectionError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.APIStatusError)
        | retry_if_exception_type(ValueError)
    ),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def completion_with_backoff(
    messages: List[ChatMessage],
    model_name: str,
    temperature=0.8,
    openai_api_key=None,
    api_base_url=None,
    deepthought: bool = False,
    model_kwargs: dict = {},
    tracer: dict = {},
) -> str:
    client_key = f"{openai_api_key}--{api_base_url}"
    client = openai_clients.get(client_key)
    if not client:
        client = get_openai_client(openai_api_key, api_base_url)
        openai_clients[client_key] = client

    stream = not is_non_streaming_model(model_name, api_base_url)
    stream_processor = default_stream_processor
    if stream:
        model_kwargs["stream_options"] = {"include_usage": True}

    formatted_messages = format_message_for_api(messages, api_base_url)

    # Tune reasoning models arguments
    if is_openai_reasoning_model(model_name, api_base_url):
        temperature = 1
        reasoning_effort = "medium" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
    elif is_twitter_reasoning_model(model_name, api_base_url):
        reasoning_effort = "high" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
    elif model_name.startswith("deepseek-reasoner"):
        # Two successive messages cannot be from the same role. Should merge any back-to-back messages from the same role.
        # The first message should always be a user message (except system message).
        updated_messages: List[dict] = []
        for i, message in enumerate(formatted_messages):
            if i > 0 and message["role"] == formatted_messages[i - 1]["role"]:
                updated_messages[-1]["content"] += " " + message["content"]
            elif i == 1 and formatted_messages[i - 1]["role"] == "system" and message["role"] == "assistant":
                updated_messages[-1]["content"] += " " + message["content"]
            else:
                updated_messages.append(message)
        formatted_messages = updated_messages
    elif is_qwen_reasoning_model(model_name, api_base_url):
        stream_processor = partial(in_stream_thought_processor, thought_tag="think")
        # Reasoning is enabled by default. Disable when deepthought is False.
        # See https://qwenlm.github.io/blog/qwen3/#advanced-usages
        if not deepthought:
            add_qwen_no_think_tag(formatted_messages)

    read_timeout = 300 if is_local_api(api_base_url) else 60
    if os.getenv("KHOJ_LLM_SEED"):
        model_kwargs["seed"] = int(os.getenv("KHOJ_LLM_SEED"))

    aggregated_response = ""
    if stream:
        with client.beta.chat.completions.stream(
            messages=formatted_messages,  # type: ignore
            model=model_name,
            temperature=temperature,
            timeout=httpx.Timeout(30, read=read_timeout),
            **model_kwargs,
        ) as chat:
            for chunk in stream_processor(chat):
                if chunk.type == "content.delta":
                    aggregated_response += chunk.delta
                elif chunk.type == "thought.delta":
                    pass
    else:
        # Non-streaming chat completion
        chunk = client.beta.chat.completions.parse(
            messages=formatted_messages,  # type: ignore
            model=model_name,
            temperature=temperature,
            timeout=httpx.Timeout(30, read=read_timeout),
            **model_kwargs,
        )
        aggregated_response = chunk.choices[0].message.content

    # Calculate cost of chat
    input_tokens = chunk.usage.prompt_tokens if hasattr(chunk, "usage") and chunk.usage else 0
    output_tokens = chunk.usage.completion_tokens if hasattr(chunk, "usage") and chunk.usage else 0
    cost = (
        chunk.usage.model_extra.get("estimated_cost", 0) if hasattr(chunk, "usage") and chunk.usage else 0
    )  # Estimated costs returned by DeepInfra API

    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, usage=tracer.get("usage"), cost=cost
    )

    # Validate the response. If empty, raise an error to retry.
    if is_none_or_empty(aggregated_response):
        logger.warning(f"No response by {model_name}\nLast Message by {messages[-1].role}: {messages[-1].content}.")
        raise ValueError(f"Empty or no response by {model_name} over API. Retry if needed.")

    # Save conversation trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, aggregated_response, tracer)

    return aggregated_response


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.APIError)
        | retry_if_exception_type(openai._exceptions.APIConnectionError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.APIStatusError)
        | retry_if_exception_type(ValueError)
    ),
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.WARNING),
    reraise=False,
)
async def chat_completion_with_backoff(
    messages: list[ChatMessage],
    model_name: str,
    temperature,
    openai_api_key=None,
    api_base_url=None,
    deepthought=False,
    model_kwargs: dict = {},
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    client_key = f"{openai_api_key}--{api_base_url}"
    client = openai_async_clients.get(client_key)
    if not client:
        client = get_openai_async_client(openai_api_key, api_base_url)
        openai_async_clients[client_key] = client

    stream = not is_non_streaming_model(model_name, api_base_url)
    stream_processor = adefault_stream_processor
    if stream:
        model_kwargs["stream_options"] = {"include_usage": True}

    formatted_messages = format_message_for_api(messages, api_base_url)

    # Configure thinking for openai reasoning models
    if is_openai_reasoning_model(model_name, api_base_url):
        temperature = 1
        reasoning_effort = "medium" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
        model_kwargs.pop("stop", None)  # Remove unsupported stop param for reasoning models

        # Get the first system message and add the string `Formatting re-enabled` to it.
        # See https://platform.openai.com/docs/guides/reasoning-best-practices
        if len(formatted_messages) > 0:
            system_messages = [
                (i, message) for i, message in enumerate(formatted_messages) if message["role"] == "system"
            ]
            if len(system_messages) > 0:
                first_system_message_index, first_system_message = system_messages[0]
                first_system_message_content = first_system_message["content"]
                formatted_messages[first_system_message_index][
                    "content"
                ] = f"{first_system_message_content}\nFormatting re-enabled"
    elif is_twitter_reasoning_model(model_name, api_base_url):
        stream_processor = adeepseek_stream_processor
        reasoning_effort = "high" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
    elif model_name.startswith("deepseek-reasoner"):
        stream_processor = adeepseek_stream_processor
        # Two successive messages cannot be from the same role. Should merge any back-to-back messages from the same role.
        # The first message should always be a user message (except system message).
        updated_messages: List[dict] = []
        for i, message in enumerate(formatted_messages):
            if i > 0 and message["role"] == formatted_messages[i - 1]["role"]:
                updated_messages[-1]["content"] += " " + message["content"]
            elif i == 1 and formatted_messages[i - 1]["role"] == "system" and message["role"] == "assistant":
                updated_messages[-1]["content"] += " " + message["content"]
            else:
                updated_messages.append(message)
        formatted_messages = updated_messages
    elif is_qwen_reasoning_model(model_name, api_base_url):
        stream_processor = partial(ain_stream_thought_processor, thought_tag="think")
        # Reasoning is enabled by default. Disable when deepthought is False.
        # See https://qwenlm.github.io/blog/qwen3/#advanced-usages
        if not deepthought:
            add_qwen_no_think_tag(formatted_messages)

    read_timeout = 300 if is_local_api(api_base_url) else 60
    if os.getenv("KHOJ_LLM_SEED"):
        model_kwargs["seed"] = int(os.getenv("KHOJ_LLM_SEED"))

    aggregated_response = ""
    final_chunk = None
    response_started = False
    start_time = perf_counter()
    response: openai.AsyncStream[ChatCompletionChunk] | ChatCompletion = await client.chat.completions.create(
        messages=formatted_messages,  # type: ignore
        model=model_name,
        stream=stream,
        temperature=temperature,
        timeout=httpx.Timeout(30, read=read_timeout),
        **model_kwargs,
    )
    if not stream:
        # If not streaming, we can return the response directly
        if len(response.choices) == 0 or not response.choices[0].message:
            raise ValueError("No response by model.")
        aggregated_response = response.choices[0].message.content
        final_chunk = response
        yield ResponseWithThought(response=aggregated_response)
    else:
        async for chunk in stream_processor(response):
            # Log the time taken to start response
            if not response_started:
                response_started = True
                logger.info(f"First response took: {perf_counter() - start_time:.3f} seconds")
            # Keep track of the last chunk for usage data
            final_chunk = chunk
            # Skip empty chunks
            if len(chunk.choices) == 0:
                continue
            # Handle streamed response chunk
            response_chunk: ResponseWithThought = None
            response_delta = chunk.choices[0].delta
            if response_delta.content:
                response_chunk = ResponseWithThought(response=response_delta.content)
                aggregated_response += response_chunk.response
            elif response_delta.thought:
                response_chunk = ResponseWithThought(thought=response_delta.thought)
            if response_chunk:
                yield response_chunk

    # Calculate cost of chat after stream finishes
    input_tokens, output_tokens, cost = 0, 0, 0
    if final_chunk and hasattr(final_chunk, "usage") and final_chunk.usage:
        input_tokens = final_chunk.usage.prompt_tokens
        output_tokens = final_chunk.usage.completion_tokens
        # Estimated costs returned by DeepInfra API
        if final_chunk.usage.model_extra and "estimated_cost" in final_chunk.usage.model_extra:
            cost = final_chunk.usage.model_extra.get("estimated_cost", 0)
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, usage=tracer.get("usage"), cost=cost
    )

    # Validate the response. If empty, raise an error to retry.
    if is_none_or_empty(aggregated_response):
        logger.warning(f"No response by {model_name}\nLast Message by {messages[-1].role}: {messages[-1].content}.")
        raise ValueError(f"Empty or no response by {model_name} over API. Retry if needed.")

    # Log the time taken to stream the entire response
    logger.info(f"Chat streaming took: {perf_counter() - start_time:.3f} seconds")

    # Save conversation trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, aggregated_response, tracer)


def get_openai_api_json_support(model_name: str, api_base_url: str = None) -> JsonSupport:
    if model_name.startswith("deepseek-reasoner"):
        return JsonSupport.NONE
    if api_base_url:
        host = urlparse(api_base_url).hostname
        if host and host.endswith(".ai.azure.com"):
            return JsonSupport.OBJECT
        if host == "api.deepinfra.com":
            return JsonSupport.OBJECT
    return JsonSupport.SCHEMA


def format_message_for_api(messages: List[ChatMessage], api_base_url: str) -> List[dict]:
    """
    Format messages to send to chat model served over OpenAI (compatible) API.
    """
    formatted_messages = []
    for message in deepcopy(messages):
        if isinstance(message.content, list) and not is_openai_api(api_base_url):
            assistant_texts = []
            has_images = False
            for idx, part in enumerate(message.content):
                # Convert images to PNG format if message to be sent to non OpenAI API
                if part.get("type") == "image_url":
                    has_images = True
                    part["image_url"]["url"] = convert_image_data_uri(part["image_url"]["url"], target_format="png")
                # Deepinfra API does not support text content list in assistant messages
                # So we merge text content list into a single text string
                if (
                    part.get("type") == "text"
                    and message.role == "assistant"
                    and api_base_url.startswith("https://api.deepinfra.com/v1")
                ):
                    assistant_texts += [part["text"]]
                    message.content.pop(idx)
            if assistant_texts:
                assistant_texts_str = "\n\n".join(assistant_texts)
                if has_images:
                    message.content += [{"type": "text", "text": assistant_texts_str}]
                else:
                    message.content = assistant_texts_str
        formatted_messages.append({"role": message.role, "content": message.content})

    return formatted_messages


def is_openai_api(api_base_url: str = None) -> bool:
    """
    Check if the model is served over the official OpenAI API
    """
    return api_base_url is None or api_base_url.startswith("https://api.openai.com/v1")


def is_openai_reasoning_model(model_name: str, api_base_url: str = None) -> bool:
    """
    Check if the model is an OpenAI reasoning model
    """
    return model_name.startswith("o") and is_openai_api(api_base_url)


def is_non_streaming_model(model_name: str, api_base_url: str = None) -> bool:
    """
    Check if model response should not be streamed.
    """
    # Some OpenAI models requires biometrics to stream. Avoid streaming their responses.
    return model_name in ["o3", "o3-pro"] and is_openai_api(api_base_url)


def is_twitter_reasoning_model(model_name: str, api_base_url: str = None) -> bool:
    """
    Check if the model is a Twitter reasoning model
    """
    return (
        model_name.startswith("grok-3-mini")
        and api_base_url is not None
        and api_base_url.startswith("https://api.x.ai/v1")
    )


def is_qwen_reasoning_model(model_name: str, api_base_url: str = None) -> bool:
    """
    Check if the model is a Qwen reasoning model
    """
    return "qwen3" in model_name.lower() and api_base_url is not None


def is_local_api(api_base_url: str) -> bool:
    """
    Check if the API base URL is a local API
    """
    if not api_base_url:
        return False

    host = urlparse(api_base_url).hostname
    return host == "localhost" or host == "127.0.0.1"


class ThoughtDeltaEvent(ContentDeltaEvent):
    """
    Chat completion chunk with thoughts, reasoning support.
    """

    type: Literal["thought.delta"]
    """The thought or reasoning generated by the model."""


ChatCompletionStreamWithThoughtEvent = Union[ChatCompletionStreamEvent, ThoughtDeltaEvent]


class ChoiceDeltaWithThoughts(ChoiceDelta):
    """
    Chat completion chunk with thoughts, reasoning support.
    """

    thought: Optional[str] = None
    """The thought or reasoning generated by the model."""


class ChoiceWithThoughts(Choice):
    delta: ChoiceDeltaWithThoughts


class ChatCompletionWithThoughtsChunk(ChatCompletionChunk):
    choices: List[ChoiceWithThoughts]  # Override the choices type


def default_stream_processor(
    chat_stream: ChatCompletionStream,
) -> Generator[ChatCompletionStreamWithThoughtEvent, None, None]:
    """
    Async generator to cast and return chunks from the standard openai chat completions stream.
    """
    for chunk in chat_stream:
        yield chunk


async def adefault_stream_processor(
    chat_stream: openai.AsyncStream[ChatCompletionChunk],
) -> AsyncGenerator[ChatCompletionWithThoughtsChunk, None]:
    """
    Async generator to cast and return chunks from the standard openai chat completions stream.
    """
    async for chunk in chat_stream:
        yield ChatCompletionWithThoughtsChunk.model_validate(chunk.model_dump())


async def adeepseek_stream_processor(
    chat_stream: openai.AsyncStream[ChatCompletionChunk],
) -> AsyncGenerator[ChatCompletionWithThoughtsChunk, None]:
    """
    Async generator to cast and return chunks from the deepseek chat completions stream.
    """
    async for chunk in chat_stream:
        tchunk = ChatCompletionWithThoughtsChunk.model_validate(chunk.model_dump())
        if (
            len(tchunk.choices) > 0
            and hasattr(tchunk.choices[0].delta, "reasoning_content")
            and tchunk.choices[0].delta.reasoning_content
        ):
            tchunk.choices[0].delta.thought = chunk.choices[0].delta.reasoning_content
        yield tchunk


def in_stream_thought_processor(
    chat_stream: openai.Stream[ChatCompletionChunk], thought_tag="think"
) -> Generator[ChatCompletionStreamWithThoughtEvent, None, None]:
    """
    Generator for chat completion with thought chunks.
    Assumes <thought_tag>...</thought_tag> can only appear once at the start.
    Handles partial tags across streamed chunks.
    """
    start_tag = f"<{thought_tag}>"
    end_tag = f"</{thought_tag}>"
    buf: str = ""
    # Modes and transitions: detect_start > thought (optional) > message
    mode = "detect_start"

    for chunk in default_stream_processor(chat_stream):
        if mode == "message" or chunk.type != "content.delta":
            # Message mode is terminal, so just yield chunks, no processing
            yield chunk
            continue

        buf += chunk.delta

        if mode == "detect_start":
            # Try to determine if we start with thought tag
            if buf.startswith(start_tag):
                # Found start tag, switch mode
                buf = buf[len(start_tag) :]  # Remove start tag
                mode = "thought"
                # Fall through to process the rest of the buffer in 'thought' mode *within this iteration*
            elif len(buf) >= len(start_tag):
                # Buffer is long enough, definitely doesn't start with tag
                chunk.delta = buf
                yield chunk
                mode = "message"
                buf = ""
                continue
            elif start_tag.startswith(buf):
                # Buffer is a prefix of the start tag, need more data
                continue
            else:
                # Buffer doesn't match start tag prefix and is shorter than tag
                chunk.delta = buf
                yield chunk
                mode = "message"
                buf = ""
                continue

        if mode == "thought":
            # Look for the end tag
            idx = buf.find(end_tag)
            if idx != -1:
                # Found end tag. Yield thought content before it.
                if idx > 0 and buf[:idx].strip():
                    chunk.type = "thought.delta"
                    chunk.delta = buf[:idx]
                    yield chunk
                # Process content *after* the tag as message
                buf = buf[idx + len(end_tag) :]
                if buf:
                    chunk.delta = buf
                    yield chunk
                mode = "message"
                buf = ""
                continue
            else:
                # End tag not found yet. Yield thought content, holding back potential partial end tag.
                send_upto = len(buf)
                # Check if buffer ends with a prefix of end_tag
                for i in range(len(end_tag) - 1, 0, -1):
                    if buf.endswith(end_tag[:i]):
                        send_upto = len(buf) - i  # Don't send the partial tag yet
                        break
                if send_upto > 0 and buf[:send_upto].strip():
                    chunk.type = "thought.delta"
                    chunk.delta = buf[:send_upto]
                    yield chunk
                    buf = buf[send_upto:]  # Keep only the partial tag (or empty)
                # Need more data to find the complete end tag
                continue

    # End of stream handling
    if buf:
        if mode == "thought":  # Stream ended before </think> was found
            chunk.type = "thought.delta"
            chunk.delta = buf
            yield chunk
        elif mode == "detect_start":  # Stream ended before start tag could be confirmed/denied
            # If it wasn't a partial start tag, treat as message
            if not start_tag.startswith(buf):
                chunk.delta = buf
                yield chunk
            # else: discard partial <think>
        # If mode == "message", buffer should be empty due to logic above, but yield just in case
        elif mode == "message":
            chunk.delta = buf
            yield chunk


async def ain_stream_thought_processor(
    chat_stream: openai.AsyncStream[ChatCompletionChunk], thought_tag="think"
) -> AsyncGenerator[ChatCompletionWithThoughtsChunk, None]:
    """
    Async generator for chat completion with thought chunks.
    Assumes <thought_tag>...</thought_tag> can only appear once at the start.
    Handles partial tags across streamed chunks.
    """
    start_tag = f"<{thought_tag}>"
    end_tag = f"</{thought_tag}>"
    buf: str = ""
    # Modes and transitions: detect_start > thought (optional) > message
    mode = "detect_start"

    async for chunk in adefault_stream_processor(chat_stream):
        if len(chunk.choices) == 0:
            continue
        if mode == "message":
            # Message mode is terminal, so just yield chunks, no processing
            yield chunk
            continue

        buf += chunk.choices[0].delta.content

        if mode == "detect_start":
            # Try to determine if we start with thought tag
            if buf.startswith(start_tag):
                # Found start tag, switch mode
                buf = buf[len(start_tag) :]  # Remove start tag
                mode = "thought"
                # Fall through to process the rest of the buffer in 'thought' mode *within this iteration*
            elif len(buf) >= len(start_tag):
                # Buffer is long enough, definitely doesn't start with tag
                chunk.choices[0].delta.content = buf
                yield chunk
                mode = "message"
                buf = ""
                continue
            elif start_tag.startswith(buf):
                # Buffer is a prefix of the start tag, need more data
                continue
            else:
                # Buffer doesn't match start tag prefix and is shorter than tag
                chunk.choices[0].delta.content = buf
                yield chunk
                mode = "message"
                buf = ""
                continue

        if mode == "thought":
            # Look for the end tag
            idx = buf.find(end_tag)
            if idx != -1:
                # Found end tag. Yield thought content before it.
                if idx > 0 and buf[:idx].strip():
                    chunk.choices[0].delta.thought = buf[:idx]
                    chunk.choices[0].delta.content = ""
                    yield chunk
                # Process content *after* the tag as message
                buf = buf[idx + len(end_tag) :]
                if buf:
                    chunk.choices[0].delta.content = buf
                    yield chunk
                mode = "message"
                buf = ""
                continue
            else:
                # End tag not found yet. Yield thought content, holding back potential partial end tag.
                send_upto = len(buf)
                # Check if buffer ends with a prefix of end_tag
                for i in range(len(end_tag) - 1, 0, -1):
                    if buf.endswith(end_tag[:i]):
                        send_upto = len(buf) - i  # Don't send the partial tag yet
                        break
                if send_upto > 0 and buf[:send_upto].strip():
                    chunk.choices[0].delta.thought = buf[:send_upto]
                    chunk.choices[0].delta.content = ""
                    yield chunk
                    buf = buf[send_upto:]  # Keep only the partial tag (or empty)
                # Need more data to find the complete end tag
                continue

    # End of stream handling
    if buf:
        if mode == "thought":  # Stream ended before </think> was found
            chunk.choices[0].delta.thought = buf
            chunk.choices[0].delta.content = ""
            yield chunk
        elif mode == "detect_start":  # Stream ended before start tag could be confirmed/denied
            # If it wasn't a partial start tag, treat as message
            if not start_tag.startswith(buf):
                chunk.choices[0].delta.content = buf
                yield chunk
            # else: discard partial <think>
        # If mode == "message", buffer should be empty due to logic above, but yield just in case
        elif mode == "message":
            chunk.choices[0].delta.content = buf
            yield chunk


def add_qwen_no_think_tag(formatted_messages: List[dict]) -> None:
    """
    Add /no_think tag to the last message content if it is a user message.
    This is used to disable reasoning in Qwen models when deepthought is False.
    """
    if len(formatted_messages) > 0 and formatted_messages[-1]["role"] == "user":
        last_message = formatted_messages[-1]
        if isinstance(last_message["content"], str):
            # Append /no_think to the last message content
            formatted_messages[-1]["content"] = last_message["content"] + " /no_think"
        elif isinstance(last_message["content"], list) and len(last_message["content"]) > 0:
            # Append /no_think to the last content part
            if isinstance(last_message["content"][-1], str):
                last_message["content"][-1] = last_message["content"][-1] + " /no_think"
            else:
                # Find last content part of type text and append /no_think to "text" part
                for content_part in reversed(last_message["content"]):
                    if isinstance(content_part, dict) and content_part.get("type") == "text":
                        content_part["text"] += " /no_think"
                        break
