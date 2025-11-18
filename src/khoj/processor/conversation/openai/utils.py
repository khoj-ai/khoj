import hashlib
import json
import logging
import os
from copy import deepcopy
from time import perf_counter
from typing import AsyncGenerator, Dict, Generator, List, Literal, Optional, Union
from urllib.parse import urlparse

import httpx
import openai
from langchain_core.messages.chat import ChatMessage
from openai.lib._pydantic import _ensure_strict_json_schema
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
from openai.types.responses import Response as OpenAIResponse
from openai.types.responses import ResponseFunctionToolCall, ResponseReasoningItem
from pydantic import BaseModel
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import (
    ResponseWithThought,
    StructuredOutputSupport,
    ToolCall,
    commit_conversation_trace,
)
from khoj.utils.helpers import (
    ToolDefinition,
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


def _extract_text_for_instructions(content: Union[str, List, Dict, None]) -> str:
    """Extract plain text from a message content suitable for Responses API instructions."""
    if content is None:
        return ""
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        texts: List[str] = []
        for part in content:
            if isinstance(part, dict) and part.get("type") == "input_text" and part.get("text"):
                texts.append(str(part.get("text")))
        return "\n\n".join(texts)
    if isinstance(content, dict):
        # If a single part dict was passed
        if content.get("type") == "input_text" and content.get("text"):
            return str(content.get("text"))
    # Fallback to string conversion
    return str(content)


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.InternalServerError)
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
    temperature=0.6,
    openai_api_key=None,
    api_base_url: str | None = None,
    deepthought: bool = False,
    model_kwargs: dict = {},
    tracer: dict = {},
) -> ResponseWithThought:
    client_key = f"{openai_api_key}--{api_base_url}"
    client = openai_clients.get(client_key)
    if not client:
        client = get_openai_client(openai_api_key, api_base_url)
        openai_clients[client_key] = client

    stream = not is_non_streaming_model(model_name, api_base_url)
    stream_processor = default_stream_processor
    if stream:
        model_kwargs["stream_options"] = {"include_usage": True}

    model_kwargs["temperature"] = temperature
    model_kwargs["top_p"] = model_kwargs.get("top_p", 0.95)

    formatted_messages = format_message_for_api(messages, model_name, api_base_url)

    # Tune reasoning models arguments
    if is_openai_reasoning_model(model_name, api_base_url):
        model_kwargs["temperature"] = 1
        reasoning_effort = "medium" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
        # Remove unsupported params for reasoning models
        model_kwargs.pop("top_p", None)
        model_kwargs.pop("stop", None)
    elif is_twitter_reasoning_model(model_name, api_base_url):
        model_kwargs.pop("temperature", None)
        reasoning_effort = "high" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
        if model_name.startswith("grok-4"):
            # Grok-4 models do not support reasoning_effort parameter
            model_kwargs.pop("reasoning_effort", None)
    elif model_name.startswith("deepseek-reasoner") or model_name.startswith("deepseek-chat"):
        stream_processor = in_stream_thought_processor
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
    elif is_instream_thinking_model(model_name):
        stream_processor = in_stream_thought_processor
    elif is_qwen_style_reasoning_model(model_name, api_base_url):
        stream_processor = in_stream_thought_processor
        # Reasoning is enabled by default. Disable when deepthought is False.
        # See https://qwenlm.github.io/blog/qwen3/#advanced-usages
        if not deepthought:
            add_qwen_no_think_tag(formatted_messages)
    elif is_groq_api(api_base_url):
        model_kwargs["service_tier"] = "auto"

    read_timeout = 300 if is_local_api(api_base_url) else 60
    if os.getenv("KHOJ_LLM_SEED"):
        model_kwargs["seed"] = int(os.getenv("KHOJ_LLM_SEED"))

    tool_ids = []
    tool_calls: list[ToolCall] = []
    thoughts = ""
    aggregated_response = ""
    chunk = None
    if stream:
        with client.beta.chat.completions.stream(
            messages=formatted_messages,  # type: ignore
            model=model_name,
            timeout=httpx.Timeout(30, read=read_timeout),
            **model_kwargs,
        ) as chat:
            for chunk in stream_processor(chat):
                if chunk.type == "content.delta":
                    aggregated_response += chunk.delta
                elif chunk.type == "thought.delta":
                    thoughts += chunk.delta
                elif (
                    chunk.type == "chunk"
                    and chunk.chunk.choices
                    and hasattr(chunk.chunk.choices[0].delta, "reasoning_content")
                    and chunk.chunk.choices[0].delta.reasoning_content
                ):
                    thoughts += chunk.chunk.choices[0].delta.reasoning_content
                elif (
                    chunk.type == "chunk"
                    and chunk.chunk.choices
                    and hasattr(chunk.chunk.choices[0].delta, "reasoning")
                    and chunk.chunk.choices[0].delta.reasoning
                ):
                    thoughts += chunk.chunk.choices[0].delta.reasoning
                elif chunk.type == "chunk" and chunk.chunk.choices and chunk.chunk.choices[0].delta.tool_calls:
                    tool_ids += [tool_call.id for tool_call in chunk.chunk.choices[0].delta.tool_calls]
                elif chunk.type == "tool_calls.function.arguments.done":
                    tool_calls += [ToolCall(name=chunk.name, args=json.loads(chunk.arguments), id=None)]
        if tool_calls:
            # If there are tool calls, aggregate thoughts and responses into thoughts
            if thoughts and aggregated_response:
                # wrap each line of thought in italics
                thoughts = "\n".join([f"*{line.strip()}*" for line in thoughts.splitlines() if line.strip()])
                thoughts = f"{thoughts}\n\n{aggregated_response}"
            else:
                thoughts = thoughts or aggregated_response
            # Json dump tool calls into aggregated response
            tool_calls = [
                ToolCall(name=chunk.name, args=chunk.args, id=tool_id) for chunk, tool_id in zip(tool_calls, tool_ids)
            ]
            aggregated_response = json.dumps([tool_call.__dict__ for tool_call in tool_calls])
    else:
        # Non-streaming chat completion
        chunk = client.beta.chat.completions.parse(
            messages=formatted_messages,  # type: ignore
            model=model_name,
            timeout=httpx.Timeout(30, read=read_timeout),
            **model_kwargs,
        )
        aggregated_response = chunk.choices[0].message.content
        if hasattr(chunk.choices[0].message, "reasoning_content"):
            thoughts = chunk.choices[0].message.reasoning_content
        else:
            thoughts = chunk.choices[0].message.model_extra.get("reasoning_content", "")
        raw_tool_calls = chunk.choices[0].message.tool_calls
        if raw_tool_calls:
            tool_calls = [
                ToolCall(name=tool.function.name, args=tool.function.parsed_arguments, id=tool.id)
                for tool in raw_tool_calls
            ]
            # If there are tool calls, aggregate thoughts and responses into thoughts
            if thoughts and aggregated_response:
                # wrap each line of thought in italics
                thoughts = "\n".join([f"*{line.strip()}*" for line in thoughts.splitlines() if line.strip()])
                thoughts = f"{thoughts}\n\n{aggregated_response}"
            else:
                thoughts = thoughts or aggregated_response
            # Json dump tool calls into aggregated response
            aggregated_response = json.dumps([tool_call.__dict__ for tool_call in tool_calls])

    # Align chunk definition with non-streaming mode for post stream completion usage
    if hasattr(chunk, "chunk"):
        chunk = chunk.chunk

    # Calculate cost of chat
    input_tokens, output_tokens, cache_read_tokens, cost = 0, 0, 0, 0
    if hasattr(chunk, "usage") and chunk.usage:
        input_tokens = chunk.usage.prompt_tokens
        output_tokens = chunk.usage.completion_tokens
        if hasattr(chunk.usage, "prompt_tokens_details") and chunk.usage.prompt_tokens_details:
            cache_read_tokens = chunk.usage.prompt_tokens_details.cached_tokens
        if hasattr(chunk.usage, "completion_tokens_details") and chunk.usage.completion_tokens_details:
            output_tokens += chunk.usage.completion_tokens_details.reasoning_tokens
        cost = chunk.usage.model_extra.get("estimated_cost", 0)  # Estimated costs returned by DeepInfra API

    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, cache_read_tokens, usage=tracer.get("usage"), cost=cost
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

    return ResponseWithThought(text=aggregated_response, thought=thoughts)


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.InternalServerError)
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
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    client_key = f"{openai_api_key}--{api_base_url}"
    client = openai_async_clients.get(client_key)
    if not client:
        client = get_openai_async_client(openai_api_key, api_base_url)
        openai_async_clients[client_key] = client

    model_kwargs: dict = {}
    stream = not is_non_streaming_model(model_name, api_base_url)
    stream_processor = astream_thought_processor
    if stream:
        model_kwargs["stream_options"] = {"include_usage": True}
    else:
        model_kwargs.pop("stream_options", None)

    model_kwargs["top_p"] = model_kwargs.get("top_p", 0.95)

    formatted_messages = format_message_for_api(messages, model_name, api_base_url)

    # Configure thinking for openai reasoning models
    if is_openai_reasoning_model(model_name, api_base_url):
        temperature = 1
        reasoning_effort = "medium" if deepthought else "low"
        model_kwargs["reasoning_effort"] = reasoning_effort
        # Remove unsupported params for reasoning models
        model_kwargs.pop("top_p", None)
        model_kwargs.pop("stop", None)
    elif is_twitter_reasoning_model(model_name, api_base_url):
        reasoning_effort = "high" if deepthought else "low"
        # Grok-4 models do not support reasoning_effort parameter
        if not model_name.startswith("grok-4"):
            model_kwargs["reasoning_effort"] = reasoning_effort
    elif (
        model_name.startswith("deepseek-chat")
        or model_name.startswith("deepseek-reasoner")
        or "deepseek-r1" in model_name.lower()
    ):
        # Official Deepseek models and some inference APIs like vLLM return structured thinking output.
        # Others like DeepInfra return it in response stream.
        # Using the instream thought processor handles both cases, structured thoughts and in response thoughts.
        stream_processor = ain_stream_thought_processor
        # Two successive messages cannot be from the same role. Should merge any back-to-back messages from the same role.
        # The first message should always be a user message (except system message).
        updated_messages: List[dict] = []
        for i, message in enumerate(formatted_messages):
            if i > 0 and message["role"] == formatted_messages[i - 1]["role"]:
                updated_messages[-1]["content"] += (
                    " " + message["content"] if isinstance(message["content"], str) else message["content"]
                )
            elif i == 1 and formatted_messages[i - 1]["role"] == "system" and message["role"] == "assistant":
                updated_messages[-1]["content"] += (
                    " " + message["content"] if isinstance(message["content"], str) else message["content"]
                )
            else:
                updated_messages.append(message)
        formatted_messages = updated_messages
    elif is_instream_thinking_model(model_name):
        stream_processor = ain_stream_thought_processor
    elif is_qwen_style_reasoning_model(model_name, api_base_url):
        stream_processor = ain_stream_thought_processor
        # Reasoning is enabled by default. Disable when deepthought is False.
        # See https://qwenlm.github.io/blog/qwen3/#advanced-usages
        if not deepthought:
            add_qwen_no_think_tag(formatted_messages)
    elif is_groq_api(api_base_url):
        model_kwargs["service_tier"] = "auto"

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
        yield ResponseWithThought(text=aggregated_response)
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
                response_chunk = ResponseWithThought(text=response_delta.content)
                aggregated_response += response_chunk.text
            elif response_delta.thought:
                response_chunk = ResponseWithThought(thought=response_delta.thought)
            if response_chunk:
                yield response_chunk

    # Calculate cost of chat after stream finishes
    input_tokens, output_tokens, cache_read_tokens, cost = 0, 0, 0, 0
    if final_chunk and hasattr(final_chunk, "usage") and final_chunk.usage:
        input_tokens = final_chunk.usage.prompt_tokens
        output_tokens = final_chunk.usage.completion_tokens
        if hasattr(final_chunk.usage, "prompt_tokens_details") and final_chunk.usage.prompt_tokens_details:
            cache_read_tokens = final_chunk.usage.prompt_tokens_details.cached_tokens
        if hasattr(final_chunk.usage, "completion_tokens_details") and final_chunk.usage.completion_tokens_details:
            output_tokens += final_chunk.usage.completion_tokens_details.reasoning_tokens
        # Estimated costs returned by DeepInfra API
        if final_chunk.usage.model_extra and "estimated_cost" in final_chunk.usage.model_extra:
            cost = final_chunk.usage.model_extra.get("estimated_cost", 0)
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, cache_read_tokens, usage=tracer.get("usage"), cost=cost
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


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.InternalServerError)
        | retry_if_exception_type(ValueError)
    ),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def responses_completion_with_backoff(
    messages: List[ChatMessage],
    model_name: str,
    temperature=0.6,
    openai_api_key=None,
    api_base_url=None,
    deepthought: bool = False,
    model_kwargs: dict = {},
    tracer: dict = {},
) -> ResponseWithThought:
    """
    Synchronous helper using the OpenAI Responses API in streaming mode under the hood.
    Aggregates streamed deltas and returns a ResponseWithThought.
    """
    client_key = f"{openai_api_key}--{api_base_url}"
    client = openai_clients.get(client_key)
    if not client:
        client = get_openai_client(openai_api_key, api_base_url)
        openai_clients[client_key] = client

    formatted_messages = format_message_for_api(messages, model_name, api_base_url)
    # Move the first system message to Responses API instructions
    instructions: Optional[str] = None
    if formatted_messages and formatted_messages[0].get("role") == "system":
        instructions = _extract_text_for_instructions(formatted_messages[0].get("content")) or None
        formatted_messages = formatted_messages[1:]

    model_kwargs = deepcopy(model_kwargs)
    model_kwargs["top_p"] = model_kwargs.get("top_p", 0.95)

    # Use prompt cache key to increase probability of cache hits
    if instructions:
        model_kwargs["prompt_cache_key"] = f"{hashlib.md5(instructions[:500].encode()).hexdigest()}"

    # Configure thinking for openai reasoning models
    if is_openai_reasoning_model(model_name, api_base_url):
        temperature = 1
        reasoning_effort = "medium" if deepthought else "low"
        model_kwargs["reasoning"] = {"effort": reasoning_effort}
        if is_openai_api(api_base_url):
            model_kwargs["reasoning"]["summary"] = "auto"
            model_kwargs["include"] = ["reasoning.encrypted_content"]
        # Remove unsupported params for reasoning models
        model_kwargs.pop("top_p", None)
        model_kwargs.pop("stop", None)

    read_timeout = 300 if is_local_api(api_base_url) else 60

    # Stream and aggregate
    model_response: OpenAIResponse = client.responses.create(
        input=formatted_messages,
        instructions=instructions,
        model=model_name,
        temperature=temperature,
        timeout=httpx.Timeout(30, read=read_timeout),  # type: ignore
        store=False,
        **model_kwargs,
    )
    if not model_response or not isinstance(model_response, OpenAIResponse) or not model_response.output:
        raise ValueError(f"Empty response returned by {model_name}.")

    raw_content = [item.model_dump() for item in model_response.output]
    aggregated_text = model_response.output_text
    thoughts = ""
    tool_calls: List[ToolCall] = []
    for item in model_response.output:
        if isinstance(item, ResponseFunctionToolCall):
            tool_calls.append(ToolCall(name=item.name, args=json.loads(item.arguments), id=item.call_id))
        elif isinstance(item, ResponseReasoningItem):
            thoughts = "\n\n".join([summary.text for summary in item.summary])

    if tool_calls:
        if thoughts and aggregated_text:
            # If there are tool calls, aggregate thoughts and responses into thoughts
            thoughts = "\n".join([f"*{line.strip()}*" for line in thoughts.splitlines() if line.strip()])
            thoughts = f"{thoughts}\n\n{aggregated_text}"
        else:
            thoughts = thoughts or aggregated_text
        # Json dump tool calls into aggregated response
        aggregated_text = json.dumps([tool_call.__dict__ for tool_call in tool_calls])

    # Usage/cost tracking
    input_tokens = model_response.usage.input_tokens if model_response and model_response.usage else 0
    output_tokens = model_response.usage.output_tokens if model_response and model_response.usage else 0
    cost = 0
    cache_read_tokens = 0
    if model_response and model_response.usage and model_response.usage.input_tokens_details:
        cache_read_tokens = model_response.usage.input_tokens_details.cached_tokens
        input_tokens -= cache_read_tokens
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, cache_read_tokens, usage=tracer.get("usage"), cost=cost
    )

    # Validate final aggregated text (either message or tool-calls JSON)
    if is_none_or_empty(aggregated_text):
        logger.warning(f"No response by {model_name}\nLast Message by {messages[-1].role}: {messages[-1].content}.")
        raise ValueError(f"Empty or no response by {model_name} over Responses API. Retry if needed.")

    # Trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, aggregated_text, tracer)

    return ResponseWithThought(text=aggregated_text, thought=thoughts, raw_content=raw_content)


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.InternalServerError)
        | retry_if_exception_type(ValueError)
    ),
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.WARNING),
    reraise=False,
)
async def responses_chat_completion_with_backoff(
    messages: list[ChatMessage],
    model_name: str,
    temperature,
    openai_api_key=None,
    api_base_url=None,
    deepthought=False,  # Unused; parity with legacy signature
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Async streaming helper using the OpenAI Responses API.
    Yields ResponseWithThought chunks as text/think deltas arrive.
    """
    client_key = f"{openai_api_key}--{api_base_url}"
    client = openai_async_clients.get(client_key)
    if not client:
        client = get_openai_async_client(openai_api_key, api_base_url)
        openai_async_clients[client_key] = client

    formatted_messages = format_message_for_api(messages, model_name, api_base_url)
    # Move the first system message to Responses API instructions
    instructions: Optional[str] = None
    if formatted_messages and formatted_messages[0].get("role") == "system":
        instructions = _extract_text_for_instructions(formatted_messages[0].get("content")) or None
        formatted_messages = formatted_messages[1:]

    model_kwargs: dict = {}
    model_kwargs["top_p"] = model_kwargs.get("top_p", 0.95)
    # Configure thinking for openai reasoning models
    if is_openai_reasoning_model(model_name, api_base_url):
        temperature = 1
        reasoning_effort = "medium" if deepthought else "low"
        model_kwargs["reasoning"] = {"effort": reasoning_effort}
        if is_openai_api(api_base_url):
            model_kwargs["reasoning"]["summary"] = "auto"
            model_kwargs["include"] = ["reasoning.encrypted_content"]
        # Remove unsupported params for reasoning models
        model_kwargs.pop("top_p", None)
        model_kwargs.pop("stop", None)

    read_timeout = 300 if is_local_api(api_base_url) else 60

    aggregated_text = ""
    last_final: Optional[OpenAIResponse] = None
    # Tool call assembly buffers
    tool_calls_args: Dict[str, str] = {}
    tool_calls_name: Dict[str, str] = {}
    tool_call_order: List[str] = []

    async with client.responses.stream(
        input=formatted_messages,
        instructions=instructions,
        model=model_name,
        temperature=temperature,
        timeout=httpx.Timeout(30, read=read_timeout),
        **model_kwargs,
    ) as stream:  # type: ignore
        async for event in stream:  # type: ignore
            et = getattr(event, "type", "")
            if et == "response.output_text.delta":
                delta = getattr(event, "delta", "") or getattr(event, "output_text", "")
                if delta:
                    aggregated_text += delta
                    yield ResponseWithThought(text=delta)
            elif et == "response.reasoning.delta":
                delta = getattr(event, "delta", "")
                if delta:
                    yield ResponseWithThought(thought=delta)
            elif et == "response.tool_call.created":
                item = getattr(event, "item", None)
                tool_id = (
                    getattr(event, "id", None)
                    or getattr(event, "tool_call_id", None)
                    or (getattr(item, "id", None) if item is not None else None)
                )
                name = (
                    getattr(event, "name", None)
                    or (getattr(item, "name", None) if item is not None else None)
                    or getattr(event, "tool_name", None)
                )
                if tool_id:
                    if tool_id not in tool_calls_args:
                        tool_calls_args[tool_id] = ""
                        tool_call_order.append(tool_id)
                    if name:
                        tool_calls_name[tool_id] = name
            elif et == "response.tool_call.delta":
                tool_id = getattr(event, "id", None) or getattr(event, "tool_call_id", None)
                delta = getattr(event, "delta", None)
                if hasattr(delta, "arguments"):
                    arg_delta = getattr(delta, "arguments", "")
                else:
                    arg_delta = delta if isinstance(delta, str) else getattr(event, "arguments", "")
                if tool_id and arg_delta:
                    tool_calls_args[tool_id] = tool_calls_args.get(tool_id, "") + arg_delta
                    if tool_id not in tool_call_order:
                        tool_call_order.append(tool_id)
            elif et == "response.tool_call.completed":
                item = getattr(event, "item", None)
                tool_id = (
                    getattr(event, "id", None)
                    or getattr(event, "tool_call_id", None)
                    or (getattr(item, "id", None) if item is not None else None)
                )
                args_final = None
                if item is not None:
                    args_final = getattr(item, "arguments", None) or getattr(item, "args", None)
                if tool_id and args_final:
                    tool_calls_args[tool_id] = args_final if isinstance(args_final, str) else json.dumps(args_final)
                    if tool_id not in tool_call_order:
                        tool_call_order.append(tool_id)
            # ignore other events for now
        last_final = await stream.get_final_response()

    # Usage/cost tracking after stream ends
    input_tokens = last_final.usage.input_tokens if last_final and last_final.usage else 0
    output_tokens = last_final.usage.output_tokens if last_final and last_final.usage else 0
    cost = 0
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, usage=tracer.get("usage"), cost=cost
    )

    # If there are tool calls, package them into aggregated text for tracing parity
    if tool_call_order:
        packaged_tool_calls: List[ToolCall] = []
        for tool_id in tool_call_order:
            name = tool_calls_name.get(tool_id) or ""
            args_str = tool_calls_args.get(tool_id, "")
            try:
                args = json.loads(args_str) if isinstance(args_str, str) else args_str
            except Exception:
                logger.warning(f"Failed to parse tool call arguments for {tool_id}: {args_str}")
                args = {}
            packaged_tool_calls.append(ToolCall(name=name, args=args, id=tool_id))
        # Move any text into trace thought
        tracer_text = aggregated_text
        aggregated_text = json.dumps([tc.__dict__ for tc in packaged_tool_calls])
        # Save for trace below
        if tracer_text:
            tracer.setdefault("_responses_stream_text", tracer_text)

    if is_none_or_empty(aggregated_text):
        logger.warning(f"No response by {model_name}\nLast Message by {messages[-1].role}: {messages[-1].content}.")
        raise ValueError(f"Empty or no response by {model_name} over Responses API. Retry if needed.")

    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        # If tool-calls were present, include any streamed text in the trace thought
        trace_payload = aggregated_text
        if tracer.get("_responses_stream_text"):
            thoughts = tracer.pop("_responses_stream_text")
            trace_payload = thoughts
        commit_conversation_trace(messages, trace_payload, tracer)


def get_structured_output_support(model_name: str, api_base_url: str = None) -> StructuredOutputSupport:
    if model_name.startswith("deepseek-reasoner"):
        return StructuredOutputSupport.NONE
    if api_base_url:
        host = urlparse(api_base_url).hostname
        if host and host.endswith(".ai.azure.com"):
            return StructuredOutputSupport.OBJECT
        if host == "api.deepinfra.com":
            return StructuredOutputSupport.OBJECT
    return StructuredOutputSupport.TOOL


def format_message_for_api(raw_messages: List[ChatMessage], model_name: str, api_base_url: str) -> List[dict]:
    """
    Format messages to send to chat model served over OpenAI (compatible) API.
    """
    formatted_messages = []
    messages = deepcopy(raw_messages)
    for message in reversed(messages):  # Process in reverse to not mess up iterator when drop invalid messages
        # Handle tool call and tool result message types
        message_type = message.additional_kwargs.get("message_type")
        if message_type == "tool_call":
            if supports_responses_api(model_name, api_base_url):
                for part in message.content:
                    if "status" in part:
                        part.pop("status")  # Drop unsupported tool call status field
                formatted_messages.extend(message.content)
                continue
            # Convert tool_call to OpenAI function call format
            content = []
            for part in message.content:
                # Skip tool calls without valid IDs as OpenAI API requires string IDs
                if not part.get("id"):
                    logger.warning(f"Dropping tool call without valid ID: {part.get('name')}")
                    continue
                content.append(
                    {
                        "type": "function",
                        "id": part.get("id"),
                        "function": {
                            "name": part.get("name"),
                            "arguments": json.dumps(part.get("input", part.get("args", {}))),
                        },
                    }
                )
            # Only add the message if there are valid tool calls
            if content:
                formatted_messages.append(
                    {
                        "role": "assistant",
                        "content": None,
                        "tool_calls": content,
                    }
                )
            else:
                logger.warning("Dropping tool call message with no valid tool calls")
            continue
        if message_type == "tool_result":
            # Convert tool_result to OpenAI tool result format
            # Each part is a result for a tool call
            for part in message.content:
                tool_call_id = part.get("id") or part.get("tool_use_id")
                # Skip tool results without valid tool_call_id as OpenAI API requires string IDs
                if not tool_call_id:
                    logger.warning(f"Dropping tool result without valid tool_call_id: {part.get('name')}")
                    continue
                if supports_responses_api(model_name, api_base_url):
                    formatted_messages.append(
                        {
                            "type": "function_call_output",
                            "call_id": tool_call_id,
                            "output": part.get("content") or "No output",
                        }
                    )
                else:
                    formatted_messages.append(
                        {
                            "role": "tool",
                            "tool_call_id": tool_call_id,
                            "name": part.get("name"),
                            "content": part.get("content") or "No output",
                        }
                    )
            continue
        if isinstance(message.content, list) and not supports_responses_api(model_name, api_base_url):
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
                    and (api_base_url.startswith("https://api.deepinfra.com/v1") or is_cerebras_api(api_base_url))
                ):
                    assistant_texts += [part["text"]]
                    message.content.pop(idx)
            if assistant_texts:
                assistant_texts_str = "\n\n".join(assistant_texts)
                if has_images:
                    message.content += [{"type": "text", "text": assistant_texts_str}]
                else:
                    message.content = assistant_texts_str
        elif isinstance(message.content, list):
            # Drop invalid content parts
            for part in reversed(message.content):
                if part["type"] == "text" and not part.get("text"):
                    message.content.remove(part)
                elif part["type"] == "image_url" and not part.get("image_url"):
                    message.content.remove(part)
                # OpenAI models use the Responses API which uses slightly different content types
                if part["type"] == "text":
                    part["type"] = "output_text" if message.role == "assistant" else "input_text"
                if part["type"] == "image_url":
                    part["type"] = "output_image" if message.role == "assistant" else "input_image"
                    part["image_url"] = part["image_url"]["url"]
            # If no valid content parts left, remove the message
            if is_none_or_empty(message.content):
                messages.remove(message)
                continue
        elif isinstance(message.content, str) and not message.content.strip():
            # If content is empty string, remove the message
            messages.remove(message)
            continue
        formatted_messages.append({"role": message.role, "content": message.content})

    return list(reversed(formatted_messages))


def is_openai_api(api_base_url: str = None) -> bool:
    """
    Check if the model is served over the official OpenAI API
    """
    return api_base_url is None or api_base_url.startswith("https://api.openai.com/v1")


def supports_responses_api(model_name: str, api_base_url: str = None) -> bool:
    """
    Check if the model, ai api supports the OpenAI Responses API
    """
    return is_openai_api(api_base_url)


def is_openai_reasoning_model(model_name: str, api_base_url: str = None) -> bool:
    """
    Check if the model is an OpenAI reasoning model
    """
    return (
        is_openai_api(api_base_url)
        and (model_name.lower().startswith("o") or model_name.lower().startswith("gpt-5"))
        or "gpt-oss" in model_name.lower()
    )


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
    reasoning_models = "grok-3-mini", "grok-4"
    return (
        any(prefix in model_name.lower() for prefix in reasoning_models)
        and api_base_url is not None
        and api_base_url.startswith("https://api.x.ai/v1")
    )


def is_cerebras_api(api_base_url: str | None = None) -> bool:
    """
    Check if the model is served over the Cerebras API
    """
    return api_base_url is not None and api_base_url.startswith("https://api.cerebras.ai/v1")


def is_groq_api(api_base_url: str | None = None) -> bool:
    """
    Check if the model is served over the Groq API
    """
    return api_base_url is not None and api_base_url.startswith("https://api.groq.com")


def is_instream_thinking_model(model_name: str) -> bool:
    """
    Check if the model uses in-stream thinking style, i.e., <think>...</think> in output
    """
    instream_thinking_model = ["kimi-k2-thinking", "minimax-m2"]
    return any(prefix in model_name.lower() for prefix in instream_thinking_model)


def is_qwen_style_reasoning_model(model_name: str, api_base_url: str | None = None) -> bool:
    """
    Check if the model is a Qwen style reasoning model
    """
    qwen_style_reason_model = ["qwen3", "smollm3"]
    return any(prefix in model_name.lower() for prefix in qwen_style_reason_model) and api_base_url is not None


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
    Generator of chunks from the standard openai chat completions stream.
    """
    for chunk in chat_stream:
        yield chunk


async def astream_thought_processor(
    chat_stream: openai.AsyncStream[ChatCompletionChunk],
) -> AsyncGenerator[ChatCompletionWithThoughtsChunk, None]:
    """
    Async generator of chunks from standard openai chat completions stream with thoughts/reasoning.
    """
    async for chunk in chat_stream:
        try:
            # Validate the chunk has the required fields before processing
            chunk_data = chunk.model_dump()

            # Skip chunks that don't have the required object field or have invalid values
            if "object" in chunk_data and chunk_data.get("object") == "":
                chunk_data["object"] = "chat.completion.chunk"
            if not chunk_data.get("object") or chunk_data.get("object") != "chat.completion.chunk":
                logger.warning(f"Skipping invalid chunk with object field: {chunk_data.get('object', 'missing')}")
                continue
            # Handle unsupported service tiers like "on_demand" by Groq
            if chunk.service_tier and chunk.service_tier == "on_demand":
                chunk_data["service_tier"] = "auto"

            tchunk = ChatCompletionWithThoughtsChunk.model_validate(chunk_data)

            # Handlle deepseek style response with thoughts. Used by AI APIs like vLLM, sgLang, DeepSeek, LiteLLM.
            if (
                len(tchunk.choices) > 0
                and hasattr(tchunk.choices[0].delta, "reasoning_content")
                and tchunk.choices[0].delta.reasoning_content
            ):
                tchunk.choices[0].delta.thought = chunk.choices[0].delta.reasoning_content

            # Handlle openai reasoning style response with thoughts. Used by gpt-oss.
            if (
                len(tchunk.choices) > 0
                and hasattr(tchunk.choices[0].delta, "reasoning")
                and tchunk.choices[0].delta.reasoning
            ):
                tchunk.choices[0].delta.thought = chunk.choices[0].delta.reasoning

            # Handlle llama.cpp server style response with thoughts.
            elif len(tchunk.choices) > 0 and tchunk.choices[0].delta.model_extra.get("reasoning_content"):
                tchunk.choices[0].delta.thought = tchunk.choices[0].delta.model_extra.get("reasoning_content")

            yield tchunk
        except Exception as e:
            logger.warning(f"Error processing chunk: {e}. Skipping malformed chunk.")
            continue


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
    Async generator for chat completion with structured and inline thought chunks.
    Assumes <thought_tag>...</thought_tag> can only appear once at the start.
    Handles partial tags across streamed chunks.
    """
    start_tag = f"<{thought_tag}>"
    end_tag = f"</{thought_tag}>"
    buf: str = ""
    # Modes and transitions: detect_start > thought (optional) > message
    mode = "detect_start"

    async for chunk in astream_thought_processor(chat_stream):
        if len(chunk.choices) == 0:
            continue
        if mode == "message":
            # Message mode is terminal, so just yield chunks, no processing
            yield chunk
            continue

        if chunk.choices[0].delta.content is None:
            # If delta content is None, we can't process it, just yield the chunk
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


def to_openai_tools(tools: List[ToolDefinition], model: str, api_base_url: str | None = None) -> List[Dict] | None:
    "Transform tool definitions from standard format to OpenAI format."
    use_responses_api = supports_responses_api(model, api_base_url)
    strict = not is_cerebras_api(api_base_url)
    fields_to_exclude = ["minimum", "maximum"] if is_groq_api(api_base_url) else []
    if use_responses_api:
        openai_tools = [
            {
                "type": "function",
                "name": tool.name,
                "description": tool.description,
                "parameters": clean_response_schema(tool.schema, fields_to_exclude=fields_to_exclude),
                "strict": strict,
            }
            for tool in tools
        ]
    else:
        openai_tools = [
            {
                "type": "function",
                "function": {
                    "name": tool.name,
                    "description": tool.description,
                    "parameters": clean_response_schema(tool.schema, fields_to_exclude=fields_to_exclude),
                    "strict": strict,
                },
            }
            for tool in tools
        ]

    return openai_tools or None


def clean_response_schema(schema: BaseModel | dict, fields_to_exclude: list[str] = []) -> dict:
    """
    Format response schema to be compatible with OpenAI API.

    Clean the response schema by removing unsupported fields.
    """
    # Normalize schema to OpenAI compatible JSON schema format
    schema_json = schema if isinstance(schema, dict) else schema.model_json_schema()
    schema_json = _ensure_strict_json_schema(schema_json, path=(), root=schema_json)

    # Recursively drop unsupported fields from schema passed to OpenAI API
    # See https://platform.openai.com/docs/guides/structured-outputs#supported-schemas
    fields_to_exclude += ["minItems", "maxItems"]
    if isinstance(schema_json, dict) and isinstance(schema_json.get("properties"), dict):
        for _, prop_value in schema_json["properties"].items():
            if isinstance(prop_value, dict):
                # Remove specified fields from direct properties
                for field in fields_to_exclude:
                    prop_value.pop(field, None)
            # Recursively remove specified fields from child properties
            if "items" in prop_value and isinstance(prop_value["items"], dict):
                clean_response_schema(prop_value["items"])

    # Return cleaned schema
    return schema_json
