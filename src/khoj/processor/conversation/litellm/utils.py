import json
import logging
from time import perf_counter
from typing import AsyncGenerator, Dict, List, Optional

from langchain_core.messages.chat import ChatMessage
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import (
    ResponseWithThought,
    ToolCall,
    commit_conversation_trace,
)
from khoj.utils.helpers import (
    ToolDefinition,
    get_chat_usage_metrics,
    is_none_or_empty,
    is_promptrace_enabled,
)

logger = logging.getLogger(__name__)

MAX_COMPLETION_TOKENS = 16000


def format_messages_for_litellm(raw_messages: List[ChatMessage]) -> List[dict]:
    """Format ChatMessage objects into the OpenAI-compatible dict format used by LiteLLM."""
    formatted_messages = []
    for message in raw_messages:
        message_type = message.additional_kwargs.get("message_type")
        if message_type == "tool_call" and isinstance(message.content, list):
            content = []
            for part in message.content:
                if not isinstance(part, dict) or not part.get("id"):
                    logger.warning(f"Dropping tool call without valid ID: {part}")
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
            if content:
                formatted_messages.append({"role": "assistant", "content": None, "tool_calls": content})
            continue
        if message_type == "tool_result" and isinstance(message.content, list):
            for part in message.content:
                if not isinstance(part, dict):
                    continue
                tool_call_id = part.get("id") or part.get("tool_use_id")
                if not tool_call_id:
                    continue
                formatted_messages.append(
                    {"role": "tool", "tool_call_id": tool_call_id, "content": part.get("content") or "No output"}
                )
            continue

        msg: dict = {"role": message.role, "content": message.content}
        if message.role == "assistant" and not message.content:
            continue
        formatted_messages.append(msg)
    return formatted_messages


def to_litellm_tools(tools: List[ToolDefinition]) -> List[Dict] | None:
    """Transform tool definitions to OpenAI-compatible format for LiteLLM."""
    if not tools:
        return None
    litellm_tools = [
        {
            "type": "function",
            "function": {
                "name": tool.name,
                "description": tool.description,
                "parameters": tool.schema,
            },
        }
        for tool in tools
    ]
    return litellm_tools or None


@retry(
    retry=(retry_if_exception_type(Exception)),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def litellm_completion_with_backoff(
    messages: List[ChatMessage],
    model_name: str,
    api_key: str,
    api_base_url: Optional[str] = None,
    temperature: float = 0.6,
    deepthought: bool = False,
    model_kwargs: dict = {},
    tracer: dict = {},
) -> ResponseWithThought:
    import litellm

    formatted_messages = format_messages_for_litellm(messages)

    model_kwargs["temperature"] = temperature
    model_kwargs["max_tokens"] = model_kwargs.get("max_tokens", MAX_COMPLETION_TOKENS)

    response = litellm.completion(
        model=model_name,
        messages=formatted_messages,
        api_key=api_key,
        api_base=api_base_url,
        stream=False,
        **model_kwargs,
    )

    aggregated_response = response.choices[0].message.content or ""
    thoughts = ""
    if hasattr(response.choices[0].message, "reasoning_content"):
        thoughts = response.choices[0].message.reasoning_content or ""

    tool_calls: list[ToolCall] = []
    raw_tool_calls = response.choices[0].message.tool_calls
    if raw_tool_calls:
        for tool in raw_tool_calls:
            args = tool.function.arguments
            if isinstance(args, str):
                args = json.loads(args)
            tool_calls.append(ToolCall(name=tool.function.name, args=args, id=tool.id))
        if thoughts and aggregated_response:
            thoughts = "\n".join([f"*{line.strip()}*" for line in thoughts.splitlines() if line.strip()])
            thoughts = f"{thoughts}\n\n{aggregated_response}"
        else:
            thoughts = thoughts or aggregated_response
        aggregated_response = json.dumps([tc.__dict__ for tc in tool_calls])

    # Calculate cost
    input_tokens = getattr(response.usage, "prompt_tokens", 0) if response.usage else 0
    output_tokens = getattr(response.usage, "completion_tokens", 0) if response.usage else 0
    tracer["usage"] = get_chat_usage_metrics(model_name, input_tokens, output_tokens, usage=tracer.get("usage"))

    if is_none_or_empty(aggregated_response):
        raise ValueError(f"Empty or no response by {model_name} via LiteLLM. Retry if needed.")

    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, aggregated_response, tracer)

    return ResponseWithThought(text=aggregated_response, thought=thoughts)


@retry(
    retry=(retry_if_exception_type(Exception)),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.WARNING),
    reraise=False,
)
async def litellm_chat_completion_with_backoff(
    messages: List[ChatMessage],
    model_name: str,
    api_key: str,
    api_base_url: Optional[str] = None,
    temperature: float = 0.6,
    deepthought: bool = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    import litellm

    formatted_messages = format_messages_for_litellm(messages)

    model_kwargs: dict = {
        "temperature": temperature,
        "max_tokens": MAX_COMPLETION_TOKENS,
    }

    aggregated_response = ""
    response_started = False
    start_time = perf_counter()

    response = await litellm.acompletion(
        model=model_name,
        messages=formatted_messages,
        api_key=api_key,
        api_base=api_base_url,
        stream=True,
        stream_options={"include_usage": True},
        **model_kwargs,
    )

    input_tokens, output_tokens = 0, 0
    async for chunk in response:
        if not response_started:
            response_started = True
            logger.info(f"First response took: {perf_counter() - start_time:.3f} seconds")

        if hasattr(chunk, "usage") and chunk.usage:
            input_tokens = getattr(chunk.usage, "prompt_tokens", 0) or input_tokens
            output_tokens = getattr(chunk.usage, "completion_tokens", 0) or output_tokens

        if not chunk.choices:
            continue

        delta = chunk.choices[0].delta
        if delta and delta.content:
            response_chunk = ResponseWithThought(text=delta.content)
            aggregated_response += delta.content
            yield response_chunk
        elif delta and hasattr(delta, "reasoning_content") and delta.reasoning_content:
            yield ResponseWithThought(thought=delta.reasoning_content)

    tracer["usage"] = get_chat_usage_metrics(model_name, input_tokens, output_tokens, usage=tracer.get("usage"))
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, aggregated_response, tracer)
