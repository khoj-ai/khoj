import logging
import os
from threading import Thread
from typing import Dict, List
from urllib.parse import urlparse

import openai
from openai.types.chat.chat_completion import ChatCompletion
from openai.types.chat.chat_completion_chunk import ChatCompletionChunk
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
    ThreadedGenerator,
    commit_conversation_trace,
)
from khoj.utils.helpers import (
    get_chat_usage_metrics,
    get_openai_client,
    is_promptrace_enabled,
)

logger = logging.getLogger(__name__)

openai_clients: Dict[str, openai.OpenAI] = {}


@retry(
    retry=(
        retry_if_exception_type(openai._exceptions.APITimeoutError)
        | retry_if_exception_type(openai._exceptions.APIError)
        | retry_if_exception_type(openai._exceptions.APIConnectionError)
        | retry_if_exception_type(openai._exceptions.RateLimitError)
        | retry_if_exception_type(openai._exceptions.APIStatusError)
    ),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def completion_with_backoff(
    messages,
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

    formatted_messages = [{"role": message.role, "content": message.content} for message in messages]

    # Tune reasoning models arguments
    if model_name.startswith("o1") or model_name.startswith("o3"):
        temperature = 1
        model_kwargs["reasoning_effort"] = "medium"

    model_kwargs["stream_options"] = {"include_usage": True}
    if os.getenv("KHOJ_LLM_SEED"):
        model_kwargs["seed"] = int(os.getenv("KHOJ_LLM_SEED"))

    aggregated_response = ""
    with client.beta.chat.completions.stream(
        messages=formatted_messages,  # type: ignore
        model=model_name,
        temperature=temperature,
        timeout=20,
        **model_kwargs,
    ) as chat:
        for chunk in chat:
            if chunk.type == "error":
                logger.error(f"Openai api response error: {chunk.error}", exc_info=True)
                continue
            elif chunk.type == "content.delta":
                aggregated_response += chunk.delta

    # Calculate cost of chat
    input_tokens = chunk.usage.prompt_tokens if hasattr(chunk, "usage") and chunk.usage else 0
    output_tokens = chunk.usage.completion_tokens if hasattr(chunk, "usage") and chunk.usage else 0
    cost = (
        chunk.usage.model_extra.get("estimated_cost", 0) if hasattr(chunk, "usage") and chunk.usage else 0
    )  # Estimated costs returned by DeepInfra API

    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, usage=tracer.get("usage"), cost=cost
    )

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
    ),
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def chat_completion_with_backoff(
    messages,
    compiled_references,
    online_results,
    model_name,
    temperature,
    openai_api_key=None,
    api_base_url=None,
    completion_func=None,
    deepthought=False,
    model_kwargs=None,
    tracer: dict = {},
):
    g = ThreadedGenerator(compiled_references, online_results, completion_func=completion_func)
    t = Thread(
        target=llm_thread,
        args=(g, messages, model_name, temperature, openai_api_key, api_base_url, deepthought, model_kwargs, tracer),
    )
    t.start()
    return g


def llm_thread(
    g,
    messages,
    model_name: str,
    temperature,
    openai_api_key=None,
    api_base_url=None,
    deepthought=False,
    model_kwargs: dict = {},
    tracer: dict = {},
):
    try:
        client_key = f"{openai_api_key}--{api_base_url}"
        client = openai_clients.get(client_key)
        if not client:
            client = get_openai_client(openai_api_key, api_base_url)
            openai_clients[client_key] = client

        formatted_messages = [{"role": message.role, "content": message.content} for message in messages]

        # Tune reasoning models arguments
        if model_name.startswith("o1") or model_name.startswith("o3"):
            temperature = 1
            model_kwargs["reasoning_effort"] = "medium"

        if model_name.startswith("o3"):
            # Get the first system message and add the string `Formatting re-enabled` to it.
            # See https://platform.openai.com/docs/guides/reasoning-best-practices
            if len(formatted_messages) > 0:
                system_messages = [
                    (i, message) for i, message in enumerate(formatted_messages) if message["role"] == "system"
                ]
                if len(system_messages) > 0:
                    first_system_message_index, first_system_message = system_messages[0]
                    formatted_messages[first_system_message_index][
                        "content"
                    ] = f"{first_system_message} Formatting re-enabled"
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

        stream = True
        model_kwargs["stream_options"] = {"include_usage": True}
        if os.getenv("KHOJ_LLM_SEED"):
            model_kwargs["seed"] = int(os.getenv("KHOJ_LLM_SEED"))

        chat: ChatCompletion | openai.Stream[ChatCompletionChunk] = client.chat.completions.create(
            messages=formatted_messages,
            model=model_name,  # type: ignore
            stream=stream,
            temperature=temperature,
            timeout=20,
            **model_kwargs,
        )

        aggregated_response = ""
        if not stream:
            chunk = chat
            aggregated_response = chunk.choices[0].message.content
            g.send(aggregated_response)
        else:
            for chunk in chat:
                if len(chunk.choices) == 0:
                    continue
                delta_chunk = chunk.choices[0].delta
                text_chunk = ""
                if isinstance(delta_chunk, str):
                    text_chunk = delta_chunk
                elif delta_chunk.content:
                    text_chunk = delta_chunk.content
                if text_chunk:
                    aggregated_response += text_chunk
                    g.send(text_chunk)

        # Calculate cost of chat
        input_tokens = chunk.usage.prompt_tokens if hasattr(chunk, "usage") and chunk.usage else 0
        output_tokens = chunk.usage.completion_tokens if hasattr(chunk, "usage") and chunk.usage else 0
        cost = (
            chunk.usage.model_extra.get("estimated_cost", 0) if hasattr(chunk, "usage") and chunk.usage else 0
        )  # Estimated costs returned by DeepInfra API
        tracer["usage"] = get_chat_usage_metrics(
            model_name, input_tokens, output_tokens, usage=tracer.get("usage"), cost=cost
        )

        # Save conversation trace
        tracer["chat_model"] = model_name
        tracer["temperature"] = temperature
        if is_promptrace_enabled():
            commit_conversation_trace(messages, aggregated_response, tracer)
    except Exception as e:
        logger.error(f"Error in llm_thread: {e}", exc_info=True)
    finally:
        g.close()


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
