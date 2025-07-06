import logging
import os
import random
from copy import deepcopy
from time import perf_counter
from typing import AsyncGenerator, AsyncIterator, Dict

import httpx
from google import genai
from google.genai import errors as gerrors
from google.genai import types as gtypes
from langchain_core.messages.chat import ChatMessage
from pydantic import BaseModel
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import (
    ResponseWithThought,
    commit_conversation_trace,
    get_image_from_base64,
    get_image_from_url,
)
from khoj.utils.helpers import (
    get_chat_usage_metrics,
    get_gemini_client,
    is_none_or_empty,
    is_promptrace_enabled,
)

logger = logging.getLogger(__name__)

gemini_clients: Dict[str, genai.Client] = {}

# Output tokens should be more than reasoning tokens.
# This avoids premature response termination.
MAX_OUTPUT_TOKENS_FOR_REASONING_GEMINI = 20000
MAX_OUTPUT_TOKENS_FOR_STANDARD_GEMINI = 8000
MAX_REASONING_TOKENS_GEMINI = 10000

SAFETY_SETTINGS = [
    gtypes.SafetySetting(
        category=gtypes.HarmCategory.HARM_CATEGORY_DANGEROUS_CONTENT,
        threshold=gtypes.HarmBlockThreshold.BLOCK_ONLY_HIGH,
    ),
    gtypes.SafetySetting(
        category=gtypes.HarmCategory.HARM_CATEGORY_HARASSMENT,
        threshold=gtypes.HarmBlockThreshold.BLOCK_ONLY_HIGH,
    ),
    gtypes.SafetySetting(
        category=gtypes.HarmCategory.HARM_CATEGORY_HATE_SPEECH,
        threshold=gtypes.HarmBlockThreshold.BLOCK_ONLY_HIGH,
    ),
    gtypes.SafetySetting(
        category=gtypes.HarmCategory.HARM_CATEGORY_SEXUALLY_EXPLICIT,
        threshold=gtypes.HarmBlockThreshold.BLOCK_ONLY_HIGH,
    ),
    gtypes.SafetySetting(
        category=gtypes.HarmCategory.HARM_CATEGORY_CIVIC_INTEGRITY,
        threshold=gtypes.HarmBlockThreshold.BLOCK_ONLY_HIGH,
    ),
]


def _is_retryable_error(exception: BaseException) -> bool:
    """Check if the exception is a retryable error"""
    # server errors
    if isinstance(exception, gerrors.APIError):
        return exception.code in [429, 502, 503, 504]
    # client errors
    if (
        isinstance(exception, httpx.TimeoutException)
        or isinstance(exception, httpx.NetworkError)
        or isinstance(exception, httpx.ReadError)
    ):
        return True
    # validation errors
    if isinstance(exception, ValueError):
        return True
    return False


@retry(
    retry=retry_if_exception(_is_retryable_error),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def gemini_completion_with_backoff(
    messages: list[ChatMessage],
    system_prompt: str,
    model_name: str,
    temperature=1.2,
    api_key=None,
    api_base_url: str = None,
    model_kwargs=None,
    deepthought=False,
    tracer={},
) -> str:
    client = gemini_clients.get(api_key)
    if not client:
        client = get_gemini_client(api_key, api_base_url)
        gemini_clients[api_key] = client

    formatted_messages, system_instruction = format_messages_for_gemini(messages, system_prompt)
    response_thoughts: str | None = None

    # format model response schema
    response_schema = None
    if model_kwargs and model_kwargs.get("response_schema"):
        response_schema = clean_response_schema(model_kwargs["response_schema"])

    thinking_config = None
    if deepthought and is_reasoning_model(model_name):
        thinking_config = gtypes.ThinkingConfig(thinking_budget=MAX_REASONING_TOKENS_GEMINI)

    max_output_tokens = MAX_OUTPUT_TOKENS_FOR_STANDARD_GEMINI
    if is_reasoning_model(model_name):
        max_output_tokens = MAX_OUTPUT_TOKENS_FOR_REASONING_GEMINI

    seed = int(os.getenv("KHOJ_LLM_SEED")) if os.getenv("KHOJ_LLM_SEED") else None
    config = gtypes.GenerateContentConfig(
        system_instruction=system_instruction,
        temperature=temperature,
        thinking_config=thinking_config,
        max_output_tokens=max_output_tokens,
        safety_settings=SAFETY_SETTINGS,
        response_mime_type=model_kwargs.get("response_mime_type", "text/plain") if model_kwargs else "text/plain",
        response_schema=response_schema,
        seed=seed,
        top_p=0.95,
        http_options=gtypes.HttpOptions(client_args={"timeout": httpx.Timeout(30.0, read=60.0)}),
    )

    try:
        # Generate the response
        response = client.models.generate_content(model=model_name, config=config, contents=formatted_messages)
        response_text = response.text
    except gerrors.ClientError as e:
        response = None
        response_text, _ = handle_gemini_response(e.args)
        # Respond with reason for stopping
        logger.warning(
            f"LLM Response Prevented for {model_name}: {response_text}.\n"
            + f"Last Message by {messages[-1].role}: {messages[-1].content}"
        )

    # Aggregate cost of chat
    input_tokens = response.usage_metadata.prompt_token_count or 0 if response else 0
    output_tokens = response.usage_metadata.candidates_token_count or 0 if response else 0
    thought_tokens = response.usage_metadata.thoughts_token_count or 0 if response else 0
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, thought_tokens=thought_tokens, usage=tracer.get("usage")
    )

    # Validate the response. If empty, raise an error to retry.
    if is_none_or_empty(response_text):
        logger.warning(f"No response by {model_name}\nLast Message by {messages[-1].role}: {messages[-1].content}.")
        raise ValueError(f"Empty or no response by {model_name} over API. Retry if needed.")

    # Save conversation trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, response_text, tracer)

    return response_text


@retry(
    retry=retry_if_exception(_is_retryable_error),
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.WARNING),
    reraise=False,
)
async def gemini_chat_completion_with_backoff(
    messages: list[ChatMessage],
    model_name: str,
    temperature: float,
    api_key: str,
    api_base_url: str,
    system_prompt: str,
    model_kwargs=None,
    deepthought=False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    client = gemini_clients.get(api_key)
    if not client:
        client = get_gemini_client(api_key, api_base_url)
        gemini_clients[api_key] = client

    formatted_messages, system_instruction = format_messages_for_gemini(messages, system_prompt)

    thinking_config = None
    if deepthought and is_reasoning_model(model_name):
        thinking_config = gtypes.ThinkingConfig(thinking_budget=-1, include_thoughts=True)

    max_output_tokens = MAX_OUTPUT_TOKENS_FOR_STANDARD_GEMINI
    if is_reasoning_model(model_name):
        max_output_tokens = MAX_OUTPUT_TOKENS_FOR_REASONING_GEMINI

    top_p = 0.95
    seed = int(os.getenv("KHOJ_LLM_SEED")) if os.getenv("KHOJ_LLM_SEED") else None
    config = gtypes.GenerateContentConfig(
        system_instruction=system_instruction,
        temperature=temperature,
        top_p=top_p,
        thinking_config=thinking_config,
        max_output_tokens=max_output_tokens,
        stop_sequences=["Notes:\n["],
        safety_settings=SAFETY_SETTINGS,
        seed=seed,
        http_options=gtypes.HttpOptions(async_client_args={"timeout": httpx.Timeout(30.0, read=60.0)}),
    )

    aggregated_response = ""
    final_chunk = None
    response_started = False
    start_time = perf_counter()
    chat_stream: AsyncIterator[gtypes.GenerateContentResponse] = await client.aio.models.generate_content_stream(
        model=model_name, config=config, contents=formatted_messages
    )
    async for chunk in chat_stream:
        # Log the time taken to start response
        if not response_started:
            response_started = True
            logger.info(f"First response took: {perf_counter() - start_time:.3f} seconds")
        # Keep track of the last chunk for usage data
        final_chunk = chunk

        # handle safety, rate-limit, other finish reasons
        stop_message, stopped = handle_gemini_response(chunk.candidates, chunk.prompt_feedback)
        if stopped:
            yield ResponseWithThought(response=stop_message)
            logger.warning(
                f"LLM Response Prevented for {model_name}: {stop_message}.\n"
                + f"Last Message by {messages[-1].role}: {messages[-1].content}"
            )
            break

        # emit thought vs response parts
        for part in chunk.candidates[0].content.parts or []:
            if part.thought:
                yield ResponseWithThought(thought=part.text)
            elif part.text:
                aggregated_response += part.text
                yield ResponseWithThought(response=part.text)
    # Calculate cost of chat
    input_tokens = final_chunk.usage_metadata.prompt_token_count or 0 if final_chunk else 0
    output_tokens = final_chunk.usage_metadata.candidates_token_count or 0 if final_chunk else 0
    thought_tokens = final_chunk.usage_metadata.thoughts_token_count or 0 if final_chunk else 0
    tracer["usage"] = get_chat_usage_metrics(
        model_name, input_tokens, output_tokens, thought_tokens=thought_tokens, usage=tracer.get("usage")
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


def handle_gemini_response(
    candidates: list[gtypes.Candidate], prompt_feedback: gtypes.GenerateContentResponsePromptFeedback = None
):
    """Check if Gemini response was blocked and return an explanatory error message."""
    # Check if the response was blocked due to safety concerns with the prompt
    if len(candidates) == 0 and prompt_feedback:
        message = f"\nI'd prefer to not respond to that due to **{prompt_feedback.block_reason.name}** issues with your query."
        stopped = True
    # If response hits rate limit
    elif isinstance(candidates[0], str):
        message = candidates[0]
        stopped = True
    # Check if the response was blocked due to safety concerns with the generated content
    elif candidates[0].finish_reason == gtypes.FinishReason.SAFETY:
        message = generate_safety_response(candidates[0].safety_ratings)
        stopped = True
    # Check if finish reason is empty, therefore generation is in progress
    elif not candidates[0].finish_reason:
        message = None
        stopped = False
    # Check if the response was stopped due to reaching maximum token limit or other reasons
    elif candidates[0].finish_reason != gtypes.FinishReason.STOP:
        message = f"\nI can't talk further about that because of **{candidates[0].finish_reason.name} issue.**"
        stopped = True
    # Otherwise, the response is valid and can be used
    else:
        message = None
        stopped = False
    return message, stopped


def generate_safety_response(safety_ratings: list[gtypes.SafetyRating]):
    """Generate a conversational response based on the safety ratings of the response."""
    # Get the safety rating with the highest probability
    max_safety_rating: gtypes.SafetyRating = sorted(safety_ratings, key=lambda x: x.probability, reverse=True)[0]
    # Remove the "HARM_CATEGORY_" prefix and title case the category name
    max_safety_category = " ".join(max_safety_rating.category.name.split("_")[2:]).title()
    # Add a bit of variety to the discomfort level based on the safety rating probability
    discomfort_level = {
        gtypes.HarmProbability.HARM_PROBABILITY_UNSPECIFIED: " ",
        gtypes.HarmProbability.NEGLIGIBLE: "a little ",
        gtypes.HarmProbability.LOW: "a bit ",
        gtypes.HarmProbability.MEDIUM: "moderately ",
        gtypes.HarmProbability.HIGH: random.choice(["very ", "quite ", "fairly "]),
    }[max_safety_rating.probability]
    # Generate a response using a random response template
    safety_response_choice = random.choice(
        [
            "\nUmm, I'd rather not to respond to that. The conversation has some probability of going into **{category}** territory.",
            "\nI'd prefer not to talk about **{category}** related topics. It makes me {discomfort_level}uncomfortable.",
            "\nI feel {discomfort_level}squeamish talking about **{category}** related stuff! Can we talk about something less controversial?",
            "\nThat sounds {discomfort_level}outside the [Overtone Window](https://en.wikipedia.org/wiki/Overton_window) of acceptable conversation. Should we stick to something less {category} related?",
        ]
    )
    return safety_response_choice.format(
        category=max_safety_category, probability=max_safety_rating.probability.name, discomfort_level=discomfort_level
    )


def format_messages_for_gemini(
    original_messages: list[ChatMessage], system_prompt: str = None
) -> tuple[list[gtypes.Content], str]:
    # Extract system message
    system_prompt = system_prompt or ""
    messages = deepcopy(original_messages)
    for message in messages.copy():
        if message.role == "system":
            if isinstance(message.content, list):
                system_prompt += "\n".join([part["text"] for part in message.content if part["type"] == "text"])
            else:
                system_prompt += message.content
            messages.remove(message)
    system_prompt = None if is_none_or_empty(system_prompt) else system_prompt

    for message in messages:
        # Convert message content to string list from chatml dictionary list
        if isinstance(message.content, list):
            # Convert image_urls to PIL.Image and place them at beginning of list (better for Gemini)
            message_content = []
            for item in sorted(message.content, key=lambda x: 0 if x["type"] == "image_url" else 1):
                if item["type"] == "image_url":
                    image_data = item["image_url"]["url"]
                    if image_data.startswith("http"):
                        image = get_image_from_url(image_data, type="bytes")
                    else:
                        image = get_image_from_base64(image_data, type="bytes")
                    message_content += [gtypes.Part.from_bytes(data=image.content, mime_type=image.type)]
                elif not is_none_or_empty(item.get("text")):
                    message_content += [gtypes.Part.from_text(text=item["text"])]
                else:
                    logger.error(f"Dropping invalid message content part: {item}")
            if not message_content:
                logger.error(f"Dropping message with empty content as not supported:\n{message}")
                messages.remove(message)
                continue
            message.content = message_content
        elif isinstance(message.content, str):
            message.content = [gtypes.Part.from_text(text=message.content)]
        else:
            logger.error(f"Dropping invalid type: {type(message.content)} of message content: {message.content}")
            messages.remove(message)
            continue

        if message.role == "assistant":
            message.role = "model"

    if len(messages) == 1:
        messages[0].role = "user"

    formatted_messages = [gtypes.Content(role=message.role, parts=message.content) for message in messages]
    return formatted_messages, system_prompt


def clean_response_schema(response_schema: BaseModel) -> dict:
    """
    Convert Pydantic model to dict for Gemini response schema.

    Ensure response schema adheres to the order of the original property definition.
    """
    # Convert Pydantic model to dict
    response_schema_dict = response_schema.model_json_schema()
    # Get field names in original definition order
    field_names = list(response_schema.model_fields.keys())
    # Generate content in the order in which the schema properties were defined
    response_schema_dict["property_ordering"] = field_names
    return response_schema_dict


def is_reasoning_model(model_name: str) -> bool:
    """
    Check if the model is a reasoning model.
    """
    return model_name.startswith("gemini-2.5")
