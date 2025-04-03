import logging
import os
import random
from copy import deepcopy
from threading import Thread
from typing import Dict

from google import genai
from google.genai import errors as gerrors
from google.genai import types as gtypes
from langchain.schema import ChatMessage
from pydantic import BaseModel
from tenacity import (
    before_sleep_log,
    retry,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import (
    ThreadedGenerator,
    commit_conversation_trace,
    get_image_from_base64,
    get_image_from_url,
)
from khoj.utils.helpers import (
    get_ai_api_info,
    get_chat_usage_metrics,
    is_none_or_empty,
    is_promptrace_enabled,
)

logger = logging.getLogger(__name__)

gemini_clients: Dict[str, genai.Client] = {}

MAX_OUTPUT_TOKENS_GEMINI = 8192
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


def get_gemini_client(api_key, api_base_url=None) -> genai.Client:
    api_info = get_ai_api_info(api_key, api_base_url)
    return genai.Client(
        location=api_info.region,
        project=api_info.project,
        credentials=api_info.credentials,
        api_key=api_info.api_key,
        vertexai=api_info.api_key is None,
    )


@retry(
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def gemini_completion_with_backoff(
    messages, system_prompt, model_name, temperature=1.0, api_key=None, api_base_url=None, model_kwargs=None, tracer={}
) -> str:
    client = gemini_clients.get(api_key)
    if not client:
        client = get_gemini_client(api_key, api_base_url)
        gemini_clients[api_key] = client

    formatted_messages, system_prompt = format_messages_for_gemini(messages, system_prompt)

    # format model response schema
    response_schema = None
    if model_kwargs and "response_schema" in model_kwargs:
        response_schema = clean_response_schema(model_kwargs["response_schema"])

    seed = int(os.getenv("KHOJ_LLM_SEED")) if os.getenv("KHOJ_LLM_SEED") else None
    config = gtypes.GenerateContentConfig(
        system_instruction=system_prompt,
        temperature=temperature,
        max_output_tokens=MAX_OUTPUT_TOKENS_GEMINI,
        safety_settings=SAFETY_SETTINGS,
        response_mime_type=model_kwargs.get("response_mime_type", "text/plain") if model_kwargs else "text/plain",
        response_schema=response_schema,
        seed=seed,
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
    input_tokens = response.usage_metadata.prompt_token_count if response else 0
    output_tokens = response.usage_metadata.candidates_token_count if response else 0
    tracer["usage"] = get_chat_usage_metrics(model_name, input_tokens, output_tokens, usage=tracer.get("usage"))

    # Save conversation trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, response_text, tracer)

    return response_text


@retry(
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def gemini_chat_completion_with_backoff(
    messages,
    compiled_references,
    online_results,
    model_name,
    temperature,
    api_key,
    api_base_url,
    system_prompt,
    completion_func=None,
    model_kwargs=None,
    tracer: dict = {},
):
    g = ThreadedGenerator(compiled_references, online_results, completion_func=completion_func)
    t = Thread(
        target=gemini_llm_thread,
        args=(g, messages, system_prompt, model_name, temperature, api_key, api_base_url, model_kwargs, tracer),
    )
    t.start()
    return g


def gemini_llm_thread(
    g,
    messages,
    system_prompt,
    model_name,
    temperature,
    api_key,
    api_base_url=None,
    model_kwargs=None,
    tracer: dict = {},
):
    try:
        client = gemini_clients.get(api_key)
        if not client:
            client = get_gemini_client(api_key, api_base_url)
            gemini_clients[api_key] = client

        formatted_messages, system_prompt = format_messages_for_gemini(messages, system_prompt)

        seed = int(os.getenv("KHOJ_LLM_SEED")) if os.getenv("KHOJ_LLM_SEED") else None
        config = gtypes.GenerateContentConfig(
            system_instruction=system_prompt,
            temperature=temperature,
            max_output_tokens=MAX_OUTPUT_TOKENS_GEMINI,
            stop_sequences=["Notes:\n["],
            safety_settings=SAFETY_SETTINGS,
            seed=seed,
        )

        aggregated_response = ""

        for chunk in client.models.generate_content_stream(
            model=model_name, config=config, contents=formatted_messages
        ):
            message, stopped = handle_gemini_response(chunk.candidates, chunk.prompt_feedback)
            message = message or chunk.text
            aggregated_response += message
            g.send(message)
            if stopped:
                raise ValueError(message)

        # Calculate cost of chat
        input_tokens = chunk.usage_metadata.prompt_token_count
        output_tokens = chunk.usage_metadata.candidates_token_count
        tracer["usage"] = get_chat_usage_metrics(model_name, input_tokens, output_tokens, usage=tracer.get("usage"))

        # Save conversation trace
        tracer["chat_model"] = model_name
        tracer["temperature"] = temperature
        if is_promptrace_enabled():
            commit_conversation_trace(messages, aggregated_response, tracer)
    except ValueError as e:
        logger.warning(
            f"LLM Response Prevented for {model_name}: {e.args[0]}.\n"
            + f"Last Message by {messages[-1].role}: {messages[-1].content}"
        )
    except Exception as e:
        logger.error(f"Error in gemini_llm_thread: {e}", exc_info=True)
    finally:
        g.close()


def handle_gemini_response(
    candidates: list[gtypes.Candidate], prompt_feedback: gtypes.GenerateContentResponsePromptFeedback = None
):
    """Check if Gemini response was blocked and return an explanatory error message."""
    # Check if the response was blocked due to safety concerns with the prompt
    if len(candidates) == 0 and prompt_feedback:
        message = f"\nI'd prefer to not respond to that due to **{prompt_feedback.block_reason.name}** issues with your query."
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
) -> tuple[list[str], str]:
    # Extract system message
    system_prompt = system_prompt or ""
    messages = deepcopy(original_messages)
    for message in messages.copy():
        if message.role == "system":
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
                logger.error(f"Dropping empty message content")
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
