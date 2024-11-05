import logging
import random
from threading import Thread

import google.generativeai as genai
from google.generativeai.types.answer_types import FinishReason
from google.generativeai.types.generation_types import StopCandidateException
from google.generativeai.types.safety_types import (
    HarmBlockThreshold,
    HarmCategory,
    HarmProbability,
)
from langchain.schema import ChatMessage
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
    get_image_from_url,
)
from khoj.utils import state
from khoj.utils.helpers import in_debug_mode, is_none_or_empty

logger = logging.getLogger(__name__)


MAX_OUTPUT_TOKENS_GEMINI = 8192


@retry(
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def gemini_completion_with_backoff(
    messages, system_prompt, model_name, temperature=0, api_key=None, model_kwargs=None, tracer={}
) -> str:
    genai.configure(api_key=api_key)
    model_kwargs = model_kwargs or dict()
    model_kwargs["temperature"] = temperature
    model_kwargs["max_output_tokens"] = MAX_OUTPUT_TOKENS_GEMINI
    model = genai.GenerativeModel(
        model_name,
        generation_config=model_kwargs,
        system_instruction=system_prompt,
        safety_settings={
            HarmCategory.HARM_CATEGORY_DANGEROUS_CONTENT: HarmBlockThreshold.BLOCK_ONLY_HIGH,
            HarmCategory.HARM_CATEGORY_HARASSMENT: HarmBlockThreshold.BLOCK_ONLY_HIGH,
            HarmCategory.HARM_CATEGORY_HATE_SPEECH: HarmBlockThreshold.BLOCK_ONLY_HIGH,
            HarmCategory.HARM_CATEGORY_SEXUALLY_EXPLICIT: HarmBlockThreshold.BLOCK_ONLY_HIGH,
        },
    )

    formatted_messages = [{"role": message.role, "parts": message.content} for message in messages]

    # Start chat session. All messages up to the last are considered to be part of the chat history
    chat_session = model.start_chat(history=formatted_messages[0:-1])

    try:
        # Generate the response. The last message is considered to be the current prompt
        response = chat_session.send_message(formatted_messages[-1]["parts"])
        response_text = response.text
    except StopCandidateException as e:
        response_text, _ = handle_gemini_response(e.args)
        # Respond with reason for stopping
        logger.warning(
            f"LLM Response Prevented for {model_name}: {response_text}.\n"
            + f"Last Message by {messages[-1].role}: {messages[-1].content}"
        )

    # Save conversation trace
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if in_debug_mode() or state.verbose > 1:
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
    system_prompt,
    completion_func=None,
    model_kwargs=None,
    tracer: dict = {},
):
    g = ThreadedGenerator(compiled_references, online_results, completion_func=completion_func)
    t = Thread(
        target=gemini_llm_thread,
        args=(g, messages, system_prompt, model_name, temperature, api_key, model_kwargs, tracer),
    )
    t.start()
    return g


def gemini_llm_thread(
    g, messages, system_prompt, model_name, temperature, api_key, model_kwargs=None, tracer: dict = {}
):
    try:
        genai.configure(api_key=api_key)
        model_kwargs = model_kwargs or dict()
        model_kwargs["temperature"] = temperature
        model_kwargs["max_output_tokens"] = MAX_OUTPUT_TOKENS_GEMINI
        model_kwargs["stop_sequences"] = ["Notes:\n["]
        model = genai.GenerativeModel(
            model_name,
            generation_config=model_kwargs,
            system_instruction=system_prompt,
            safety_settings={
                HarmCategory.HARM_CATEGORY_DANGEROUS_CONTENT: HarmBlockThreshold.BLOCK_ONLY_HIGH,
                HarmCategory.HARM_CATEGORY_HARASSMENT: HarmBlockThreshold.BLOCK_ONLY_HIGH,
                HarmCategory.HARM_CATEGORY_HATE_SPEECH: HarmBlockThreshold.BLOCK_ONLY_HIGH,
                HarmCategory.HARM_CATEGORY_SEXUALLY_EXPLICIT: HarmBlockThreshold.BLOCK_ONLY_HIGH,
            },
        )

        aggregated_response = ""
        formatted_messages = [{"role": message.role, "parts": message.content} for message in messages]

        # all messages up to the last are considered to be part of the chat history
        chat_session = model.start_chat(history=formatted_messages[0:-1])
        # the last message is considered to be the current prompt
        for chunk in chat_session.send_message(formatted_messages[-1]["parts"], stream=True):
            message, stopped = handle_gemini_response(chunk.candidates, chunk.prompt_feedback)
            message = message or chunk.text
            aggregated_response += message
            g.send(message)
            if stopped:
                raise StopCandidateException(message)

        # Save conversation trace
        tracer["chat_model"] = model_name
        tracer["temperature"] = temperature
        if in_debug_mode() or state.verbose > 1:
            commit_conversation_trace(messages, aggregated_response, tracer)
    except StopCandidateException as e:
        logger.warning(
            f"LLM Response Prevented for {model_name}: {e.args[0]}.\n"
            + f"Last Message by {messages[-1].role}: {messages[-1].content}"
        )
    except Exception as e:
        logger.error(f"Error in gemini_llm_thread: {e}", exc_info=True)
    finally:
        g.close()


def handle_gemini_response(candidates, prompt_feedback=None):
    """Check if Gemini response was blocked and return an explanatory error message."""
    # Check if the response was blocked due to safety concerns with the prompt
    if len(candidates) == 0 and prompt_feedback:
        message = f"\nI'd prefer to not respond to that due to **{prompt_feedback.block_reason.name}** issues with your query."
        stopped = True
    # Check if the response was blocked due to safety concerns with the generated content
    elif candidates[0].finish_reason == FinishReason.SAFETY:
        message = generate_safety_response(candidates[0].safety_ratings)
        stopped = True
    # Check if finish reason is empty, therefore generation is in progress
    elif not candidates[0].finish_reason:
        message = None
        stopped = False
    # Check if the response was stopped due to reaching maximum token limit or other reasons
    elif candidates[0].finish_reason != FinishReason.STOP:
        message = f"\nI can't talk further about that because of **{candidates[0].finish_reason.name} issue.**"
        stopped = True
    # Otherwise, the response is valid and can be used
    else:
        message = None
        stopped = False
    return message, stopped


def generate_safety_response(safety_ratings):
    """Generate a conversational response based on the safety ratings of the response."""
    # Get the safety rating with the highest probability
    max_safety_rating = sorted(safety_ratings, key=lambda x: x.probability, reverse=True)[0]
    # Remove the "HARM_CATEGORY_" prefix and title case the category name
    max_safety_category = " ".join(max_safety_rating.category.name.split("_")[2:]).title()
    # Add a bit of variety to the discomfort level based on the safety rating probability
    discomfort_level = {
        HarmProbability.HARM_PROBABILITY_UNSPECIFIED: " ",
        HarmProbability.LOW: "a bit ",
        HarmProbability.MEDIUM: "moderately ",
        HarmProbability.HIGH: random.choice(["very ", "quite ", "fairly "]),
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


def format_messages_for_gemini(messages: list[ChatMessage], system_prompt: str = None) -> tuple[list[str], str]:
    # Extract system message
    system_prompt = system_prompt or ""
    for message in messages.copy():
        if message.role == "system":
            system_prompt += message.content
            messages.remove(message)
    system_prompt = None if is_none_or_empty(system_prompt) else system_prompt

    for message in messages:
        # Convert message content to string list from chatml dictionary list
        if isinstance(message.content, list):
            # Convert image_urls to PIL.Image and place them at beginning of list (better for Gemini)
            message.content = [
                get_image_from_url(item["image_url"]["url"]).content
                if item["type"] == "image_url"
                else item.get("text", "")
                for item in sorted(message.content, key=lambda x: 0 if x["type"] == "image_url" else 1)
            ]
        elif isinstance(message.content, str):
            message.content = [message.content]

        if message.role == "assistant":
            message.role = "model"

    if len(messages) == 1:
        messages[0].role = "user"

    return messages, system_prompt
