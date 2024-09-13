import logging
from threading import Thread

import google.generativeai as genai
from tenacity import (
    before_sleep_log,
    retry,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

from khoj.processor.conversation.utils import ThreadedGenerator

logger = logging.getLogger(__name__)


DEFAULT_MAX_TOKENS_GEMINI = 8192


@retry(
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(2),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def gemini_completion_with_backoff(
    messages, system_prompt, model_name, temperature=0, api_key=None, model_kwargs=None, max_tokens=None
) -> str:
    genai.configure(api_key=api_key)
    max_tokens = max_tokens or DEFAULT_MAX_TOKENS_GEMINI
    model_kwargs = model_kwargs or dict()
    model_kwargs["temperature"] = temperature
    model_kwargs["max_output_tokens"] = max_tokens
    model = genai.GenerativeModel(model_name, generation_config=model_kwargs, system_instruction=system_prompt)

    formatted_messages = [{"role": message.role, "parts": [message.content]} for message in messages]
    # all messages up to the last are considered to be part of the chat history
    chat_session = model.start_chat(history=formatted_messages[0:-1])
    # the last message is considered to be the current prompt
    aggregated_response = chat_session.send_message(formatted_messages[-1]["parts"][0])
    return aggregated_response.text


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
    max_prompt_size=None,
    completion_func=None,
    model_kwargs=None,
):
    g = ThreadedGenerator(compiled_references, online_results, completion_func=completion_func)
    t = Thread(
        target=gemini_llm_thread,
        args=(g, messages, system_prompt, model_name, temperature, api_key, max_prompt_size, model_kwargs),
    )
    t.start()
    return g


def gemini_llm_thread(
    g, messages, system_prompt, model_name, temperature, api_key, max_prompt_size=None, model_kwargs=None
):
    try:
        genai.configure(api_key=api_key)
        max_tokens = max_prompt_size or DEFAULT_MAX_TOKENS_GEMINI
        model_kwargs = model_kwargs or dict()
        model_kwargs["temperature"] = temperature
        model_kwargs["max_output_tokens"] = max_tokens
        model_kwargs["stop_sequences"] = ["Notes:\n["]
        model = genai.GenerativeModel(model_name, generation_config=model_kwargs, system_instruction=system_prompt)

        formatted_messages = [{"role": message.role, "parts": [message.content]} for message in messages]
        # all messages up to the last are considered to be part of the chat history
        chat_session = model.start_chat(history=formatted_messages[0:-1])
        # the last message is considered to be the current prompt
        for chunk in chat_session.send_message(formatted_messages[-1]["parts"][0], stream=True):
            g.send(chunk.text)
    except Exception as e:
        logger.error(f"Error in gemini_llm_thread: {e}", exc_info=True)
    finally:
        g.close()
