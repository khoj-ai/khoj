# Standard Packages
import os
import logging
from datetime import datetime

# External Packages
from langchain.chat_models import ChatOpenAI
from langchain.llms import OpenAI
from langchain.schema import ChatMessage
import openai
import tiktoken
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_exponential,
    wait_random_exponential,
)

# Internal Packages
from khoj.utils.helpers import merge_dicts


logger = logging.getLogger(__name__)
max_prompt_size = {"gpt-3.5-turbo": 4096, "gpt-4": 8192}


@retry(
    retry=(
        retry_if_exception_type(openai.error.Timeout)
        | retry_if_exception_type(openai.error.APIError)
        | retry_if_exception_type(openai.error.APIConnectionError)
        | retry_if_exception_type(openai.error.RateLimitError)
        | retry_if_exception_type(openai.error.ServiceUnavailableError)
    ),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def completion_with_backoff(**kwargs):
    prompt = kwargs.pop("prompt")
    if "api_key" in kwargs:
        kwargs["openai_api_key"] = kwargs.get("api_key")
    else:
        kwargs["openai_api_key"] = os.getenv("OPENAI_API_KEY")
    llm = OpenAI(**kwargs, request_timeout=20, max_retries=1)
    return llm(prompt)


@retry(
    retry=(
        retry_if_exception_type(openai.error.Timeout)
        | retry_if_exception_type(openai.error.APIError)
        | retry_if_exception_type(openai.error.APIConnectionError)
        | retry_if_exception_type(openai.error.RateLimitError)
        | retry_if_exception_type(openai.error.ServiceUnavailableError)
    ),
    wait=wait_exponential(multiplier=1, min=4, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def chat_completion_with_backoff(messages, model_name, temperature, openai_api_key=None):
    chat = ChatOpenAI(
        model_name=model_name,
        temperature=temperature,
        openai_api_key=openai_api_key or os.getenv("OPENAI_API_KEY"),
        request_timeout=10,
        max_retries=1,
    )
    return chat(messages).content


def generate_chatml_messages_with_context(
    user_message, system_message, conversation_log={}, model_name="gpt-3.5-turbo", lookback_turns=2
):
    """Generate messages for ChatGPT with context from previous conversation"""
    # Extract Chat History for Context
    chat_logs = []
    for chat in conversation_log.get("chat", []):
        chat_notes = f'\n\n Notes:\n{chat.get("context")}' if chat.get("context") else "\n"
        chat_logs += [chat["message"] + chat_notes]

    rest_backnforths = []
    # Extract in reverse chronological order
    for user_msg, assistant_msg in zip(chat_logs[-2::-2], chat_logs[::-2]):
        if len(rest_backnforths) >= 2 * lookback_turns:
            break
        rest_backnforths += reciprocal_conversation_to_chatml([user_msg, assistant_msg])[::-1]

    # Format user and system messages to chatml format
    system_chatml_message = [ChatMessage(content=system_message, role="system")]
    user_chatml_message = [ChatMessage(content=user_message, role="user")]

    messages = user_chatml_message + rest_backnforths + system_chatml_message

    # Truncate oldest messages from conversation history until under max supported prompt size by model
    messages = truncate_messages(messages, max_prompt_size[model_name], model_name)

    # Return message in chronological order
    return messages[::-1]


def truncate_messages(messages, max_prompt_size, model_name):
    """Truncate messages to fit within max prompt size supported by model"""
    encoder = tiktoken.encoding_for_model(model_name)
    tokens = sum([len(encoder.encode(message.content)) for message in messages])
    while tokens > max_prompt_size and len(messages) > 1:
        messages.pop()
        tokens = sum([len(encoder.encode(message.content)) for message in messages])

    # Truncate last message if still over max supported prompt size by model
    if tokens > max_prompt_size:
        last_message = "\n".join(messages[-1].content.split("\n")[:-1])
        original_question = "\n".join(messages[-1].content.split("\n")[-1:])
        original_question_tokens = len(encoder.encode(original_question))
        remaining_tokens = max_prompt_size - original_question_tokens
        truncated_message = encoder.decode(encoder.encode(last_message)[:remaining_tokens]).strip()
        logger.debug(
            f"Truncate last message to fit within max prompt size of {max_prompt_size} supported by {model_name} model:\n {truncated_message}"
        )
        messages = [ChatMessage(content=truncated_message + original_question, role=messages[-1].role)]

    return messages


def reciprocal_conversation_to_chatml(message_pair):
    """Convert a single back and forth between user and assistant to chatml format"""
    return [ChatMessage(content=message, role=role) for message, role in zip(message_pair, ["user", "assistant"])]


def message_to_prompt(
    user_message, conversation_history="", gpt_message=None, start_sequence="\nAI:", restart_sequence="\nHuman:"
):
    """Create prompt for GPT from messages and conversation history"""
    gpt_message = f" {gpt_message}" if gpt_message else ""

    return f"{conversation_history}{restart_sequence} {user_message}{start_sequence}{gpt_message}"


def message_to_log(user_message, gpt_message, user_message_metadata={}, khoj_message_metadata={}, conversation_log=[]):
    """Create json logs from messages, metadata for conversation log"""
    default_khoj_message_metadata = {
        "intent": {"type": "remember", "memory-type": "notes", "query": user_message},
        "trigger-emotion": "calm",
    }
    khoj_response_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Create json log from Human's message
    human_log = merge_dicts({"message": user_message, "by": "you"}, user_message_metadata)

    # Create json log from GPT's response
    khoj_log = merge_dicts(khoj_message_metadata, default_khoj_message_metadata)
    khoj_log = merge_dicts({"message": gpt_message, "by": "khoj", "created": khoj_response_time}, khoj_log)

    conversation_log.extend([human_log, khoj_log])
    return conversation_log


def extract_summaries(metadata):
    """Extract summaries from metadata"""
    return "".join([f'\n{session["summary"]}' for session in metadata])
