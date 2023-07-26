# Standard Packages
import logging
from datetime import datetime
from typing import Optional

# External Packages
from langchain.schema import ChatMessage

# Internal Packages
from khoj.utils.constants import empty_escape_sequences
from khoj.processor.conversation import prompts
from khoj.processor.conversation.openai.utils import (
    chat_completion_with_backoff,
    completion_with_backoff,
)
from khoj.processor.conversation.utils import generate_chatml_messages_with_context


logger = logging.getLogger(__name__)


def summarize(session, model, api_key=None, temperature=0.5, max_tokens=200):
    """
    Summarize conversation session using the specified OpenAI chat model
    """
    messages = [ChatMessage(content=prompts.summarize_chat.format(), role="system")] + session

    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {messages}")
    response = completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=temperature,
        max_tokens=max_tokens,
        model_kwargs={"stop": ['"""'], "frequency_penalty": 0.2},
        openai_api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    return str(response.content).replace("\n\n", "")


def extract_questions(
    text, model: Optional[str] = "gpt-4", conversation_log={}, api_key=None, temperature=0, max_tokens=100
):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = "".join(
        [
            f'Q: {chat["intent"]["query"]}\n\n{chat["intent"].get("inferred-queries") or list([chat["intent"]["query"]])}\n\n{chat["message"]}\n\n'
            for chat in conversation_log.get("chat", [])[-4:]
            if chat["by"] == "khoj"
        ]
    )

    # Get dates relative to today for prompt creation
    today = datetime.today()
    current_new_year = today.replace(month=1, day=1)
    last_new_year = current_new_year.replace(year=today.year - 1)

    prompt = prompts.extract_questions.format(
        current_date=today.strftime("%A, %Y-%m-%d"),
        last_new_year=last_new_year.strftime("%Y"),
        last_new_year_date=last_new_year.strftime("%Y-%m-%d"),
        current_new_year_date=current_new_year.strftime("%Y-%m-%d"),
        bob_tom_age_difference={current_new_year.year - 1984 - 30},
        bob_age={current_new_year.year - 1984},
        chat_history=chat_history,
        text=text,
    )
    messages = [ChatMessage(content=prompt, role="assistant")]

    # Get Response from GPT
    response = completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=temperature,
        max_tokens=max_tokens,
        model_kwargs={"stop": ["A: ", "\n"]},
        openai_api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    try:
        questions = (
            response.content.strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("']", '"]')
            .replace("', '", '", "')
            .replace('["', "")
            .replace('"]', "")
            .split('", "')
        )
    except:
        logger.warning(f"GPT returned invalid JSON. Falling back to using user message as search query.\n{response}")
        questions = [text]
    logger.debug(f"Extracted Questions by GPT: {questions}")
    return questions


def converse(
    references,
    user_query,
    conversation_log={},
    model: str = "gpt-3.5-turbo",
    api_key: Optional[str] = None,
    temperature: float = 0.2,
    completion_func=None,
):
    """
    Converse with user using OpenAI's ChatGPT
    """
    # Initialize Variables
    current_date = datetime.now().strftime("%Y-%m-%d")
    compiled_references = "\n\n".join({f"# {item}" for item in references})

    # Get Conversation Primer appropriate to Conversation Type
    if compiled_references == "":
        conversation_primer = prompts.general_conversation.format(current_date=current_date, query=user_query)
    else:
        conversation_primer = prompts.notes_conversation.format(
            current_date=current_date, query=user_query, references=compiled_references
        )

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        prompts.personality.format(),
        conversation_log,
        model,
    )
    truncated_messages = "\n".join({f"{message.content[:40]}..." for message in messages})
    logger.debug(f"Conversation Context for GPT: {truncated_messages}")

    # Get Response from GPT
    return chat_completion_with_backoff(
        messages=messages,
        compiled_references=references,
        model_name=model,
        temperature=temperature,
        openai_api_key=api_key,
        completion_func=completion_func,
    )
