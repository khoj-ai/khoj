# Standard Packages
import json
import logging
from datetime import datetime

# Internal Packages
from khoj.utils.constants import empty_escape_sequences
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import (
    chat_completion_with_backoff,
    completion_with_backoff,
    generate_chatml_messages_with_context,
)


logger = logging.getLogger(__name__)


def answer(text, user_query, model, api_key=None, temperature=0.5, max_tokens=500):
    """
    Answer user query using provided text as reference with OpenAI's GPT
    """
    # Setup Prompt from arguments
    prompt = prompts.answer.format(text=text, user_query=user_query)

    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {prompt}")
    response = completion_with_backoff(
        prompt=prompt,
        model_name=model,
        temperature=temperature,
        max_tokens=max_tokens,
        stop='"""',
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    return str(response).replace("\n\n", "")


def summarize(text, summary_type, model, user_query=None, api_key=None, temperature=0.5, max_tokens=200):
    """
    Summarize user input using OpenAI's GPT
    """
    # Setup Prompt based on Summary Type
    if summary_type == "chat":
        prompt = prompts.summarize_chat.format(text=text)
    elif summary_type == "notes":
        prompt = prompts.summarize_notes.format(text=text, user_query=user_query)

    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {prompt}")
    response = completion_with_backoff(
        prompt=prompt,
        model_name=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop='"""',
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    return str(response).replace("\n\n", "")


def extract_questions(text, model="text-davinci-003", conversation_log={}, api_key=None, temperature=0, max_tokens=100):
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

    # Get Response from GPT
    response = completion_with_backoff(
        prompt=prompt,
        model_name=model,
        temperature=temperature,
        max_tokens=max_tokens,
        stop=["A: ", "\n"],
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    try:
        questions = json.loads(
            # Clean response to increase likelihood of valid JSON. E.g replace ' with " to enclose strings
            response.strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("']", '"]')
            .replace("', '", '", "')
        )
    except json.decoder.JSONDecodeError:
        logger.warn(f"GPT returned invalid JSON. Falling back to using user message as search query.\n{response}")
        questions = [text]
    logger.debug(f"Extracted Questions by GPT: {questions}")
    return questions


def extract_search_type(text, model, api_key=None, temperature=0.5, max_tokens=100, verbose=0):
    """
    Extract search type from user query using OpenAI's GPT
    """
    # Setup Prompt to extract search type
    prompt = prompts.search_type + f"{text}\nA:"
    if verbose > 1:
        print(f"Message -> Prompt: {text} -> {prompt}")

    # Get Response from GPT
    logger.debug(f"Prompt for GPT: {prompt}")
    response = completion_with_backoff(
        prompt=prompt,
        model_name=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop=["\n"],
        api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    return json.loads(response.strip(empty_escape_sequences))


def converse(references, user_query, conversation_log={}, model="gpt-3.5-turbo", api_key=None, temperature=0.2):
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

    # Get Response from GPT
    logger.debug(f"Conversation Context for GPT: {messages}")
    response = chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=temperature,
        openai_api_key=api_key,
    )

    # Extract, Clean Message from GPT's Response
    return response.strip(empty_escape_sequences)
