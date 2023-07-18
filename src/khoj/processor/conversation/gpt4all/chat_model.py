from typing import Union, List
from khoj.processor.conversation import prompts
from khoj.utils.constants import empty_escape_sequences
from datetime import datetime
import logging
from gpt4all import GPT4All
from khoj.processor.conversation.utils import ThreadedGenerator
from threading import Thread

logger = logging.getLogger(__name__)


def extract_questions_falcon(
    text: str,
    model: str = "ggml-model-gpt4all-falcon-q4_0.bin",
    loaded_model: Union[GPT4All, None] = None,
    conversation_log={},
    use_history: bool = False,
    run_extraction: bool = False,
):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    all_questions = text.split("? ")
    all_questions = [q + "?" for q in all_questions[:-1]] + [all_questions[-1]]
    if not run_extraction:
        return all_questions

    gpt4all_model = loaded_model or GPT4All(model)

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = ""

    if use_history:
        chat_history = "".join(
            [
                f'Q: {chat["intent"]["query"]}\n\n{chat["intent"].get("inferred-queries") or list([chat["intent"]["query"]])}\n\nA: {chat["message"]}\n\n'
                for chat in conversation_log.get("chat", [])[-4:]
                if chat["by"] == "khoj"
            ]
        )

    prompt = prompts.extract_questions_falcon.format(
        chat_history=chat_history,
        text=text,
    )
    message = prompts.general_conversation_falcon.format(query=prompt)
    response = gpt4all_model.generate(message, max_tokens=200, top_k=2)

    # Extract, Clean Message from GPT's Response
    try:
        questions = (
            str(response)
            .strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("']", '"]')
            .replace("', '", '", "')
            .replace('["', "")
            .replace('"]', "")
            .split('", "')
        )
    except:
        logger.warning(f"Falcon returned invalid JSON. Falling back to using user message as search query.\n{response}")
        return all_questions
    logger.debug(f"Extracted Questions by Falcon: {questions}")
    questions.extend(all_questions)
    return questions


def converse_falcon(
    references,
    user_query,
    conversation_log={},
    model: str = "ggml-model-gpt4all-falcon-q4_0.bin",
    loaded_model: Union[GPT4All, None] = None,
    completion_func=None,
) -> ThreadedGenerator:
    """
    Converse with user using OpenAI's ChatGPT
    """
    gpt4all_model = loaded_model or GPT4All(model)
    # Initialize Variables
    current_date = datetime.now().strftime("%Y-%m-%d")
    compiled_references_message = "\n\n".join({f"### {item}" for item in references})

    # Get Conversation Primer appropriate to Conversation Type
    if compiled_references_message == "":
        conversation_primer = prompts.conversation_falcon.format(query=user_query)
    else:
        conversation_primer = prompts.notes_conversation.format(
            current_date=current_date, query=user_query, references=compiled_references_message
        )

    # Setup Prompt with Primer or Conversation History
    messages = generate_messages_with_history_falcon(
        conversation_primer,
        prompts.personality.format(),
        conversation_log,
    )
    truncated_messages = "\n".join({f"{message[:40]}..." for message in messages})
    logger.debug(f"Conversation Context for Falcon: {truncated_messages}")

    g = ThreadedGenerator(references, completion_func=completion_func)
    t = Thread(target=llm_thread, args=(g, messages, gpt4all_model))
    t.start()
    return g


def llm_thread(g, messages: List[str], model: GPT4All):
    message = "\n".join(messages)
    prompted_message = prompts.general_conversation_falcon.format(query=message)
    response_iterator = model.generate(
        prompted_message, streaming=True, max_tokens=256, top_k=1, temp=0, repeat_penalty=1.7
    )
    for response in response_iterator:
        # if response.strip(empty_escape_sequences) == "":
        # continue
        logger.info(response)
        g.send(response)
    g.close()


def generate_messages_with_history_falcon(user_message, system_message, conversation_log={}, lookback_turns=2):
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
        rest_backnforths.append(prompts.chat_history_falcon.format(user_msg=user_msg, assistant_msg=assistant_msg))

    # Format user and system messages to chatml format

    messages = [system_message] + rest_backnforths[::-1] + [user_message]
    # TODO Add truncation based on max prompt size

    # Return message in chronological order
    return messages
