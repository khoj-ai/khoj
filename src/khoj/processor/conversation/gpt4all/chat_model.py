from typing import Union, List
from datetime import datetime
import logging
from threading import Thread

from langchain.schema import ChatMessage

from gpt4all import GPT4All

from khoj.processor.conversation.utils import ThreadedGenerator, generate_chatml_messages_with_context
from khoj.processor.conversation import prompts
from khoj.utils.constants import empty_escape_sequences

logger = logging.getLogger(__name__)


def extract_questions_offline(
    text: str,
    model: str = "llama-2-7b-chat.ggmlv3.q4_K_S.bin",
    loaded_model: Union[GPT4All, None] = None,
    conversation_log={},
    use_history: bool = True,
    should_extract_questions: bool = True,
):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    all_questions = text.split("? ")
    all_questions = [q + "?" for q in all_questions[:-1]] + [all_questions[-1]]

    if not should_extract_questions:
        return all_questions

    gpt4all_model = loaded_model or GPT4All(model)

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = ""

    if use_history:
        for chat in conversation_log.get("chat", [])[-4:]:
            if chat["by"] == "khoj":
                chat_history += f"Q: {chat['intent']['query']}\n"
                chat_history += f"A: {chat['message']}\n"

    current_date = datetime.now().strftime("%Y-%m-%d")
    last_year = datetime.now().year - 1
    last_christmas_date = f"{last_year}-12-25"
    next_christmas_date = f"{datetime.now().year}-12-25"
    system_prompt = prompts.extract_questions_system_prompt_llamav2.format(
        message=(prompts.system_prompt_message_extract_questions_llamav2)
    )
    example_questions = prompts.extract_questions_llamav2_sample.format(
        query=text,
        chat_history=chat_history,
        current_date=current_date,
        last_year=last_year,
        last_christmas_date=last_christmas_date,
        next_christmas_date=next_christmas_date,
    )
    message = system_prompt + example_questions
    response = gpt4all_model.generate(message, max_tokens=200, top_k=2, temp=0)

    # Extract, Clean Message from GPT's Response
    try:
        # This will expect to be a list with a single string with a list of questions
        questions = (
            str(response)
            .strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("<s>", "")
            .replace("</s>", "")
            .replace("']", '"]')
            .replace("', '", '", "')
            .replace('["', "")
            .replace('"]', "")
            .split("? ")
        )
        questions = [q + "?" for q in questions[:-1]] + [questions[-1]]
        questions = filter_questions(questions)
    except:
        logger.warning(f"Llama returned invalid JSON. Falling back to using user message as search query.\n{response}")
        return all_questions
    logger.debug(f"Extracted Questions by Llama: {questions}")
    questions.extend(all_questions)
    return questions


def filter_questions(questions: List[str]):
    # Skip questions that seem to be apologizing for not being able to answer the question
    hint_words = [
        "sorry",
        "apologize",
        "unable",
        "can't",
        "cannot",
        "don't know",
        "don't understand",
        "do not know",
        "do not understand",
    ]
    filtered_questions = []
    for q in questions:
        if not any([word in q.lower() for word in hint_words]):
            filtered_questions.append(q)

    return filtered_questions


def converse_offline(
    references,
    user_query,
    conversation_log={},
    model: str = "llama-2-7b-chat.ggmlv3.q4_K_S.bin",
    loaded_model: Union[GPT4All, None] = None,
    completion_func=None,
) -> ThreadedGenerator:
    """
    Converse with user using Llama
    """
    gpt4all_model = loaded_model or GPT4All(model)
    # Initialize Variables
    current_date = datetime.now().strftime("%Y-%m-%d")
    compiled_references_message = "\n\n".join({f"{item}" for item in references})

    # Get Conversation Primer appropriate to Conversation Type
    # TODO If compiled_references_message is too long, we need to truncate it.
    if compiled_references_message == "":
        conversation_primer = prompts.conversation_llamav2.format(query=user_query)
    else:
        conversation_primer = prompts.notes_conversation_llamav2.format(
            query=user_query, references=compiled_references_message
        )

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        prompts.system_prompt_message_llamav2,
        conversation_log,
        model_name=model,
    )

    g = ThreadedGenerator(references, completion_func=completion_func)
    t = Thread(target=llm_thread, args=(g, messages, gpt4all_model))
    t.start()
    return g


def llm_thread(g, messages: List[ChatMessage], model: GPT4All):
    user_message = messages[-1]
    system_message = messages[0]
    conversation_history = messages[1:-1]

    formatted_messages = [
        prompts.chat_history_llamav2_from_assistant.format(message=message.content)
        if message.role == "assistant"
        else prompts.chat_history_llamav2_from_user.format(message=message.content)
        for message in conversation_history
    ]

    chat_history = "".join(formatted_messages)
    templated_system_message = prompts.system_prompt_llamav2.format(message=system_message.content)
    templated_user_message = prompts.general_conversation_llamav2.format(query=user_message.content)
    prompted_message = templated_system_message + chat_history + templated_user_message
    response_iterator = model.generate(prompted_message, streaming=True, max_tokens=2000)
    for response in response_iterator:
        logger.info(response)
        g.send(response)
    g.close()
