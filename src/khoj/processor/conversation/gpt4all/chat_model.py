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


def extract_questions_falcon(
    text: str,
    model: str = "llama-2-7b-chat.ggmlv3.q4_K_S.bin",
    loaded_model: Union[GPT4All, None] = None,
    conversation_log={},
    use_history: bool = False,
):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    all_questions = text.split("? ")
    all_questions = [q + "?" for q in all_questions[:-1]] + [all_questions[-1]]

    gpt4all_model = loaded_model or GPT4All(model)

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = ""

    if use_history:
        for chat in conversation_log.get("chat", [])[-4:]:
            if chat["by"] == "khoj":
                chat_history += prompts.chat_history_llamav2_from_user.format(message=chat["intent"]["query"])
                if chat["intent"].get("inferred-queries"):
                    chat_history += prompts.chat_history_llamav2_from_assistant.format(
                        message=chat["intent"]["inferred-queries"]
                    )
                else:
                    chat_history += prompts.chat_history_llamav2_from_assistant.format(message=chat["intent"]["query"])

    message = prompts.system_prompt_llamav2.format(
        message=(prompts.system_prompt_message_extract_questions_llamav2)
    ) + prompts.extract_questions_llamav2_sample.format(query=text, chat_history=chat_history)
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
    model: str = "llama-2-7b-chat.ggmlv3.q4_K_S.bin",
    loaded_model: Union[GPT4All, None] = None,
    completion_func=None,
) -> ThreadedGenerator:
    """
    Converse with user using Falcon
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
