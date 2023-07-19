from typing import Union, List
from datetime import datetime
import logging
from gpt4all import GPT4All
from threading import Thread
from langchain.schema import ChatMessage


from khoj.processor.conversation.utils import ThreadedGenerator, generate_chatml_messages_with_context
from khoj.processor.conversation import prompts
from khoj.utils.constants import empty_escape_sequences

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
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        prompts.personality.format(),
        conversation_log,
        model_name="text-davinci-003",  # This isn't actually the model, but this helps us get an approximate encoding to run message truncation.
    )

    g = ThreadedGenerator(references, completion_func=completion_func)
    t = Thread(target=llm_thread, args=(g, messages, gpt4all_model))
    t.start()
    return g


def llm_thread(g, messages: List[ChatMessage], model: GPT4All):
    user_message = messages[0]
    system_message = messages[-1]
    conversation_history = messages[1:-1]

    formatted_messages = [
        prompts.chat_history_falcon_from_assistant.format(message=system_message)
        if message.role == "assistant"
        else prompts.chat_history_falcon_from_user.format(message=message.content)
        for message in conversation_history
    ]

    chat_history = "".join(formatted_messages)
    full_message = system_message.content + chat_history + user_message.content

    prompted_message = prompts.general_conversation_falcon.format(query=full_message)
    response_iterator = model.generate(
        prompted_message, streaming=True, max_tokens=256, top_k=1, temp=0, repeat_penalty=1.7
    )
    for response in response_iterator:
        logger.info(response)
        g.send(response)
    g.close()
