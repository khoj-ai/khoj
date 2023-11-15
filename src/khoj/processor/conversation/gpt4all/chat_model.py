from typing import Iterator, Union, List, Any
from datetime import datetime
import logging
from threading import Thread

from langchain.schema import ChatMessage

from khoj.processor.conversation.utils import ThreadedGenerator, generate_chatml_messages_with_context
from khoj.processor.conversation import prompts
from khoj.utils.constants import empty_escape_sequences
from khoj.utils import state
from khoj.utils.helpers import ConversationCommand, is_none_or_empty

logger = logging.getLogger(__name__)


def extract_questions_offline(
    text: str,
    model: str = "mistral-7b-instruct-v0.1.Q4_0.gguf",
    loaded_model: Union[Any, None] = None,
    conversation_log={},
    use_history: bool = True,
    should_extract_questions: bool = True,
) -> List[str]:
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    try:
        from gpt4all import GPT4All
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    # Assert that loaded_model is either None or of type GPT4All
    assert loaded_model is None or isinstance(loaded_model, GPT4All), "loaded_model must be of type GPT4All or None"

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
    system_prompt = prompts.system_prompt_extract_questions_gpt4all.format(
        message=(prompts.system_prompt_message_extract_questions_gpt4all)
    )
    example_questions = prompts.extract_questions_gpt4all_sample.format(
        query=text,
        chat_history=chat_history,
        current_date=current_date,
        last_year=last_year,
        last_christmas_date=last_christmas_date,
        next_christmas_date=next_christmas_date,
    )
    message = system_prompt + example_questions
    state.chat_lock.acquire()
    try:
        response = gpt4all_model.generate(message, max_tokens=200, top_k=2, temp=0, n_batch=512)
    finally:
        state.chat_lock.release()

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
        if not any([word in q.lower() for word in hint_words]) and not is_none_or_empty(q):
            filtered_questions.append(q)

    return filtered_questions


def converse_offline(
    references,
    user_query,
    conversation_log={},
    model: str = "mistral-7b-instruct-v0.1.Q4_0.gguf",
    loaded_model: Union[Any, None] = None,
    completion_func=None,
    conversation_command=ConversationCommand.Default,
    max_prompt_size=None,
    tokenizer_name=None,
) -> Union[ThreadedGenerator, Iterator[str]]:
    """
    Converse with user using Llama
    """
    try:
        from gpt4all import GPT4All
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    assert loaded_model is None or isinstance(loaded_model, GPT4All), "loaded_model must be of type GPT4All or None"
    gpt4all_model = loaded_model or GPT4All(model)
    # Initialize Variables
    compiled_references_message = "\n\n".join({f"{item}" for item in references})

    # Get Conversation Primer appropriate to Conversation Type
    if conversation_command == ConversationCommand.Notes and is_none_or_empty(compiled_references_message):
        return iter([prompts.no_notes_found.format()])
    elif conversation_command == ConversationCommand.General or is_none_or_empty(compiled_references_message):
        conversation_primer = user_query
    else:
        conversation_primer = prompts.notes_conversation_gpt4all.format(
            query=user_query, references=compiled_references_message
        )

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        prompts.system_prompt_message_gpt4all,
        conversation_log,
        model_name=model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
    )

    g = ThreadedGenerator(references, completion_func=completion_func)
    t = Thread(target=llm_thread, args=(g, messages, gpt4all_model))
    t.start()
    return g


def llm_thread(g, messages: List[ChatMessage], model: Any):
    try:
        from gpt4all import GPT4All
    except ModuleNotFoundError as e:
        logger.info("There was an error importing GPT4All. Please run pip install gpt4all in order to install it.")
        raise e

    assert isinstance(model, GPT4All), "model should be of type GPT4All"
    user_message = messages[-1]
    system_message = messages[0]
    conversation_history = messages[1:-1]

    formatted_messages = [
        prompts.khoj_message_gpt4all.format(message=message.content)
        if message.role == "assistant"
        else prompts.user_message_gpt4all.format(message=message.content)
        for message in conversation_history
    ]

    stop_words = ["<s>"]
    chat_history = "".join(formatted_messages)
    templated_system_message = prompts.system_prompt_gpt4all.format(message=system_message.content)
    templated_user_message = prompts.user_message_gpt4all.format(message=user_message.content)
    prompted_message = templated_system_message + chat_history + templated_user_message

    state.chat_lock.acquire()
    response_iterator = model.generate(prompted_message, streaming=True, max_tokens=500, n_batch=512)
    try:
        for response in response_iterator:
            if any(stop_word in response.strip() for stop_word in stop_words):
                logger.debug(f"Stop response as hit stop word in {response}")
                break
            g.send(response)
    finally:
        state.chat_lock.release()
    g.close()
