import json
import logging
from datetime import datetime, timedelta
from threading import Thread
from typing import Any, Iterator, List, Union

from langchain.schema import ChatMessage
from llama_cpp import Llama

from khoj.database.models import Agent
from khoj.processor.conversation import prompts
from khoj.processor.conversation.offline.utils import download_model
from khoj.processor.conversation.utils import (
    ThreadedGenerator,
    generate_chatml_messages_with_context,
)
from khoj.utils import state
from khoj.utils.constants import empty_escape_sequences
from khoj.utils.helpers import ConversationCommand, is_none_or_empty
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


def extract_questions_offline(
    text: str,
    model: str = "NousResearch/Hermes-2-Pro-Mistral-7B-GGUF",
    loaded_model: Union[Any, None] = None,
    conversation_log={},
    use_history: bool = True,
    should_extract_questions: bool = True,
    location_data: LocationData = None,
    max_prompt_size: int = None,
) -> List[str]:
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    all_questions = text.split("? ")
    all_questions = [q + "?" for q in all_questions[:-1]] + [all_questions[-1]]

    if not should_extract_questions:
        return all_questions

    assert loaded_model is None or isinstance(loaded_model, Llama), "loaded_model must be of type Llama, if configured"
    offline_chat_model = loaded_model or download_model(model, max_tokens=max_prompt_size)

    location = f"{location_data.city}, {location_data.region}, {location_data.country}" if location_data else "Unknown"

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = ""

    if use_history:
        for chat in conversation_log.get("chat", [])[-4:]:
            if chat["by"] == "khoj" and "text-to-image" not in chat["intent"].get("type"):
                chat_history += f"Q: {chat['intent']['query']}\n"
                chat_history += f"Khoj: {chat['message']}\n\n"

    today = datetime.today()
    yesterday = (today - timedelta(days=1)).strftime("%Y-%m-%d")
    last_year = today.year - 1
    example_questions = prompts.extract_questions_offline.format(
        query=text,
        chat_history=chat_history,
        current_date=today.strftime("%Y-%m-%d"),
        yesterday_date=yesterday,
        last_year=last_year,
        this_year=today.year,
        location=location,
    )
    messages = generate_chatml_messages_with_context(
        example_questions, model_name=model, loaded_model=offline_chat_model, max_prompt_size=max_prompt_size
    )

    state.chat_lock.acquire()
    try:
        response = send_message_to_model_offline(
            messages, loaded_model=offline_chat_model, max_prompt_size=max_prompt_size
        )
    finally:
        state.chat_lock.release()

    # Extract, Clean Message from GPT's Response
    try:
        # This will expect to be a list with a single string with a list of questions
        questions_str = (
            str(response)
            .strip(empty_escape_sequences)
            .replace("['", '["')
            .replace("<s>", "")
            .replace("</s>", "")
            .replace("']", '"]')
            .replace("', '", '", "')
        )
        questions: List[str] = json.loads(questions_str)
        questions = filter_questions(questions)
    except:
        logger.warning(f"Llama returned invalid JSON. Falling back to using user message as search query.\n{response}")
        return all_questions
    logger.debug(f"Extracted Questions by Llama: {questions}")
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
    filtered_questions = set()
    for q in questions:
        if not any([word in q.lower() for word in hint_words]) and not is_none_or_empty(q):
            filtered_questions.add(q)

    return list(filtered_questions)


def converse_offline(
    user_query,
    references=[],
    online_results=[],
    conversation_log={},
    model: str = "NousResearch/Hermes-2-Pro-Mistral-7B-GGUF",
    loaded_model: Union[Any, None] = None,
    completion_func=None,
    conversation_commands=[ConversationCommand.Default],
    max_prompt_size=None,
    tokenizer_name=None,
    location_data: LocationData = None,
    user_name: str = None,
    agent: Agent = None,
) -> Union[ThreadedGenerator, Iterator[str]]:
    """
    Converse with user using Llama
    """
    # Initialize Variables
    assert loaded_model is None or isinstance(loaded_model, Llama), "loaded_model must be of type Llama, if configured"
    offline_chat_model = loaded_model or download_model(model, max_tokens=max_prompt_size)
    compiled_references_message = "\n\n".join({f"{item['compiled']}" for item in references})

    current_date = datetime.now().strftime("%Y-%m-%d")

    if agent and agent.personality:
        system_prompt = prompts.custom_system_prompt_offline_chat.format(
            name=agent.name, bio=agent.personality, current_date=current_date
        )
    else:
        system_prompt = prompts.system_prompt_offline_chat.format(current_date=current_date)

    conversation_primer = prompts.query_prompt.format(query=user_query)

    if location_data:
        location = f"{location_data.city}, {location_data.region}, {location_data.country}"
        location_prompt = prompts.user_location.format(location=location)
        system_prompt = f"{system_prompt}\n{location_prompt}"

    if user_name:
        user_name_prompt = prompts.user_name.format(name=user_name)
        system_prompt = f"{system_prompt}\n{user_name_prompt}"

    # Get Conversation Primer appropriate to Conversation Type
    if conversation_commands == [ConversationCommand.Notes] and is_none_or_empty(compiled_references_message):
        return iter([prompts.no_notes_found.format()])
    elif conversation_commands == [ConversationCommand.Online] and is_none_or_empty(online_results):
        completion_func(chat_response=prompts.no_online_results_found.format())
        return iter([prompts.no_online_results_found.format()])

    if ConversationCommand.Online in conversation_commands:
        simplified_online_results = online_results.copy()
        for result in online_results:
            if online_results[result].get("webpages"):
                simplified_online_results[result] = online_results[result]["webpages"]

        conversation_primer = f"{prompts.online_search_conversation.format(online_results=str(simplified_online_results))}\n{conversation_primer}"
    if not is_none_or_empty(compiled_references_message):
        conversation_primer = f"{prompts.notes_conversation_offline.format(references=compiled_references_message)}\n{conversation_primer}"

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        system_prompt,
        conversation_log,
        model_name=model,
        loaded_model=offline_chat_model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
    )

    g = ThreadedGenerator(references, online_results, completion_func=completion_func)
    t = Thread(target=llm_thread, args=(g, messages, offline_chat_model, max_prompt_size))
    t.start()
    return g


def llm_thread(g, messages: List[ChatMessage], model: Any, max_prompt_size: int = None):
    stop_phrases = ["<s>", "INST]", "Notes:"]

    state.chat_lock.acquire()
    try:
        response_iterator = send_message_to_model_offline(
            messages, loaded_model=model, stop=stop_phrases, max_prompt_size=max_prompt_size, streaming=True
        )
        for response in response_iterator:
            g.send(response["choices"][0]["delta"].get("content", ""))
    finally:
        state.chat_lock.release()
    g.close()


def send_message_to_model_offline(
    messages: List[ChatMessage],
    loaded_model=None,
    model="NousResearch/Hermes-2-Pro-Mistral-7B-GGUF",
    streaming=False,
    stop=[],
    max_prompt_size: int = None,
):
    assert loaded_model is None or isinstance(loaded_model, Llama), "loaded_model must be of type Llama, if configured"
    offline_chat_model = loaded_model or download_model(model, max_tokens=max_prompt_size)
    messages_dict = [{"role": message.role, "content": message.content} for message in messages]
    response = offline_chat_model.create_chat_completion(messages_dict, stop=stop, stream=streaming)
    if streaming:
        return response
    else:
        return response["choices"][0]["message"].get("content", "")
