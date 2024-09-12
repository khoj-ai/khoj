import json
import logging
from datetime import datetime, timedelta
from threading import Thread
from typing import Any, Iterator, List, Union

from langchain.schema import ChatMessage
from llama_cpp import Llama

from khoj.database.models import Agent, ChatModelOptions, KhojUser
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
    model: str = "bartowski/Meta-Llama-3.1-8B-Instruct-GGUF",
    loaded_model: Union[Any, None] = None,
    conversation_log={},
    use_history: bool = True,
    should_extract_questions: bool = True,
    location_data: LocationData = None,
    user: KhojUser = None,
    max_prompt_size: int = None,
    temperature: float = 0.7,
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
    username = prompts.user_name.format(name=user.get_full_name()) if user and user.get_full_name() else ""

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = ""

    if use_history:
        for chat in conversation_log.get("chat", [])[-4:]:
            if chat["by"] == "khoj" and "text-to-image" not in chat["intent"].get("type"):
                chat_history += f"Q: {chat['intent']['query']}\n"
                chat_history += f"Khoj: {chat['message']}\n\n"

    # Get dates relative to today for prompt creation
    today = datetime.today()
    yesterday = (today - timedelta(days=1)).strftime("%Y-%m-%d")
    last_year = today.year - 1
    example_questions = prompts.extract_questions_offline.format(
        query=text,
        chat_history=chat_history,
        current_date=today.strftime("%Y-%m-%d"),
        day_of_week=today.strftime("%A"),
        current_month=today.strftime("%Y-%m"),
        yesterday_date=yesterday,
        last_year=last_year,
        this_year=today.year,
        location=location,
        username=username,
    )

    messages = generate_chatml_messages_with_context(
        example_questions,
        model_name=model,
        loaded_model=offline_chat_model,
        max_prompt_size=max_prompt_size,
        model_type=ChatModelOptions.ModelType.OFFLINE,
    )

    state.chat_lock.acquire()
    try:
        response = send_message_to_model_offline(
            messages,
            loaded_model=offline_chat_model,
            model=model,
            max_prompt_size=max_prompt_size,
            temperature=temperature,
            response_type="json_object",
        )
    finally:
        state.chat_lock.release()

    # Extract and clean the chat model's response
    try:
        response = response.strip(empty_escape_sequences)
        response = json.loads(response)
        questions = [q.strip() for q in response["queries"] if q.strip()]
        questions = filter_questions(questions)
    except:
        logger.warning(f"Llama returned invalid JSON. Falling back to using user message as search query.\n{response}")
        return all_questions
    logger.debug(f"Questions extracted by {model}: {questions}")
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
    model: str = "bartowski/Meta-Llama-3.1-8B-Instruct-GGUF",
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

    current_date = datetime.now()

    if agent and agent.personality:
        system_prompt = prompts.custom_system_prompt_offline_chat.format(
            name=agent.name,
            bio=agent.personality,
            current_date=current_date.strftime("%Y-%m-%d"),
            day_of_week=current_date.strftime("%A"),
        )
    else:
        system_prompt = prompts.system_prompt_offline_chat.format(
            current_date=current_date.strftime("%Y-%m-%d"),
            day_of_week=current_date.strftime("%A"),
        )

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

        conversation_primer = f"{prompts.online_search_conversation_offline.format(online_results=str(simplified_online_results))}\n{conversation_primer}"
    if not is_none_or_empty(compiled_references_message):
        conversation_primer = f"{prompts.notes_conversation_offline.format(references=compiled_references_message)}\n\n{conversation_primer}"

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        system_prompt,
        conversation_log,
        model_name=model,
        loaded_model=offline_chat_model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
        model_type=ChatModelOptions.ModelType.OFFLINE,
    )

    truncated_messages = "\n".join({f"{message.content[:70]}..." for message in messages})
    logger.debug(f"Conversation Context for {model}: {truncated_messages}")

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
    model="bartowski/Meta-Llama-3.1-8B-Instruct-GGUF",
    temperature: float = 0.2,
    streaming=False,
    stop=[],
    max_prompt_size: int = None,
    response_type: str = "text",
):
    assert loaded_model is None or isinstance(loaded_model, Llama), "loaded_model must be of type Llama, if configured"
    offline_chat_model = loaded_model or download_model(model, max_tokens=max_prompt_size)
    messages_dict = [{"role": message.role, "content": message.content} for message in messages]
    response = offline_chat_model.create_chat_completion(
        messages_dict, stop=stop, stream=streaming, temperature=temperature, response_format={"type": response_type}
    )
    if streaming:
        return response
    else:
        return response["choices"][0]["message"].get("content", "")
