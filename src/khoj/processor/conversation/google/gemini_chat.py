import json
import logging
import re
from datetime import datetime, timedelta
from typing import Dict, Optional

from langchain.schema import ChatMessage

from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.google.utils import (
    gemini_chat_completion_with_backoff,
    gemini_completion_with_backoff,
)
from khoj.processor.conversation.utils import generate_chatml_messages_with_context
from khoj.utils.helpers import ConversationCommand, is_none_or_empty
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


def extract_questions_gemini(
    text,
    model: Optional[str] = "gemini-1.5-flash",
    conversation_log={},
    api_key=None,
    temperature=0,
    max_tokens=None,
    location_data: LocationData = None,
    user: KhojUser = None,
):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    # Extract Past User Message and Inferred Questions from Conversation Log
    location = f"{location_data.city}, {location_data.region}, {location_data.country}" if location_data else "Unknown"
    username = prompts.user_name.format(name=user.get_full_name()) if user and user.get_full_name() else ""

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = "".join(
        [
            f'User: {chat["intent"]["query"]}\nAssistant: {{"queries": {chat["intent"].get("inferred-queries") or list([chat["intent"]["query"]])}}}\nA: {chat["message"]}\n\n'
            for chat in conversation_log.get("chat", [])[-4:]
            if chat["by"] == "khoj" and "text-to-image" not in chat["intent"].get("type")
        ]
    )

    # Get dates relative to today for prompt creation
    today = datetime.today()
    current_new_year = today.replace(month=1, day=1)
    last_new_year = current_new_year.replace(year=today.year - 1)

    system_prompt = prompts.extract_questions_anthropic_system_prompt.format(
        current_date=today.strftime("%Y-%m-%d"),
        day_of_week=today.strftime("%A"),
        current_month=today.strftime("%Y-%m"),
        last_new_year=last_new_year.strftime("%Y"),
        last_new_year_date=last_new_year.strftime("%Y-%m-%d"),
        current_new_year_date=current_new_year.strftime("%Y-%m-%d"),
        yesterday_date=(today - timedelta(days=1)).strftime("%Y-%m-%d"),
        location=location,
        username=username,
    )

    prompt = prompts.extract_questions_anthropic_user_message.format(
        chat_history=chat_history,
        text=text,
    )

    messages = [ChatMessage(content=prompt, role="user")]

    model_kwargs = {"response_mime_type": "application/json"}

    response = gemini_completion_with_backoff(
        messages=messages,
        system_prompt=system_prompt,
        model_name=model,
        temperature=temperature,
        api_key=api_key,
        max_tokens=max_tokens,
        model_kwargs=model_kwargs,
    )

    # Extract, Clean Message from Gemini's Response
    try:
        response = response.strip()
        match = re.search(r"\{.*?\}", response)
        if match:
            response = match.group()
        response = json.loads(response)
        response = [q.strip() for q in response["queries"] if q.strip()]
        if not isinstance(response, list) or not response:
            logger.error(f"Invalid response for constructing subqueries: {response}")
            return [text]
        return response
    except:
        logger.warning(f"Gemini returned invalid JSON. Falling back to using user message as search query.\n{response}")
        questions = [text]
    logger.debug(f"Extracted Questions by Gemini: {questions}")
    return questions


def gemini_send_message_to_model(messages, api_key, model, response_type="text"):
    """
    Send message to model
    """
    system_prompt = None
    if len(messages) == 1:
        messages[0].role = "user"
    else:
        system_prompt = ""
        for message in messages.copy():
            if message.role == "system":
                system_prompt += message.content
                messages.remove(message)

    model_kwargs = {}
    if response_type == "json_object":
        model_kwargs["response_mime_type"] = "application/json"

    # Get Response from Gemini
    return gemini_completion_with_backoff(
        messages=messages, system_prompt=system_prompt, model_name=model, api_key=api_key, model_kwargs=model_kwargs
    )


def converse_gemini(
    references,
    user_query,
    online_results: Optional[Dict[str, Dict]] = None,
    conversation_log={},
    model: Optional[str] = "gemini-1.5-flash",
    api_key: Optional[str] = None,
    temperature: float = 0.2,
    completion_func=None,
    conversation_commands=[ConversationCommand.Default],
    max_prompt_size=None,
    tokenizer_name=None,
    location_data: LocationData = None,
    user_name: str = None,
    agent: Agent = None,
):
    """
    Converse with user using Google's Gemini
    """
    # Initialize Variables
    current_date = datetime.now()
    compiled_references = "\n\n".join({f"# {item}" for item in references})

    conversation_primer = prompts.query_prompt.format(query=user_query)

    if agent and agent.personality:
        system_prompt = prompts.custom_personality.format(
            name=agent.name,
            bio=agent.personality,
            current_date=current_date.strftime("%Y-%m-%d"),
            day_of_week=current_date.strftime("%A"),
        )
    else:
        system_prompt = prompts.personality.format(
            current_date=current_date.strftime("%Y-%m-%d"),
            day_of_week=current_date.strftime("%A"),
        )

    if location_data:
        location = f"{location_data.city}, {location_data.region}, {location_data.country}"
        location_prompt = prompts.user_location.format(location=location)
        system_prompt = f"{system_prompt}\n{location_prompt}"

    if user_name:
        user_name_prompt = prompts.user_name.format(name=user_name)
        system_prompt = f"{system_prompt}\n{user_name_prompt}"

    # Get Conversation Primer appropriate to Conversation Type
    if conversation_commands == [ConversationCommand.Notes] and is_none_or_empty(compiled_references):
        completion_func(chat_response=prompts.no_notes_found.format())
        return iter([prompts.no_notes_found.format()])
    elif conversation_commands == [ConversationCommand.Online] and is_none_or_empty(online_results):
        completion_func(chat_response=prompts.no_online_results_found.format())
        return iter([prompts.no_online_results_found.format()])

    if ConversationCommand.Online in conversation_commands or ConversationCommand.Webpage in conversation_commands:
        conversation_primer = (
            f"{prompts.online_search_conversation.format(online_results=str(online_results))}\n{conversation_primer}"
        )
    if not is_none_or_empty(compiled_references):
        conversation_primer = f"{prompts.notes_conversation.format(query=user_query, references=compiled_references)}\n\n{conversation_primer}"

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        conversation_primer,
        conversation_log=conversation_log,
        model_name=model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
    )

    for message in messages:
        if message.role == "assistant":
            message.role = "model"

    for message in messages.copy():
        if message.role == "system":
            system_prompt += message.content
            messages.remove(message)

    truncated_messages = "\n".join({f"{message.content[:40]}..." for message in messages})
    logger.debug(f"Conversation Context for Gemini: {truncated_messages}")

    # Get Response from Google AI
    return gemini_chat_completion_with_backoff(
        messages=messages,
        compiled_references=references,
        online_results=online_results,
        model_name=model,
        temperature=temperature,
        api_key=api_key,
        system_prompt=system_prompt,
        completion_func=completion_func,
        max_prompt_size=max_prompt_size,
    )
