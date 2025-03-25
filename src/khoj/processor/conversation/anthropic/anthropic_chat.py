import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional

import pyjson5
from langchain.schema import ChatMessage

from khoj.database.models import Agent, ChatModel, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.anthropic.utils import (
    anthropic_chat_completion_with_backoff,
    anthropic_completion_with_backoff,
    format_messages_for_anthropic,
)
from khoj.processor.conversation.utils import (
    clean_json,
    construct_structured_message,
    generate_chatml_messages_with_context,
    messages_to_print,
)
from khoj.utils.helpers import (
    ConversationCommand,
    is_none_or_empty,
    truncate_code_context,
)
from khoj.utils.rawconfig import FileAttachment, LocationData
from khoj.utils.yaml import yaml_dump

logger = logging.getLogger(__name__)


def extract_questions_anthropic(
    text,
    model: Optional[str] = "claude-3-7-sonnet-latest",
    conversation_log={},
    api_key=None,
    api_base_url=None,
    location_data: LocationData = None,
    user: KhojUser = None,
    query_images: Optional[list[str]] = None,
    vision_enabled: bool = False,
    personality_context: Optional[str] = None,
    query_files: str = None,
    tracer: dict = {},
):
    """
    Infer search queries to retrieve relevant notes to answer user query
    """
    # Extract Past User Message and Inferred Questions from Conversation Log
    location = f"{location_data}" if location_data else "Unknown"
    username = prompts.user_name.format(name=user.get_full_name()) if user and user.get_full_name() else ""

    # Extract Past User Message and Inferred Questions from Conversation Log
    chat_history = "".join(
        [
            f'User: {chat["intent"]["query"]}\nAssistant: {{"queries": {chat["intent"].get("inferred-queries") or list([chat["intent"]["query"]])}}}\nA: {chat["message"]}\n\n'
            for chat in conversation_log.get("chat", [])[-4:]
            if chat["by"] == "khoj"
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
        personality_context=personality_context,
    )

    prompt = prompts.extract_questions_anthropic_user_message.format(
        chat_history=chat_history,
        text=text,
    )

    content = construct_structured_message(
        message=prompt,
        images=query_images,
        model_type=ChatModel.ModelType.ANTHROPIC,
        vision_enabled=vision_enabled,
        attached_file_context=query_files,
    )

    messages = [ChatMessage(content=content, role="user")]

    response = anthropic_completion_with_backoff(
        messages=messages,
        system_prompt=system_prompt,
        model_name=model,
        api_key=api_key,
        api_base_url=api_base_url,
        response_type="json_object",
        tracer=tracer,
    )

    # Extract, Clean Message from Claude's Response
    try:
        response = clean_json(response)
        response = pyjson5.loads(response)
        response = [q.strip() for q in response["queries"] if q.strip()]
        if not isinstance(response, list) or not response:
            logger.error(f"Invalid response for constructing subqueries: {response}")
            return [text]
        return response
    except:
        logger.warning(f"Claude returned invalid JSON. Falling back to using user message as search query.\n{response}")
        questions = [text]
    logger.debug(f"Extracted Questions by Claude: {questions}")
    return questions


def anthropic_send_message_to_model(
    messages, api_key, api_base_url, model, response_type="text", deepthought=False, tracer={}
):
    """
    Send message to model
    """
    # Get Response from GPT. Don't use response_type because Anthropic doesn't support it.
    return anthropic_completion_with_backoff(
        messages=messages,
        system_prompt="",
        model_name=model,
        api_key=api_key,
        api_base_url=api_base_url,
        response_type=response_type,
        deepthought=deepthought,
        tracer=tracer,
    )


def converse_anthropic(
    references,
    user_query,
    online_results: Optional[Dict[str, Dict]] = None,
    code_results: Optional[Dict[str, Dict]] = None,
    conversation_log={},
    model: Optional[str] = "claude-3-7-sonnet-latest",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    completion_func=None,
    conversation_commands=[ConversationCommand.Default],
    max_prompt_size=None,
    tokenizer_name=None,
    location_data: LocationData = None,
    user_name: str = None,
    agent: Agent = None,
    query_images: Optional[list[str]] = None,
    vision_available: bool = False,
    query_files: str = None,
    generated_files: List[FileAttachment] = None,
    program_execution_context: Optional[List[str]] = None,
    generated_asset_results: Dict[str, Dict] = {},
    deepthought: Optional[bool] = False,
    tracer: dict = {},
):
    """
    Converse with user using Anthropic's Claude
    """
    # Initialize Variables
    current_date = datetime.now()

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
        location_prompt = prompts.user_location.format(location=f"{location_data}")
        system_prompt = f"{system_prompt}\n{location_prompt}"

    if user_name:
        user_name_prompt = prompts.user_name.format(name=user_name)
        system_prompt = f"{system_prompt}\n{user_name_prompt}"

    # Get Conversation Primer appropriate to Conversation Type
    if conversation_commands == [ConversationCommand.Notes] and is_none_or_empty(references):
        completion_func(chat_response=prompts.no_notes_found.format())
        return iter([prompts.no_notes_found.format()])
    elif conversation_commands == [ConversationCommand.Online] and is_none_or_empty(online_results):
        completion_func(chat_response=prompts.no_online_results_found.format())
        return iter([prompts.no_online_results_found.format()])

    context_message = ""
    if not is_none_or_empty(references):
        context_message = f"{prompts.notes_conversation.format(query=user_query, references=yaml_dump(references))}\n\n"
    if ConversationCommand.Online in conversation_commands or ConversationCommand.Webpage in conversation_commands:
        context_message += f"{prompts.online_search_conversation.format(online_results=yaml_dump(online_results))}\n\n"
    if ConversationCommand.Code in conversation_commands and not is_none_or_empty(code_results):
        context_message += (
            f"{prompts.code_executed_context.format(code_results=truncate_code_context(code_results))}\n\n"
        )
    context_message = context_message.strip()

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        user_query,
        context_message=context_message,
        conversation_log=conversation_log,
        model_name=model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
        query_images=query_images,
        vision_enabled=vision_available,
        model_type=ChatModel.ModelType.ANTHROPIC,
        query_files=query_files,
        generated_files=generated_files,
        generated_asset_results=generated_asset_results,
        program_execution_context=program_execution_context,
    )

    logger.debug(f"Conversation Context for Claude: {messages_to_print(messages)}")

    # Get Response from Claude
    return anthropic_chat_completion_with_backoff(
        messages=messages,
        compiled_references=references,
        online_results=online_results,
        model_name=model,
        temperature=0.2,
        api_key=api_key,
        api_base_url=api_base_url,
        system_prompt=system_prompt,
        completion_func=completion_func,
        max_prompt_size=max_prompt_size,
        deepthought=deepthought,
        tracer=tracer,
    )
