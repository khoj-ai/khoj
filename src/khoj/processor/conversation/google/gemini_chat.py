import logging
from datetime import datetime
from typing import AsyncGenerator, Dict, List, Optional

from khoj.database.models import Agent, ChatMessageModel, ChatModel
from khoj.processor.conversation import prompts
from khoj.processor.conversation.google.utils import (
    gemini_chat_completion_with_backoff,
    gemini_completion_with_backoff,
)
from khoj.processor.conversation.utils import (
    OperatorRun,
    ResponseWithThought,
    generate_chatml_messages_with_context,
    messages_to_print,
)
from khoj.utils.helpers import is_none_or_empty, truncate_code_context
from khoj.utils.rawconfig import FileAttachment, LocationData
from khoj.utils.yaml import yaml_dump

logger = logging.getLogger(__name__)


def gemini_send_message_to_model(
    messages,
    api_key,
    model,
    api_base_url=None,
    response_type="text",
    response_schema=None,
    tools=None,
    model_kwargs=None,
    deepthought=False,
    tracer={},
):
    """
    Send message to model
    """
    model_kwargs = {}

    if tools:
        model_kwargs["tools"] = tools
    # Monitor for flakiness in 1.5+ models. This would cause unwanted behavior and terminate response early in 1.5 models.
    elif response_type == "json_object":
        model_kwargs["response_mime_type"] = "application/json"
        if response_schema:
            model_kwargs["response_schema"] = response_schema

    # Get Response from Gemini
    return gemini_completion_with_backoff(
        messages=messages,
        system_prompt="",
        model_name=model,
        api_key=api_key,
        api_base_url=api_base_url,
        model_kwargs=model_kwargs,
        deepthought=deepthought,
        tracer=tracer,
    )


async def converse_gemini(
    # Query
    user_query: str,
    # Context
    references: list[dict],
    online_results: Optional[Dict[str, Dict]] = None,
    code_results: Optional[Dict[str, Dict]] = None,
    operator_results: Optional[List[OperatorRun]] = None,
    query_images: Optional[list[str]] = None,
    query_files: str = None,
    generated_files: List[FileAttachment] = None,
    generated_asset_results: Dict[str, Dict] = {},
    program_execution_context: List[str] = None,
    location_data: LocationData = None,
    user_name: str = None,
    chat_history: List[ChatMessageModel] = [],
    # Model
    model: Optional[str] = "gemini-2.5-flash",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    temperature: float = 1.0,
    max_prompt_size=None,
    tokenizer_name=None,
    agent: Agent = None,
    vision_available: bool = False,
    deepthought: Optional[bool] = False,
    tracer={},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using Google's Gemini
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

    system_prompt += f"{system_prompt}\n\n{prompts.gemini_verbose_language_personality}"
    if location_data:
        location_prompt = prompts.user_location.format(location=f"{location_data}")
        system_prompt = f"{system_prompt}\n{location_prompt}"

    if user_name:
        user_name_prompt = prompts.user_name.format(name=user_name)
        system_prompt = f"{system_prompt}\n{user_name_prompt}"

    context_message = ""
    if not is_none_or_empty(references):
        context_message = f"{prompts.notes_conversation.format(query=user_query, references=yaml_dump(references))}\n\n"
    if not is_none_or_empty(online_results):
        context_message += f"{prompts.online_search_conversation.format(online_results=yaml_dump(online_results))}\n\n"
    if not is_none_or_empty(code_results):
        context_message += (
            f"{prompts.code_executed_context.format(code_results=truncate_code_context(code_results))}\n\n"
        )
    if not is_none_or_empty(operator_results):
        operator_content = [
            {"query": oc.query, "response": oc.response, "webpages": oc.webpages} for oc in operator_results
        ]
        context_message += (
            f"{prompts.operator_execution_context.format(operator_results=yaml_dump(operator_content))}\n\n"
        )
    context_message = context_message.strip()

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        user_query,
        context_message=context_message,
        chat_history=chat_history,
        model_name=model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
        query_images=query_images,
        vision_enabled=vision_available,
        model_type=ChatModel.ModelType.GOOGLE,
        query_files=query_files,
        generated_files=generated_files,
        generated_asset_results=generated_asset_results,
        program_execution_context=program_execution_context,
    )

    logger.debug(f"Conversation Context for Gemini: {messages_to_print(messages)}")

    # Get Response from Google AI
    async for chunk in gemini_chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=temperature,
        api_key=api_key,
        api_base_url=api_base_url,
        system_prompt=system_prompt,
        deepthought=deepthought,
        tracer=tracer,
    ):
        yield chunk
