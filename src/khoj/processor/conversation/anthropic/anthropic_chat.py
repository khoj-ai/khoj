import logging
from datetime import datetime
from typing import AsyncGenerator, Dict, List, Optional

from khoj.database.models import Agent, ChatMessageModel, ChatModel
from khoj.processor.conversation import prompts
from khoj.processor.conversation.anthropic.utils import (
    anthropic_chat_completion_with_backoff,
    anthropic_completion_with_backoff,
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


def anthropic_send_message_to_model(
    messages,
    api_key,
    api_base_url,
    model,
    response_type="text",
    response_schema=None,
    tools=None,
    deepthought=False,
    tracer={},
):
    """
    Send message to model
    """
    # Get response from model. Don't use response_type because Anthropic doesn't support it.
    return anthropic_completion_with_backoff(
        messages=messages,
        system_prompt="",
        model_name=model,
        api_key=api_key,
        api_base_url=api_base_url,
        response_type=response_type,
        response_schema=response_schema,
        tools=tools,
        deepthought=deepthought,
        tracer=tracer,
    )


async def converse_anthropic(
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
    program_execution_context: Optional[List[str]] = None,
    generated_asset_results: Dict[str, Dict] = {},
    location_data: LocationData = None,
    user_name: str = None,
    chat_history: List[ChatMessageModel] = [],
    # Model
    model: Optional[str] = "claude-3-7-sonnet-latest",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    max_prompt_size=None,
    tokenizer_name=None,
    agent: Agent = None,
    vision_available: bool = False,
    deepthought: Optional[bool] = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
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
        model_type=ChatModel.ModelType.ANTHROPIC,
        query_files=query_files,
        generated_files=generated_files,
        generated_asset_results=generated_asset_results,
        program_execution_context=program_execution_context,
    )

    logger.debug(f"Conversation Context for Claude: {messages_to_print(messages)}")

    # Get Response from Claude
    async for chunk in anthropic_chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=0.2,
        api_key=api_key,
        api_base_url=api_base_url,
        system_prompt=system_prompt,
        max_prompt_size=max_prompt_size,
        deepthought=deepthought,
        tracer=tracer,
    ):
        yield chunk
