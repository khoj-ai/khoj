import logging
from datetime import datetime
from typing import Any, AsyncGenerator, Dict, List, Optional

from khoj.database.models import Agent, ChatMessageModel, ChatModel
from khoj.processor.conversation import prompts
from khoj.processor.conversation.openai.utils import (
    chat_completion_with_backoff,
    clean_response_schema,
    completion_with_backoff,
    get_structured_output_support,
    is_openai_api,
    responses_chat_completion_with_backoff,
    responses_completion_with_backoff,
    to_openai_tools,
)
from khoj.processor.conversation.utils import (
    OperatorRun,
    ResponseWithThought,
    StructuredOutputSupport,
    generate_chatml_messages_with_context,
    messages_to_print,
)
from khoj.utils.helpers import ToolDefinition, is_none_or_empty, truncate_code_context
from khoj.utils.rawconfig import FileAttachment, LocationData
from khoj.utils.yaml import yaml_dump

logger = logging.getLogger(__name__)


def send_message_to_model(
    messages,
    api_key,
    model,
    response_type="text",
    response_schema=None,
    tools: list[ToolDefinition] = None,
    deepthought=False,
    api_base_url=None,
    tracer: dict = {},
):
    """
    Send message to model
    """

    model_kwargs: Dict[str, Any] = {}
    json_support = get_structured_output_support(model, api_base_url)
    if tools and json_support == StructuredOutputSupport.TOOL:
        model_kwargs["tools"] = to_openai_tools(tools, use_responses_api=is_openai_api(api_base_url))
    elif response_schema and json_support >= StructuredOutputSupport.SCHEMA:
        # Drop unsupported fields from schema passed to OpenAI APi
        cleaned_response_schema = clean_response_schema(response_schema)
        if is_openai_api(api_base_url):
            model_kwargs["text"] = {
                "format": {
                    "type": "json_schema",
                    "strict": True,
                    "name": response_schema.__name__,
                    "schema": cleaned_response_schema,
                }
            }
        else:
            model_kwargs["response_format"] = {
                "type": "json_schema",
                "json_schema": {
                    "schema": cleaned_response_schema,
                    "name": response_schema.__name__,
                    "strict": True,
                },
            }
    elif response_type == "json_object" and json_support == StructuredOutputSupport.OBJECT:
        model_kwargs["response_format"] = {"type": response_type}

    # Get Response from GPT
    if is_openai_api(api_base_url):
        return responses_completion_with_backoff(
            messages=messages,
            model_name=model,
            openai_api_key=api_key,
            api_base_url=api_base_url,
            deepthought=deepthought,
            model_kwargs=model_kwargs,
            tracer=tracer,
        )
    else:
        return completion_with_backoff(
            messages=messages,
            model_name=model,
            openai_api_key=api_key,
            api_base_url=api_base_url,
            deepthought=deepthought,
            model_kwargs=model_kwargs,
            tracer=tracer,
        )


async def converse_openai(
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
    chat_history: list[ChatMessageModel] = [],
    model: str = "gpt-4.1-mini",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    temperature: float = 0.6,
    max_prompt_size=None,
    tokenizer_name=None,
    user_name: str = None,
    agent: Agent = None,
    vision_available: bool = False,
    deepthought: Optional[bool] = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using OpenAI's ChatGPT
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
        context_message = f"{prompts.notes_conversation.format(references=yaml_dump(references))}\n\n"
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
        system_prompt,
        chat_history,
        context_message=context_message,
        model_name=model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
        query_images=query_images,
        vision_enabled=vision_available,
        model_type=ChatModel.ModelType.OPENAI,
        query_files=query_files,
        generated_files=generated_files,
        generated_asset_results=generated_asset_results,
        program_execution_context=program_execution_context,
    )
    logger.debug(f"Conversation Context for GPT: {messages_to_print(messages)}")

    # Get Response from GPT
    if is_openai_api(api_base_url):
        async for chunk in responses_chat_completion_with_backoff(
            messages=messages,
            model_name=model,
            temperature=temperature,
            openai_api_key=api_key,
            api_base_url=api_base_url,
            deepthought=deepthought,
            tracer=tracer,
        ):
            yield chunk
    else:
        # For non-OpenAI APIs, use the chat completion method
        async for chunk in chat_completion_with_backoff(
            messages=messages,
            model_name=model,
            temperature=temperature,
            openai_api_key=api_key,
            api_base_url=api_base_url,
            deepthought=deepthought,
            tracer=tracer,
        ):
            yield chunk
