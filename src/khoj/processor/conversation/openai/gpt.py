import logging
from datetime import datetime
from typing import AsyncGenerator, Dict, List, Optional

from openai.lib._pydantic import _ensure_strict_json_schema
from pydantic import BaseModel

from khoj.database.models import Agent, ChatMessageModel, ChatModel
from khoj.processor.conversation import prompts
from khoj.processor.conversation.openai.utils import (
    chat_completion_with_backoff,
    completion_with_backoff,
    get_openai_api_json_support,
)
from khoj.processor.conversation.utils import (
    JsonSupport,
    OperatorRun,
    ResponseWithThought,
    generate_chatml_messages_with_context,
    messages_to_print,
)
from khoj.utils.helpers import is_none_or_empty, truncate_code_context
from khoj.utils.rawconfig import FileAttachment, LocationData
from khoj.utils.yaml import yaml_dump

logger = logging.getLogger(__name__)


def send_message_to_model(
    messages,
    api_key,
    model,
    response_type="text",
    response_schema=None,
    deepthought=False,
    api_base_url=None,
    tracer: dict = {},
):
    """
    Send message to model
    """

    model_kwargs = {}
    json_support = get_openai_api_json_support(model, api_base_url)
    if response_schema and json_support == JsonSupport.SCHEMA:
        # Drop unsupported fields from schema passed to OpenAI APi
        cleaned_response_schema = clean_response_schema(response_schema)
        model_kwargs["response_format"] = {
            "type": "json_schema",
            "json_schema": {
                "schema": cleaned_response_schema,
                "name": response_schema.__name__,
                "strict": True,
            },
        }
    elif response_type == "json_object" and json_support == JsonSupport.OBJECT:
        model_kwargs["response_format"] = {"type": response_type}

    # Get Response from GPT
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
    model: str = "gpt-4o-mini",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    temperature: float = 0.4,
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


def clean_response_schema(schema: BaseModel | dict) -> dict:
    """
    Format response schema to be compatible with OpenAI API.

    Clean the response schema by removing unsupported fields.
    """
    # Normalize schema to OpenAI compatible JSON schema format
    schema_json = schema if isinstance(schema, dict) else schema.model_json_schema()
    schema_json = _ensure_strict_json_schema(schema_json, path=(), root=schema_json)

    # Recursively drop unsupported fields from schema passed to OpenAI API
    # See https://platform.openai.com/docs/guides/structured-outputs#supported-schemas
    fields_to_exclude = ["minItems", "maxItems"]
    if isinstance(schema_json, dict) and isinstance(schema_json.get("properties"), dict):
        for _, prop_value in schema_json["properties"].items():
            if isinstance(prop_value, dict):
                # Remove specified fields from direct properties
                for field in fields_to_exclude:
                    prop_value.pop(field, None)
            # Recursively remove specified fields from child properties
            if "items" in prop_value and isinstance(prop_value["items"], dict):
                clean_response_schema(prop_value["items"])

    # Return cleaned schema
    return schema_json
