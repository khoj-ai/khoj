import logging
from typing import Any, AsyncGenerator, Dict, List, Optional

from langchain_core.messages.chat import ChatMessage

from khoj.processor.conversation.openai.utils import (
    chat_completion_with_backoff,
    clean_response_schema,
    completion_with_backoff,
    get_structured_output_support,
    is_cerebras_api,
    responses_chat_completion_with_backoff,
    responses_completion_with_backoff,
    supports_responses_api,
    to_openai_tools,
)
from khoj.processor.conversation.utils import (
    ResponseWithThought,
    StructuredOutputSupport,
    messages_to_print,
)
from khoj.utils.helpers import ToolDefinition

logger = logging.getLogger(__name__)


def send_message_to_model(
    messages,
    api_key,
    model: str,
    response_type="text",
    response_schema=None,
    tools: list[ToolDefinition] = None,
    deepthought=False,
    api_base_url: str | None = None,
    tracer: dict = {},
):
    """
    Send message to model
    """

    model_kwargs: Dict[str, Any] = {}
    json_support = get_structured_output_support(model, api_base_url)
    strict = not is_cerebras_api(api_base_url)
    if tools and json_support == StructuredOutputSupport.TOOL:
        model_kwargs["tools"] = to_openai_tools(tools, model=model, api_base_url=api_base_url)
    elif response_schema and json_support >= StructuredOutputSupport.SCHEMA:
        # Drop unsupported fields from schema passed to OpenAI APi
        cleaned_response_schema = clean_response_schema(response_schema)
        if supports_responses_api(model, api_base_url):
            model_kwargs["text"] = {
                "format": {
                    "type": "json_schema",
                    "strict": strict,
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
                    "strict": strict,
                },
            }
    elif response_type == "json_object" and json_support == StructuredOutputSupport.OBJECT:
        model_kwargs["response_format"] = {"type": response_type}

    # Get Response from GPT
    if supports_responses_api(model, api_base_url):
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
    messages: List[ChatMessage],
    # Model
    model: str = "gpt-4.1-mini",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    temperature: float = 0.6,
    deepthought: Optional[bool] = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using OpenAI's ChatGPT
    """
    logger.debug(f"Conversation Context for GPT: {messages_to_print(messages)}")

    # Get Response from GPT
    if supports_responses_api(model, api_base_url):
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
