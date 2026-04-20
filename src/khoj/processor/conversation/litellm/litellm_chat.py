import logging
from typing import AsyncGenerator, List, Optional

from langchain_core.messages.chat import ChatMessage

from khoj.processor.conversation.litellm.utils import (
    litellm_chat_completion_with_backoff,
    litellm_completion_with_backoff,
    to_litellm_tools,
)
from khoj.processor.conversation.utils import (
    ResponseWithThought,
    messages_to_print,
)
from khoj.utils.helpers import ToolDefinition

logger = logging.getLogger(__name__)


def litellm_send_message_to_model(
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
    """Send message to model via LiteLLM AI Gateway."""
    model_kwargs: dict = {}

    if tools:
        model_kwargs["tools"] = to_litellm_tools(tools)
    elif response_schema:
        schema_json = response_schema if isinstance(response_schema, dict) else response_schema.model_json_schema()
        model_kwargs["response_format"] = {
            "type": "json_schema",
            "json_schema": {
                "schema": schema_json,
                "name": getattr(response_schema, "__name__", "response"),
            },
        }
    elif response_type == "json_object":
        model_kwargs["response_format"] = {"type": response_type}

    return litellm_completion_with_backoff(
        messages=messages,
        model_name=model,
        api_key=api_key,
        api_base_url=api_base_url,
        deepthought=deepthought,
        model_kwargs=model_kwargs,
        tracer=tracer,
    )


async def converse_litellm(
    messages: List[ChatMessage],
    model: str = "gpt-4o-mini",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    deepthought: Optional[bool] = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """Converse with user using LiteLLM AI Gateway."""
    logger.debug(f"Conversation Context for LiteLLM: {messages_to_print(messages)}")

    async for chunk in litellm_chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=0.6,
        api_key=api_key,
        api_base_url=api_base_url,
        deepthought=deepthought,
        tracer=tracer,
    ):
        yield chunk
