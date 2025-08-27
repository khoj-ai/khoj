import logging
from typing import AsyncGenerator, List, Optional

from langchain_core.messages.chat import ChatMessage

from khoj.processor.conversation.google.utils import (
    gemini_chat_completion_with_backoff,
    gemini_completion_with_backoff,
)
from khoj.processor.conversation.utils import (
    ResponseWithThought,
    messages_to_print,
)

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
    messages: List[ChatMessage],
    # Model
    model: Optional[str] = "gemini-2.5-flash",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    temperature: float = 1.0,
    deepthought: Optional[bool] = False,
    tracer={},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using Google's Gemini
    """
    logger.debug(f"Conversation Context for Gemini: {messages_to_print(messages)}")

    # Get Response from Google AI
    async for chunk in gemini_chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=temperature,
        api_key=api_key,
        api_base_url=api_base_url,
        deepthought=deepthought,
        tracer=tracer,
    ):
        yield chunk
