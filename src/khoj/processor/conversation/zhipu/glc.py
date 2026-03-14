"""
Zhipu AI (智谱) GLM model integration for Khoj.
Uses OpenAI-compatible API endpoint: https://open.bigmodel.cn/api/paas/v4/
"""

import logging
from typing import Any, AsyncGenerator, Dict, List, Optional

from langchain_core.messages.chat import ChatMessage

from khoj.processor.conversation.openai.utils import (
    chat_completion_with_backoff,
    completion_with_backoff,
    chat_completion_with_backoff,
)
from khoj.processor.conversation.utils import ResponseWithThought
from khoj.utils.helpers import ToolDefinition

logger = logging.getLogger(__name__)

# Zhipu AI API base URL (OpenAI-compatible)
ZHIPU_API_BASE_URL = "https://open.bigmodel.cn/api/paas/v4"

# Supported Zhipu models
ZHIPU_MODELS = [
    "glm-4-plus",
    "glm-4-0520",
    "glm-4",
    "glm-4-air",
    "glm-4-airx",
    "glm-4-long",
    "glm-4-flash",
    "glm-4v-plus",
    "glm-4v",
    "glm-z1-air",
    "glm-z1-airx",
    "glm-z1-flash",
]


def zhipu_send_message_to_model(
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
    Send message to Zhipu AI model using OpenAI-compatible API.
    """
    # Use Zhipu's OpenAI-compatible endpoint
    zhipu_base_url = api_base_url or ZHIPU_API_BASE_URL

    model_kwargs: Dict[str, Any] = {}

    # Zhipu supports JSON mode for some models
    if response_type == "json_object":
        model_kwargs["response_format"] = {"type": "json_object"}

    # Get Response from Zhipu AI
    return completion_with_backoff(
        messages=messages,
        model_name=model,
        openai_api_key=api_key,
        api_base_url=zhipu_base_url,
        deepthought=deepthought,
        model_kwargs=model_kwargs,
        tracer=tracer,
    )


async def converse_zhipu(
    messages: List[ChatMessage],
    model: str = "glm-4-flash",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    temperature: float = 0.7,
    deepthought: Optional[bool] = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using Zhipu AI's GLM models.
    Uses OpenAI-compatible streaming API.
    """
    logger.debug(f"Conversation Context for Zhipu GLM: {model}")

    # Use Zhipu's OpenAI-compatible endpoint
    zhipu_base_url = api_base_url or ZHIPU_API_BASE_URL

    # Get Response from Zhipu AI using streaming
    async for chunk in chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=temperature,
        openai_api_key=api_key,
        api_base_url=zhipu_base_url,
        deepthought=deepthought,
        tracer=tracer,
    ):
        yield chunk
