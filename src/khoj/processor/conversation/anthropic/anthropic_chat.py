import logging
from typing import AsyncGenerator, List, Optional

from langchain_core.messages.chat import ChatMessage

from khoj.processor.conversation.anthropic.utils import (
    anthropic_chat_completion_with_backoff,
    anthropic_completion_with_backoff,
)
from khoj.processor.conversation.utils import (
    ResponseWithThought,
    messages_to_print,
)

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
    messages: List[ChatMessage],
    # Model
    model: Optional[str] = "claude-3-7-sonnet-latest",
    api_key: Optional[str] = None,
    api_base_url: Optional[str] = None,
    deepthought: Optional[bool] = False,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using Anthropic's Claude
    """
    logger.debug(f"Conversation Context for Claude: {messages_to_print(messages)}")

    # Get Response from Claude
    async for chunk in anthropic_chat_completion_with_backoff(
        messages=messages,
        model_name=model,
        temperature=0.2,
        api_key=api_key,
        api_base_url=api_base_url,
        deepthought=deepthought,
        tracer=tracer,
    ):
        yield chunk
