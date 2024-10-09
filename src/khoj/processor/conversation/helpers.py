from fastapi import HTTPException

from khoj.database.adapters import ConversationAdapters, ais_user_subscribed
from khoj.database.models import ChatModelOptions, KhojUser
from khoj.processor.conversation.anthropic.anthropic_chat import (
    anthropic_send_message_to_model,
)
from khoj.processor.conversation.google.gemini_chat import gemini_send_message_to_model
from khoj.processor.conversation.offline.chat_model import send_message_to_model_offline
from khoj.processor.conversation.openai.gpt import send_message_to_model
from khoj.processor.conversation.utils import generate_chatml_messages_with_context
from khoj.utils import state
from khoj.utils.config import OfflineChatProcessorModel


async def send_message_to_model_wrapper(
    message: str,
    system_message: str = "",
    response_type: str = "text",
    chat_model_option: ChatModelOptions = None,
    subscribed: bool = False,
    uploaded_image_url: str = None,
):
    conversation_config: ChatModelOptions = (
        chat_model_option or await ConversationAdapters.aget_default_conversation_config()
    )

    vision_available = conversation_config.vision_enabled
    if not vision_available and uploaded_image_url:
        vision_enabled_config = await ConversationAdapters.aget_vision_enabled_config()
        if vision_enabled_config:
            conversation_config = vision_enabled_config
            vision_available = True

    chat_model = conversation_config.chat_model
    max_tokens = (
        conversation_config.subscribed_max_prompt_size
        if subscribed and conversation_config.subscribed_max_prompt_size
        else conversation_config.max_prompt_size
    )
    tokenizer = conversation_config.tokenizer
    model_type = conversation_config.model_type
    vision_available = conversation_config.vision_enabled

    if model_type == ChatModelOptions.ModelType.OFFLINE:
        if state.offline_chat_processor_config is None or state.offline_chat_processor_config.loaded_model is None:
            state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model, max_tokens)

        loaded_model = state.offline_chat_processor_config.loaded_model
        truncated_messages = generate_chatml_messages_with_context(
            user_message=message,
            system_message=system_message,
            model_name=chat_model,
            loaded_model=loaded_model,
            tokenizer_name=tokenizer,
            max_prompt_size=max_tokens,
            vision_enabled=vision_available,
            model_type=conversation_config.model_type,
        )

        return send_message_to_model_offline(
            messages=truncated_messages,
            loaded_model=loaded_model,
            model=chat_model,
            max_prompt_size=max_tokens,
            streaming=False,
            response_type=response_type,
        )

    elif model_type == ChatModelOptions.ModelType.OPENAI:
        openai_chat_config = conversation_config.openai_config
        api_key = openai_chat_config.api_key
        api_base_url = openai_chat_config.api_base_url
        truncated_messages = generate_chatml_messages_with_context(
            user_message=message,
            system_message=system_message,
            model_name=chat_model,
            max_prompt_size=max_tokens,
            tokenizer_name=tokenizer,
            vision_enabled=vision_available,
            uploaded_image_url=uploaded_image_url,
            model_type=conversation_config.model_type,
        )

        return send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model,
            response_type=response_type,
            api_base_url=api_base_url,
        )
    elif model_type == ChatModelOptions.ModelType.ANTHROPIC:
        api_key = conversation_config.openai_config.api_key
        truncated_messages = generate_chatml_messages_with_context(
            user_message=message,
            system_message=system_message,
            model_name=chat_model,
            max_prompt_size=max_tokens,
            tokenizer_name=tokenizer,
            vision_enabled=vision_available,
            uploaded_image_url=uploaded_image_url,
            model_type=conversation_config.model_type,
        )

        return anthropic_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model,
        )
    elif model_type == ChatModelOptions.ModelType.GOOGLE:
        api_key = conversation_config.openai_config.api_key
        truncated_messages = generate_chatml_messages_with_context(
            user_message=message,
            system_message=system_message,
            model_name=chat_model,
            max_prompt_size=max_tokens,
            tokenizer_name=tokenizer,
            vision_enabled=vision_available,
            uploaded_image_url=uploaded_image_url,
        )

        return gemini_send_message_to_model(
            messages=truncated_messages, api_key=api_key, model=chat_model, response_type=response_type
        )
    else:
        raise HTTPException(status_code=500, detail="Invalid conversation config")
