import logging
from typing import AsyncGenerator, Dict, List, Optional, Tuple

from fastapi import HTTPException
from pydantic import BaseModel

from khoj.database.adapters import AgentAdapters, ConversationAdapters
from khoj.database.models import ChatMessageModel, ChatModel, Conversation, KhojUser
from khoj.processor.conversation.anthropic.anthropic_chat import (
    anthropic_send_message_to_model,
    converse_anthropic,
)
from khoj.processor.conversation.google.gemini_chat import (
    converse_gemini,
    gemini_send_message_to_model,
)
from khoj.processor.conversation.offline.chat_model import (
    converse_offline,
    send_message_to_model_offline,
)
from khoj.processor.conversation.openai.gpt import (
    converse_openai,
    send_message_to_model,
)
from khoj.processor.conversation.utils import (
    OperatorRun,
    ResearchIteration,
    ResponseWithThought,
    generate_chatml_messages_with_context,
)
from khoj.utils import state
from khoj.utils.config import OfflineChatProcessorModel
from khoj.utils.helpers import ToolDefinition
from khoj.utils.rawconfig import FileAttachment, LocationData

logger = logging.getLogger(__name__)


async def send_message_to_model_wrapper(
    query: str,
    system_message: str = "",
    response_type: str = "text",
    response_schema: BaseModel = None,
    tools: List[ToolDefinition] = None,
    deepthought: bool = False,
    user: KhojUser = None,
    query_images: List[str] = None,
    context: str = "",
    query_files: str = None,
    chat_history: list[ChatMessageModel] = [],
    agent_chat_model: ChatModel = None,
    tracer: dict = {},
):
    chat_model: ChatModel = await ConversationAdapters.aget_default_chat_model(user, agent_chat_model)
    vision_available = chat_model.vision_enabled
    if not vision_available and query_images:
        logger.warning(f"Vision is not enabled for default model: {chat_model.name}.")
        vision_enabled_config = await ConversationAdapters.aget_vision_enabled_config()
        if vision_enabled_config:
            chat_model = vision_enabled_config
            vision_available = True
    if vision_available and query_images:
        logger.info(f"Using {chat_model.name} model to understand {len(query_images)} images.")

    max_tokens = await ConversationAdapters.aget_max_context_size(chat_model, user)
    chat_model_name = chat_model.name
    tokenizer = chat_model.tokenizer
    model_type = chat_model.model_type
    vision_available = chat_model.vision_enabled
    api_key = chat_model.ai_model_api.api_key
    api_base_url = chat_model.ai_model_api.api_base_url
    loaded_model = None

    if model_type == ChatModel.ModelType.OFFLINE:
        if state.offline_chat_processor_config is None or state.offline_chat_processor_config.loaded_model is None:
            state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model_name, max_tokens)
        loaded_model = state.offline_chat_processor_config.loaded_model

    truncated_messages = generate_chatml_messages_with_context(
        user_message=query,
        context_message=context,
        system_message=system_message,
        chat_history=chat_history,
        model_name=chat_model_name,
        loaded_model=loaded_model,
        tokenizer_name=tokenizer,
        max_prompt_size=max_tokens,
        vision_enabled=vision_available,
        query_images=query_images,
        model_type=model_type,
        query_files=query_files,
    )

    if model_type == ChatModel.ModelType.OFFLINE:
        return send_message_to_model_offline(
            messages=truncated_messages,
            loaded_model=loaded_model,
            model_name=chat_model_name,
            max_prompt_size=max_tokens,
            streaming=False,
            response_type=response_type,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.OPENAI:
        return send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tools=tools,
            deepthought=deepthought,
            api_base_url=api_base_url,
            tracer=tracer,
        )
    elif model_type == ChatModel.ModelType.ANTHROPIC:
        return anthropic_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tools=tools,
            deepthought=deepthought,
            api_base_url=api_base_url,
            tracer=tracer,
        )
    elif model_type == ChatModel.ModelType.GOOGLE:
        return gemini_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tools=tools,
            deepthought=deepthought,
            api_base_url=api_base_url,
            tracer=tracer,
        )
    else:
        raise HTTPException(status_code=500, detail="Invalid conversation config")


def send_message_to_model_wrapper_sync(
    message: str,
    system_message: str = "",
    response_type: str = "text",
    response_schema: BaseModel = None,
    user: KhojUser = None,
    query_images: List[str] = None,
    query_files: str = "",
    chat_history: List[ChatMessageModel] = [],
    tracer: dict = {},
):
    chat_model: ChatModel = ConversationAdapters.get_default_chat_model(user)

    if chat_model is None:
        raise HTTPException(status_code=500, detail="Contact the server administrator to set a default chat model.")

    max_tokens = ConversationAdapters.get_max_context_size(chat_model, user)
    chat_model_name = chat_model.name
    model_type = chat_model.model_type
    vision_available = chat_model.vision_enabled
    api_key = chat_model.ai_model_api.api_key
    api_base_url = chat_model.ai_model_api.api_base_url
    loaded_model = None

    if model_type == ChatModel.ModelType.OFFLINE:
        if state.offline_chat_processor_config is None or state.offline_chat_processor_config.loaded_model is None:
            state.offline_chat_processor_config = OfflineChatProcessorModel(chat_model_name, max_tokens)
        loaded_model = state.offline_chat_processor_config.loaded_model

    truncated_messages = generate_chatml_messages_with_context(
        user_message=message,
        system_message=system_message,
        chat_history=chat_history,
        model_name=chat_model_name,
        loaded_model=loaded_model,
        max_prompt_size=max_tokens,
        vision_enabled=vision_available,
        model_type=model_type,
        query_images=query_images,
        query_files=query_files,
    )

    if model_type == ChatModel.ModelType.OFFLINE:
        return send_message_to_model_offline(
            messages=truncated_messages,
            loaded_model=loaded_model,
            model_name=chat_model_name,
            max_prompt_size=max_tokens,
            streaming=False,
            response_type=response_type,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.OPENAI:
        return send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            api_base_url=api_base_url,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.ANTHROPIC:
        return anthropic_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            api_base_url=api_base_url,
            model=chat_model_name,
            response_type=response_type,
            tracer=tracer,
        )

    elif model_type == ChatModel.ModelType.GOOGLE:
        return gemini_send_message_to_model(
            messages=truncated_messages,
            api_key=api_key,
            api_base_url=api_base_url,
            model=chat_model_name,
            response_type=response_type,
            response_schema=response_schema,
            tracer=tracer,
        )
    else:
        raise HTTPException(status_code=500, detail="Invalid conversation config")


async def agenerate_chat_response(
    q: str,
    chat_history: List[ChatMessageModel],
    conversation: Conversation,
    compiled_references: List[Dict] = [],
    online_results: Dict[str, Dict] = {},
    code_results: Dict[str, Dict] = {},
    operator_results: List[OperatorRun] = [],
    research_results: List[ResearchIteration] = [],
    user: KhojUser = None,
    location_data: LocationData = None,
    user_name: Optional[str] = None,
    query_images: Optional[List[str]] = None,
    query_files: str = None,
    raw_generated_files: List[FileAttachment] = [],
    program_execution_context: List[str] = [],
    generated_asset_results: Dict[str, Dict] = {},
    is_subscribed: bool = False,
    tracer: dict = {},
) -> Tuple[AsyncGenerator[ResponseWithThought, None], Dict[str, str]]:
    # Initialize Variables
    chat_response_generator: AsyncGenerator[ResponseWithThought, None] = None

    metadata = {}
    agent = await AgentAdapters.aget_conversation_agent_by_id(conversation.agent.id) if conversation.agent else None

    try:
        query_to_run = q
        deepthought = False
        if research_results:
            compiled_research = "".join([r.summarizedResult for r in research_results if r.summarizedResult])
            if compiled_research:
                query_to_run = f"<query>{q}</query>\n<collected_research>\n{compiled_research}\n</collected_research>"
            compiled_references = []
            online_results = {}
            code_results = {}
            operator_results = []
            deepthought = True

        chat_model = await ConversationAdapters.aget_valid_chat_model(user, conversation, is_subscribed)
        vision_available = chat_model.vision_enabled
        if not vision_available and query_images:
            vision_enabled_config = await ConversationAdapters.aget_vision_enabled_config()
            if vision_enabled_config:
                chat_model = vision_enabled_config
                vision_available = True

        if chat_model.model_type == "offline":
            loaded_model = state.offline_chat_processor_config.loaded_model
            chat_response_generator = converse_offline(
                # Query
                user_query=query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                location_data=location_data,
                user_name=user_name,
                query_files=query_files,
                chat_history=chat_history,
                # Model
                loaded_model=loaded_model,
                model_name=chat_model.name,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                tracer=tracer,
            )

        elif chat_model.model_type == ChatModel.ModelType.OPENAI:
            openai_chat_config = chat_model.ai_model_api
            api_key = openai_chat_config.api_key
            chat_model_name = chat_model.name
            chat_response_generator = converse_openai(
                # Query
                query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                query_images=query_images,
                query_files=query_files,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                program_execution_context=program_execution_context,
                location_data=location_data,
                user_name=user_name,
                chat_history=chat_history,
                # Model
                model=chat_model_name,
                api_key=api_key,
                api_base_url=openai_chat_config.api_base_url,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                vision_available=vision_available,
                deepthought=deepthought,
                tracer=tracer,
            )

        elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
            api_key = chat_model.ai_model_api.api_key
            api_base_url = chat_model.ai_model_api.api_base_url
            chat_response_generator = converse_anthropic(
                # Query
                query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                query_images=query_images,
                query_files=query_files,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                program_execution_context=program_execution_context,
                location_data=location_data,
                user_name=user_name,
                chat_history=chat_history,
                # Model
                model=chat_model.name,
                api_key=api_key,
                api_base_url=api_base_url,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                vision_available=vision_available,
                deepthought=deepthought,
                tracer=tracer,
            )
        elif chat_model.model_type == ChatModel.ModelType.GOOGLE:
            api_key = chat_model.ai_model_api.api_key
            api_base_url = chat_model.ai_model_api.api_base_url
            chat_response_generator = converse_gemini(
                # Query
                query_to_run,
                # Context
                references=compiled_references,
                online_results=online_results,
                code_results=code_results,
                operator_results=operator_results,
                query_images=query_images,
                query_files=query_files,
                generated_files=raw_generated_files,
                generated_asset_results=generated_asset_results,
                program_execution_context=program_execution_context,
                location_data=location_data,
                user_name=user_name,
                chat_history=chat_history,
                # Model
                model=chat_model.name,
                api_key=api_key,
                api_base_url=api_base_url,
                max_prompt_size=chat_model.max_prompt_size,
                tokenizer_name=chat_model.tokenizer,
                agent=agent,
                vision_available=vision_available,
                deepthought=deepthought,
                tracer=tracer,
            )

        metadata.update({"chat_model": chat_model.name})

    except Exception as e:
        logger.error(e, exc_info=True)
        raise HTTPException(status_code=500, detail=str(e))

    # Return the generator directly
    return chat_response_generator, metadata
