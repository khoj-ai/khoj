import base64
import json
import logging
import math
import mimetypes
import os
import re
import uuid
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from io import BytesIO
from typing import Any, Callable, Dict, List, Literal, Optional, Tuple, Union

import PIL.Image
import pyjson5
import requests
import tiktoken
import yaml
from langchain_core.messages.chat import ChatMessage
from llama_cpp import LlamaTokenizer
from llama_cpp.llama import Llama
from pydantic import BaseModel, ConfigDict, ValidationError, create_model
from transformers import AutoTokenizer, PreTrainedTokenizer, PreTrainedTokenizerFast

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import (
    ChatMessageModel,
    ChatModel,
    ClientApplication,
    Intent,
    KhojUser,
    ToolContext,
)
from khoj.processor.conversation import prompts
from khoj.processor.conversation.offline.utils import download_model, infer_max_tokens
from khoj.search_filter.base_filter import BaseFilter
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.utils import state
from khoj.utils.helpers import (
    ConversationCommand,
    is_none_or_empty,
    is_promptrace_enabled,
    merge_dicts,
)
from khoj.utils.rawconfig import FileAttachment
from khoj.utils.yaml import yaml_dump

logger = logging.getLogger(__name__)

try:
    from git import Repo
except ImportError:
    if is_promptrace_enabled():
        logger.warning("GitPython not installed. `pip install gitpython` to use prompt tracer.")

model_to_prompt_size = {
    # OpenAI Models
    "gpt-4o": 60000,
    "gpt-4o-mini": 60000,
    "gpt-4.1": 60000,
    "gpt-4.1-mini": 120000,
    "gpt-4.1-nano": 120000,
    "o1-mini": 90000,
    "o1": 30000,
    "o3-mini": 90000,
    "o3": 60000,
    "o3-pro": 30000,
    "o4-mini": 90000,
    # Google Models
    "gemini-2.5-flash": 120000,
    "gemini-2.5-pro": 60000,
    "gemini-2.0-flash": 120000,
    "gemini-2.0-flash-lite": 120000,
    "gemini-1.5-flash": 120000,
    "gemini-1.5-pro": 60000,
    # Anthropic Models
    "claude-3-5-sonnet-20241022": 60000,
    "claude-3-5-sonnet-latest": 60000,
    "claude-3-7-sonnet-20250219": 60000,
    "claude-3-7-sonnet-latest": 60000,
    "claude-3-5-haiku-20241022": 60000,
    "claude-sonnet-4-0": 60000,
    "claude-sonnet-4-20250514": 60000,
    "claude-opus-4-0": 60000,
    "claude-opus-4-20250514": 60000,
    # Offline Models
    "bartowski/Qwen2.5-14B-Instruct-GGUF": 20000,
    "bartowski/Meta-Llama-3.1-8B-Instruct-GGUF": 20000,
    "bartowski/Llama-3.2-3B-Instruct-GGUF": 20000,
    "bartowski/gemma-2-9b-it-GGUF": 6000,
    "bartowski/gemma-2-2b-it-GGUF": 6000,
}
model_to_tokenizer: Dict[str, str] = {}


class AgentMessage(BaseModel):
    role: Literal["user", "assistant", "system", "environment"]
    content: Union[str, List, None] = None
    # Raw response should contain tool calls. The role should be "assistant" for tool calls.
    raw_response: Optional[Any] = None


class OperatorRun:
    def __init__(
        self,
        query: str,
        trajectory: list[AgentMessage] | list[dict] = None,
        response: str = None,
        webpages: list[dict] = None,
    ):
        self.query = query
        self.response = response
        self.webpages = webpages or []
        self.trajectory: list[AgentMessage] = []
        if trajectory:
            for item in trajectory:
                if isinstance(item, dict):
                    self.trajectory.append(AgentMessage(**item))
                elif hasattr(item, "role") and hasattr(item, "content"):  # Heuristic for AgentMessage like object
                    self.trajectory.append(item)
                else:
                    logger.warning(f"Unexpected item type in trajectory: {type(item)}")

    def to_dict(self) -> dict:
        # Ensure AgentMessage instances in trajectory are also dicts
        serialized_trajectory = []
        for msg in self.trajectory:
            if hasattr(msg, "model_dump"):  # Check if it's a Pydantic model
                serialized_trajectory.append(msg.model_dump())
            elif isinstance(msg, dict):
                serialized_trajectory.append(msg)  # Already a dict
        return {
            "query": self.query,
            "response": self.response,
            "trajectory": serialized_trajectory,
            "webpages": self.webpages,
        }


class ToolCall:
    def __init__(self, name: str, args: dict, id: str):
        self.name = name
        self.args = args
        self.id = id


class ResearchIteration:
    def __init__(
        self,
        query: ToolCall | dict | str,
        context: list = None,
        onlineContext: dict = None,
        codeContext: dict = None,
        operatorContext: dict | OperatorRun = None,
        toolContext: list = None,
        summarizedResult: str = None,
        warning: str = None,
        raw_response: list = None,
    ):
        self.query = ToolCall(**query) if isinstance(query, dict) else query
        self.context = context
        self.onlineContext = onlineContext
        self.codeContext = codeContext
        self.operatorContext = OperatorRun(**operatorContext) if isinstance(operatorContext, dict) else operatorContext
        self.toolContext = toolContext
        self.summarizedResult = summarizedResult
        self.warning = warning
        self.raw_response = raw_response

    def to_dict(self) -> dict:
        data = vars(self).copy()
        data["query"] = self.query.__dict__ if isinstance(self.query, ToolCall) else self.query
        data["operatorContext"] = self.operatorContext.to_dict() if self.operatorContext else None
        return data


def construct_iteration_history(
    previous_iterations: List[ResearchIteration],
    query: str = None,
    query_images: List[str] = None,
    query_files: str = None,
) -> list[ChatMessageModel]:
    iteration_history: list[ChatMessageModel] = []
    query_message_content = construct_structured_message(query, query_images, attached_file_context=query_files)
    if query_message_content:
        iteration_history.append(ChatMessageModel(by="you", message=query_message_content))

    for iteration in previous_iterations:
        if not iteration.query or isinstance(iteration.query, str):
            iteration_history.append(
                ChatMessageModel(
                    by="you",
                    message=iteration.summarizedResult
                    or iteration.warning
                    or "Please specify what you want to do next.",
                )
            )
            continue
        iteration_history += [
            ChatMessageModel(
                by="khoj",
                message=iteration.raw_response or [iteration.query.__dict__],
                intent=Intent(type="tool_call", query=query),
            ),
            ChatMessageModel(
                by="you",
                intent=Intent(type="tool_result"),
                message=[
                    {
                        "type": "tool_result",
                        "id": iteration.query.id,
                        "name": iteration.query.name,
                        "content": iteration.summarizedResult,
                    }
                ],
            ),
        ]

    return iteration_history


def construct_chat_history(chat_history: list[ChatMessageModel], n: int = 4, agent_name="AI") -> str:
    chat_history_str = ""
    for chat in chat_history[-n:]:
        intent_type = chat.intent.type if chat.intent and chat.intent.type else ""
        inferred_queries = chat.intent.inferred_queries if chat.intent else None
        if chat.by == "khoj" and intent_type in ["remember", "reminder", "summarize"]:
            if inferred_queries:
                chat_history_str += f'{agent_name}: {{"queries": {inferred_queries}}}\n'
            chat_history_str += f"{agent_name}: {chat.message}\n\n"
        elif chat.by == "khoj" and chat.images:
            chat_history_str += f"{agent_name}: [generated image redacted for space]\n"
        elif chat.by == "khoj" and ("excalidraw" in intent_type):
            chat_history_str += f"{agent_name}: {inferred_queries[0]}\n"
        elif chat.by == "you":
            chat_history_str += f"User: {chat.message}\n"
            raw_query_files = chat.queryFiles
            if raw_query_files:
                query_files: Dict[str, str] = {}
                for file in raw_query_files:
                    query_files[file["name"]] = file["content"]

                query_file_context = gather_raw_query_files(query_files)
                chat_history_str += f"User: {query_file_context}\n"

    return chat_history_str


def construct_question_history(
    conversation_log: list[ChatMessageModel],
    include_query: bool = True,
    lookback: int = 6,
    query_prefix: str = "Q",
    agent_name: str = "Khoj",
) -> str:
    """
    Constructs a chat history string formatted for query extraction purposes.
    """
    history_parts = ""
    original_query = None
    for chat in conversation_log[-lookback:]:
        if chat.by == "you":
            original_query = json.dumps(chat.message)
            history_parts += f"{query_prefix}: {original_query}\n"
        if chat.by == "khoj":
            if original_query is None:
                continue

            message = chat.message
            inferred_queries_list = chat.intent.inferred_queries or []

            # Ensure inferred_queries_list is a list, defaulting to the original query in a list
            if not inferred_queries_list:
                inferred_queries_list = [original_query]
            # If it's a string (though unlikely based on usage), wrap it in a list
            elif isinstance(inferred_queries_list, str):
                inferred_queries_list = [inferred_queries_list]

            if include_query:
                # Ensure 'type' exists and is a string before checking 'to-image'
                intent_type = chat.intent.type if chat.intent and chat.intent.type else ""
                if "to-image" not in intent_type:
                    history_parts += f'{agent_name}: {{"queries": {inferred_queries_list}}}\n'
                    history_parts += f"A: {message}\n\n"
            else:
                history_parts += f"{agent_name}: {message}\n\n"

            # Reset original_query for the next turn
            original_query = None

    return history_parts


def construct_chat_history_for_operator(conversation_history: List[ChatMessageModel], n: int = 6) -> list[AgentMessage]:
    """
    Construct chat history for operator agent in conversation log.
    Only include last n completed turns (i.e with user and khoj message).
    """
    chat_history: list[AgentMessage] = []
    user_message: Optional[AgentMessage] = None

    for chat in conversation_history:
        if len(chat_history) >= n:
            break
        if chat.by == "you" and chat.message:
            content = [{"type": "text", "text": chat.message}]
            for file in chat.queryFiles or []:
                content += [{"type": "text", "text": f'## File: {file["name"]}\n\n{file["content"]}'}]
            user_message = AgentMessage(role="user", content=content)
        elif chat.by == "khoj" and chat.message:
            chat_history += [user_message, AgentMessage(role="assistant", content=chat.message)]
    return chat_history


def construct_tool_chat_history(
    previous_iterations: List[ResearchIteration], tool: ConversationCommand = None
) -> List[ChatMessageModel]:
    """
    Construct chat history from previous iterations for a specific tool

    If a tool is provided, only the inferred queries for that tool is added.
    If no tool is provided inferred query for all tools used are added.
    """
    chat_history: list = []
    base_extractor: Callable[[ResearchIteration], List[str]] = lambda iteration: []
    extract_inferred_query_map: Dict[ConversationCommand, Callable[[ResearchIteration], List[str]]] = {
        ConversationCommand.Notes: (
            lambda iteration: [c["query"] for c in iteration.context] if iteration.context else []
        ),
        ConversationCommand.SearchWeb: (
            lambda iteration: list(iteration.onlineContext.keys()) if iteration.onlineContext else []
        ),
        ConversationCommand.ReadWebpage: (
            lambda iteration: list(iteration.onlineContext.keys()) if iteration.onlineContext else []
        ),
        ConversationCommand.RunCode: (
            lambda iteration: list(iteration.codeContext.keys()) if iteration.codeContext else []
        ),
    }
    for iteration in previous_iterations:
        if not iteration.query or isinstance(iteration.query, str):
            chat_history.append(
                ChatMessageModel(
                    by="you",
                    message=iteration.summarizedResult
                    or iteration.warning
                    or "Please specify what you want to do next.",
                )
            )
            continue

        # If a tool is provided use the inferred query extractor for that tool if available
        # If no tool is provided, use inferred query extractor for the tool used in the iteration
        # Fallback to base extractor if the tool does not have an inferred query extractor
        try:
            inferred_query_extractor = extract_inferred_query_map.get(
                tool or ConversationCommand(iteration.query.name), base_extractor
            )
        except:
            inferred_query_extractor = base_extractor
        chat_history += [
            ChatMessageModel(
                by="you",
                message=yaml.dump(iteration.query.args, default_flow_style=False),
            ),
            ChatMessageModel(
                by="khoj",
                intent=Intent(
                    type="remember",
                    query=yaml.dump(iteration.query.args, default_flow_style=False),
                    inferred_queries=inferred_query_extractor(iteration),
                    memory_type="notes",
                ),
                message=iteration.summarizedResult,
            ),
        ]

    return chat_history


class ChatEvent(Enum):
    START_LLM_RESPONSE = "start_llm_response"
    END_LLM_RESPONSE = "end_llm_response"
    MESSAGE = "message"
    REFERENCES = "references"
    GENERATED_ASSETS = "generated_assets"
    STATUS = "status"
    THOUGHT = "thought"
    METADATA = "metadata"
    USAGE = "usage"
    END_RESPONSE = "end_response"
    INTERRUPT = "interrupt"


def message_to_log(
    user_message,
    chat_response,
    user_message_metadata={},
    khoj_message_metadata={},
    chat_history: List[ChatMessageModel] = [],
) -> List[ChatMessageModel]:
    """Create json logs from messages, metadata for conversation log"""
    default_khoj_message_metadata = {
        "intent": {"type": "remember", "memory-type": "notes", "query": user_message},
    }
    khoj_response_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Filter out any fields that are set to None
    user_message_metadata = {k: v for k, v in user_message_metadata.items() if v is not None}
    khoj_message_metadata = {k: v for k, v in khoj_message_metadata.items() if v is not None}

    # Create json log from Human's message
    human_log = merge_dicts({"message": user_message, "by": "you"}, user_message_metadata)

    # Create json log from GPT's response
    khoj_log = merge_dicts(khoj_message_metadata, default_khoj_message_metadata)
    khoj_log = merge_dicts({"message": chat_response, "by": "khoj", "created": khoj_response_time}, khoj_log)

    # Validate message logs
    # Only validates top-level fields, not nested fields, defined in ChatMessageModel
    class StrictChatMessageModel(ChatMessageModel):
        model_config = ConfigDict(extra="forbid", strict=True)

    try:
        StrictChatMessageModel(**human_log)
    except ValidationError as e:
        logger.error(f"Validation error in user chat message: {e}\nUser Message: {human_log}\n")
    try:
        StrictChatMessageModel(**khoj_log)
    except ValidationError as e:
        logger.error(f"Validation error in khoj chat message: {e}\nKhoj Message: {khoj_log}\n")

    human_message = ChatMessageModel(**human_log)
    khoj_message = ChatMessageModel(**khoj_log)
    chat_history.extend([human_message, khoj_message])
    return chat_history


async def save_to_conversation_log(
    q: str,
    chat_response: str,
    user: KhojUser,
    user_message_time: str = None,
    compiled_references: List[Dict[str, Any]] = [],
    online_results: Dict[str, Any] = {},
    code_results: Dict[str, Any] = {},
    tool_results: List[Dict[str, Any]] = [],
    operator_results: List[OperatorRun] = None,
    inferred_queries: List[str] = [],
    intent_type: str = "remember",
    client_application: ClientApplication = None,
    conversation_id: str = None,
    automation_id: str = None,
    query_images: List[str] = None,
    raw_query_files: List[FileAttachment] = [],
    generated_images: List[str] = [],
    raw_generated_files: List[FileAttachment] = [],
    generated_mermaidjs_diagram: str = None,
    research_results: Optional[List[ResearchIteration]] = None,
    train_of_thought: List[Any] = [],
    tracer: Dict[str, Any] = {},
):
    user_message_time = user_message_time or datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    turn_id = tracer.get("mid") or str(uuid.uuid4())

    user_message_metadata = {"created": user_message_time, "images": query_images, "turnId": turn_id}

    if raw_query_files and len(raw_query_files) > 0:
        user_message_metadata["queryFiles"] = [file.model_dump(mode="json") for file in raw_query_files]

    khoj_message_metadata = {
        "context": compiled_references,
        "intent": {"inferred-queries": inferred_queries, "type": intent_type},
        "onlineContext": online_results,
        "codeContext": code_results,
        "toolContext": tool_results,
        "operatorContext": [o.to_dict() for o in operator_results] if operator_results and not chat_response else None,
        "researchContext": [r.to_dict() for r in research_results] if research_results and not chat_response else None,
        "automationId": automation_id,
        "trainOfThought": train_of_thought,
        "turnId": turn_id,
        "images": generated_images,
        "queryFiles": [file.model_dump(mode="json") for file in raw_generated_files],
    }

    if generated_mermaidjs_diagram:
        khoj_message_metadata["mermaidjsDiagram"] = generated_mermaidjs_diagram

    try:
        new_messages = message_to_log(
            user_message=q,
            chat_response=chat_response,
            user_message_metadata=user_message_metadata,
            khoj_message_metadata=khoj_message_metadata,
            chat_history=[],
        )
    except ValidationError as e:
        new_messages = None
        logger.error(f"Error constructing chat history: {e}")

    if new_messages:
        await ConversationAdapters.save_conversation(
            user,
            new_messages,
            client_application=client_application,
            conversation_id=conversation_id,
            user_message=q,
        )

    if is_promptrace_enabled():
        merge_message_into_conversation_trace(q, chat_response, tracer)

    logger.info(
        f"""
Saved Conversation Turn
You ({user.username}): "{q}"

Khoj: "{chat_response}"
""".strip()
    )


def construct_structured_message(
    message: list[dict] | str,
    images: list[str] = None,
    model_type: str = None,
    vision_enabled: bool = True,
    attached_file_context: str = None,
):
    """
    Format messages into appropriate multimedia format for supported chat model types.

    Assume vision is enabled and chat model provider supports messages in chatml format, unless specified otherwise.
    """
    if not model_type or model_type in [
        ChatModel.ModelType.OPENAI,
        ChatModel.ModelType.GOOGLE,
        ChatModel.ModelType.ANTHROPIC,
    ]:
        constructed_messages: List[dict[str, Any]] = []
        if not is_none_or_empty(message):
            constructed_messages += [{"type": "text", "text": message}] if isinstance(message, str) else message
        # Drop image message passed by caller if chat model does not have vision enabled
        if not vision_enabled:
            constructed_messages = [m for m in constructed_messages if m.get("type") != "image_url"]
        if not is_none_or_empty(attached_file_context):
            constructed_messages += [{"type": "text", "text": attached_file_context}]
        if vision_enabled and images:
            for image in images:
                constructed_messages += [{"type": "image_url", "image_url": {"url": image}}]
        return constructed_messages

    message = message if isinstance(message, str) else "\n\n".join(m["text"] for m in message)
    if not is_none_or_empty(attached_file_context):
        return f"{attached_file_context}\n\n{message}"

    return message


def gather_raw_query_files(
    query_files: Dict[str, str],
):
    """
    Gather contextual data from the given (raw) files
    """

    if len(query_files) == 0:
        return ""

    contextual_data = " ".join(
        [f"File: {file_name}\n\n{file_content}\n\n" for file_name, file_content in query_files.items()]
    )
    return f"I have attached the following files:\n\n{contextual_data}"


def generate_chatml_messages_with_context(
    user_message: str,
    system_message: str = None,
    chat_history: list[ChatMessageModel] = [],
    model_name="gpt-4o-mini",
    loaded_model: Optional[Llama] = None,
    max_prompt_size=None,
    tokenizer_name=None,
    query_images=None,
    vision_enabled=False,
    model_type="",
    context_message="",
    query_files: str = None,
    generated_files: List[FileAttachment] = None,
    generated_asset_results: Dict[str, Dict] = {},
    program_execution_context: List[str] = [],
):
    """Generate chat messages with appropriate context from previous conversation to send to the chat model"""
    # Set max prompt size from user config or based on pre-configured for model and machine specs
    if not max_prompt_size:
        if loaded_model:
            max_prompt_size = infer_max_tokens(loaded_model.n_ctx(), model_to_prompt_size.get(model_name, math.inf))
        else:
            max_prompt_size = model_to_prompt_size.get(model_name, 10000)

    # Scale lookback turns proportional to max prompt size supported by model
    lookback_turns = max_prompt_size // 750

    # Extract Chat History for Context
    chatml_messages: List[ChatMessage] = []
    for chat in chat_history:
        message_context = []
        message_attached_files = ""

        generated_assets = {}

        chat_message = chat.message
        role = "user" if chat.by == "you" else "assistant"

        # Legacy code to handle excalidraw diagrams prior to Dec 2024
        if chat.by == "khoj" and "excalidraw" in chat.intent.type or "":
            chat_message = (chat.intent.inferred_queries or [])[0]

        if chat.queryFiles:
            raw_query_files = chat.queryFiles
            query_files_dict = dict()
            for file in raw_query_files:
                query_files_dict[file["name"]] = file["content"]

            message_attached_files = gather_raw_query_files(query_files_dict)
            chatml_messages.append(ChatMessage(content=message_attached_files, role=role))

        if not is_none_or_empty(chat.onlineContext):
            message_context += [
                {
                    "type": "text",
                    "text": f"{prompts.online_search_conversation.format(online_results=chat.onlineContext)}",
                }
            ]

        if not is_none_or_empty(chat.codeContext):
            message_context += [
                {
                    "type": "text",
                    "text": f"{prompts.code_executed_context.format(code_results=chat.codeContext)}",
                }
            ]

        if not is_none_or_empty(chat.operatorContext):
            operator_context = chat.operatorContext
            operator_content = "\n\n".join([f'## Task: {oc["query"]}\n{oc["response"]}\n' for oc in operator_context])
            message_context += [
                {
                    "type": "text",
                    "text": f"{prompts.operator_execution_context.format(operator_results=operator_content)}",
                }
            ]

        if not is_none_or_empty(chat.context):
            references = "\n\n".join(
                {f"# URI: {item.uri}\n## {item.compiled}\n" for item in chat.context or [] if isinstance(item, dict)}
            )
            message_context += [{"type": "text", "text": f"{prompts.notes_conversation.format(references=references)}"}]

        if not is_none_or_empty(message_context):
            reconstructed_context_message = ChatMessage(content=message_context, role="user")
            chatml_messages.insert(0, reconstructed_context_message)

        if not is_none_or_empty(chat.images) and role == "assistant":
            generated_assets["image"] = {
                "query": (chat.intent.inferred_queries or [user_message])[0],
            }

        if not is_none_or_empty(chat.mermaidjsDiagram) and role == "assistant":
            generated_assets["diagram"] = {
                "query": (chat.intent.inferred_queries or [user_message])[0],
            }

        if not is_none_or_empty(generated_assets):
            chatml_messages.append(
                ChatMessage(
                    content=f"{prompts.generated_assets_context.format(generated_assets=yaml_dump(generated_assets))}\n",
                    role="user",
                )
            )

        message_content = construct_structured_message(
            chat_message, chat.images if role == "user" else [], model_type, vision_enabled
        )

        reconstructed_message = ChatMessage(
            content=message_content,
            role=role,
            additional_kwargs={"message_type": chat.intent.type if chat.intent else None},
        )
        chatml_messages.insert(0, reconstructed_message)

        if len(chatml_messages) >= 3 * lookback_turns:
            break

    messages: list[ChatMessage] = []

    if not is_none_or_empty(generated_asset_results):
        messages.append(
            ChatMessage(
                content=f"{prompts.generated_assets_context.format(generated_assets=yaml_dump(generated_asset_results))}\n\n",
                role="user",
            )
        )

    if not is_none_or_empty(user_message):
        messages.append(
            ChatMessage(
                content=construct_structured_message(
                    user_message, query_images, model_type, vision_enabled, query_files
                ),
                role="user",
            )
        )

    if generated_files:
        message_attached_files = gather_raw_query_files({file.name: file.content for file in generated_files})
        messages.append(ChatMessage(content=message_attached_files, role="assistant"))

    if program_execution_context:
        program_context_text = "\n".join(program_execution_context)
        context_message += f"{prompts.additional_program_context.format(context=program_context_text)}\n"

    if not is_none_or_empty(context_message):
        messages.append(ChatMessage(content=context_message, role="user"))

    if len(chatml_messages) > 0:
        messages += chatml_messages

    if not is_none_or_empty(system_message):
        messages.append(ChatMessage(content=system_message, role="system"))

    # Normalize message content to list of chatml dictionaries
    for message in messages:
        if isinstance(message.content, str):
            message.content = [{"type": "text", "text": message.content}]

    # Truncate oldest messages from conversation history until under max supported prompt size by model
    messages = truncate_messages(messages, max_prompt_size, model_name, loaded_model, tokenizer_name)

    # Return message in chronological order
    return messages[::-1]


def get_encoder(
    model_name: str,
    loaded_model: Optional[Llama] = None,
    tokenizer_name=None,
) -> tiktoken.Encoding | PreTrainedTokenizer | PreTrainedTokenizerFast | LlamaTokenizer:
    default_tokenizer = "gpt-4o"

    try:
        if loaded_model:
            encoder = loaded_model.tokenizer()
        elif model_name.startswith("gpt-") or model_name.startswith("o1"):
            # as tiktoken doesn't recognize o1 model series yet
            encoder = tiktoken.encoding_for_model("gpt-4o" if model_name.startswith("o1") else model_name)
        elif tokenizer_name:
            if tokenizer_name in state.pretrained_tokenizers:
                encoder = state.pretrained_tokenizers[tokenizer_name]
            else:
                encoder = AutoTokenizer.from_pretrained(tokenizer_name)
                state.pretrained_tokenizers[tokenizer_name] = encoder
        else:
            encoder = download_model(model_name).tokenizer()
    except:
        encoder = tiktoken.encoding_for_model(default_tokenizer)
        if state.verbose > 2:
            logger.debug(
                f"Fallback to default chat model tokenizer: {default_tokenizer}.\nConfigure tokenizer for model: {model_name} in Khoj settings to improve context stuffing."
            )
    return encoder


def count_tokens(
    message_content: str | list[str | dict],
    encoder: PreTrainedTokenizer | PreTrainedTokenizerFast | LlamaTokenizer | tiktoken.Encoding,
) -> int:
    """
    Count the total number of tokens in a list of messages.

    Assumes each images takes 500 tokens for approximation.
    """
    if isinstance(message_content, list):
        image_count = 0
        message_content_parts: list[str] = []
        # Collate message content into single string to ease token counting
        for part in message_content:
            if isinstance(part, dict) and part.get("type") == "image_url":
                image_count += 1
            elif isinstance(part, dict) and part.get("type") == "text":
                message_content_parts.append(part["text"])
            elif isinstance(part, dict) and hasattr(part, "model_dump"):
                message_content_parts.append(json.dumps(part.model_dump()))
            elif isinstance(part, dict) and hasattr(part, "__dict__"):
                message_content_parts.append(json.dumps(part.__dict__))
            elif isinstance(part, dict):
                # If part is a dict but not a recognized type, convert to JSON string
                try:
                    message_content_parts.append(json.dumps(part))
                except (TypeError, ValueError) as e:
                    logger.warning(f"Failed to serialize part {part} to JSON: {e}. Skipping.")
                    image_count += 1  # Treat as an image/binary if serialization fails
            elif isinstance(part, str):
                message_content_parts.append(part)
            else:
                logger.warning(f"Unknown message type: {part}. Skipping.")
        message_content = "\n".join(message_content_parts).rstrip()
        return len(encoder.encode(message_content)) + image_count * 500
    elif isinstance(message_content, str):
        return len(encoder.encode(message_content))
    else:
        return len(encoder.encode(json.dumps(message_content)))


def count_total_tokens(messages: list[ChatMessage], encoder, system_message: Optional[ChatMessage]) -> Tuple[int, int]:
    """Count total tokens in messages including system message"""
    system_message_tokens = count_tokens(system_message.content, encoder) if system_message else 0
    message_tokens = sum([count_tokens(message.content, encoder) for message in messages])
    # Reserves 4 tokens to demarcate each message (e.g <|im_start|>user, <|im_end|>, <|endoftext|> etc.)
    total_tokens = message_tokens + system_message_tokens + 4 * len(messages)
    return total_tokens, system_message_tokens


def truncate_messages(
    messages: list[ChatMessage],
    max_prompt_size: int,
    model_name: str,
    loaded_model: Optional[Llama] = None,
    tokenizer_name=None,
) -> list[ChatMessage]:
    """Truncate messages to fit within max prompt size supported by model"""
    encoder = get_encoder(model_name, loaded_model, tokenizer_name)

    # Extract system message from messages
    system_message = None
    for idx, message in enumerate(messages):
        if message.role == "system":
            system_message = messages.pop(idx)
            break

    # Drop older messages until under max supported prompt size by model
    total_tokens, system_message_tokens = count_total_tokens(messages, encoder, system_message)

    while total_tokens > max_prompt_size and (len(messages) > 1 or len(messages[0].content) > 1):
        # If the last message has more than one content part, pop the oldest content part.
        # For tool calls, the whole message should dropped, assistant's tool call content being truncated annoys AI APIs.
        if len(messages[-1].content) > 1 and messages[-1].additional_kwargs.get("message_type") != "tool_call":
            # The oldest content part is earlier in content list. So pop from the front.
            messages[-1].content.pop(0)
        # Otherwise, pop the last message if it has only one content part or is a tool call.
        else:
            # The oldest message is the last one. So pop from the back.
            dropped_message = messages.pop()
            # Drop tool result pair of tool call, if tool call message has been removed
            if (
                dropped_message.additional_kwargs.get("message_type") == "tool_call"
                and messages
                and messages[-1].additional_kwargs.get("message_type") == "tool_result"
            ):
                messages.pop()

        total_tokens, _ = count_total_tokens(messages, encoder, system_message)

    # Truncate current message if still over max supported prompt size by model
    total_tokens, _ = count_total_tokens(messages, encoder, system_message)
    if total_tokens > max_prompt_size:
        # At this point, a single message with a single content part of type dict should remain
        assert (
            len(messages) == 1 and len(messages[0].content) == 1 and isinstance(messages[0].content[0], dict)
        ), "Expected a single message with a single content part remaining at this point in truncation"

        # Collate message content into single string to ease truncation
        part = messages[0].content[0]
        message_content: str = part["text"] if part["type"] == "text" else json.dumps(part)
        message_role = messages[0].role

        remaining_context = "\n".join(message_content.split("\n")[:-1])
        original_question = "\n" + "\n".join(message_content.split("\n")[-1:])

        original_question_tokens = count_tokens(original_question, encoder)
        remaining_tokens = max_prompt_size - system_message_tokens
        if remaining_tokens > original_question_tokens:
            remaining_tokens -= original_question_tokens
            truncated_context = encoder.decode(encoder.encode(remaining_context)[:remaining_tokens]).strip()
            truncated_content = truncated_context + original_question
        else:
            truncated_content = encoder.decode(encoder.encode(original_question)[:remaining_tokens]).strip()
        messages = [ChatMessage(content=[{"type": "text", "text": truncated_content}], role=message_role)]

        truncated_snippet = (
            f"{truncated_content[:1000]}\n...\n{truncated_content[-1000:]}"
            if len(truncated_content) > 2000
            else truncated_content
        )
        logger.debug(
            f"Truncate current message to fit within max prompt size of {max_prompt_size} supported by {model_name} model:\n {truncated_snippet}"
        )

    if system_message:
        # Default system message role is system.
        # Fallback to system message role of user for models that do not support this role like gemma-2 and openai's o1 model series.
        system_message.role = "user" if "gemma-2" in model_name or model_name.startswith("o1") else "system"
    return messages + [system_message] if system_message else messages


def reciprocal_conversation_to_chatml(message_pair):
    """Convert a single back and forth between user and assistant to chatml format"""
    return [ChatMessage(content=message, role=role) for message, role in zip(message_pair, ["user", "assistant"])]


def clean_json(response: str):
    """Remove any markdown json codeblock and newline formatting if present. Useful for non schema enforceable models"""
    return response.strip().replace("\n", "").removeprefix("```json").removesuffix("```")


def clean_mermaidjs(response: str):
    """Remove any markdown mermaidjs codeblock and newline formatting if present. Useful for non schema enforceable models"""
    return response.strip().removeprefix("```mermaid").removesuffix("```")


def clean_code_python(code: str):
    """Remove any markdown codeblock and newline formatting if present. Useful for non schema enforceable models"""
    return code.strip().removeprefix("```python").removesuffix("```")


def load_complex_json(json_str):
    """
    Preprocess a raw JSON string to
    - escape unescaped double quotes within value strings while preserving the JSON structure and already escaped quotes.
    - remove suffix after the first valid JSON object,
    """

    def replace_unescaped_quotes(match):
        # Get the content between colons and commas/end braces
        content = match.group(1)
        # Replace unescaped double, single quotes that aren't already escaped
        # Uses negative lookbehind to avoid replacing already escaped quotes
        # Replace " with \"
        processed_dq = re.sub(r'(?<!\\)"', '\\"', content)
        # Replace \' with \\'
        processed_final = re.sub(r"(?<!\\)\\'", r"\\\\'", processed_dq)
        return f': "{processed_final}"'

    # Match content between : and either , or }
    # This pattern looks for ': ' followed by any characters until , or }
    pattern = r':\s*"(.*?)(?<!\\)"(?=[,}])'

    # Process the JSON string
    cleaned = clean_json(rf"{json_str}")
    processed = re.sub(pattern, replace_unescaped_quotes, cleaned)

    # See which json loader can load the processed JSON as valid
    errors = []
    json_loaders_to_try = [json.loads, pyjson5.loads]
    for loads in json_loaders_to_try:
        try:
            return loads(processed)
        except (json.JSONDecodeError, pyjson5.Json5Exception) as e_load:
            loader_name = loads.__name__
            errors.append(f"{loader_name} (initial parse): {type(e_load).__name__}: {str(e_load)}")

            # Handle plain text suffixes by slicing at error position
            if hasattr(e_load, "pos") and 0 < e_load.pos < len(processed):
                try:
                    sliced = processed[: e_load.pos].strip()
                    if sliced:
                        return loads(sliced)
                except Exception as e_slice:
                    errors.append(
                        f"{loader_name} after slice at {e_load.pos}: {type(e_slice).__name__}: {str(e_slice)}"
                    )
    # If all loaders fail, raise the aggregated error
    raise ValueError(
        f"Failed to load JSON with errors: {'; '.join(errors)}\n\n"
        f"While attempting to load this cleaned JSON:\n{processed}"
    )


def defilter_query(query: str):
    """Remove any query filters in query"""
    defiltered_query = query
    filters: List[BaseFilter] = [WordFilter(), FileFilter(), DateFilter()]
    for filter in filters:
        defiltered_query = filter.defilter(defiltered_query)
    return defiltered_query


@dataclass
class ImageWithType:
    content: Any
    type: str


def get_image_from_base64(image: str, type="b64"):
    # Extract image type and base64 data from inline image data
    image_base64 = image.split(",", 1)[1]
    image_type = image.split(";", 1)[0].split(":", 1)[1]

    # Convert image to desired format
    if type == "b64":
        return ImageWithType(content=image_base64, type=image_type)
    elif type == "pil":
        image_data = base64.b64decode(image_base64)
        image_pil = PIL.Image.open(BytesIO(image_data))
        return ImageWithType(content=image_pil, type=image_type)
    elif type == "bytes":
        image_data = base64.b64decode(image_base64)
        return ImageWithType(content=image_data, type=image_type)


def get_image_from_url(image_url: str, type="pil"):
    try:
        response = requests.get(image_url)
        response.raise_for_status()  # Check if the request was successful

        # Get content type from response or infer from URL
        content_type = response.headers.get("content-type") or mimetypes.guess_type(image_url)[0] or "image/webp"

        # Convert image to desired format
        image_data: Any = None
        if type == "b64":
            image_data = base64.b64encode(response.content).decode("utf-8")
        elif type == "pil":
            image_data = PIL.Image.open(BytesIO(response.content))
        elif type == "bytes":
            image_data = response.content
        else:
            raise ValueError(f"Invalid image type: {type}")

        return ImageWithType(content=image_data, type=content_type)
    except requests.exceptions.RequestException as e:
        logger.error(f"Failed to get image from URL {image_url}: {e}")
        return ImageWithType(content=None, type=None)


def commit_conversation_trace(
    session: list[ChatMessage],
    response: str | list[dict],
    tracer: dict,
    system_message: str | list[dict] = "",
    repo_path: str = None,
) -> str:
    """
    Save trace of conversation step using git. Useful to visualize, compare and debug traces.
    Returns the path to the repository.
    """
    try:
        from git import Repo
    except ImportError:
        return None

    # Infer repository path from environment variable or provided path
    repo_path = repo_path if not is_none_or_empty(repo_path) else os.getenv("PROMPTRACE_DIR")
    if not repo_path:
        return None

    # Serialize session, system message and response to yaml
    system_message_yaml = json.dumps(system_message, ensure_ascii=False, sort_keys=False)
    response_yaml = json.dumps(response, ensure_ascii=False, sort_keys=False)
    formatted_session = [{"role": message.role, "content": message.content} for message in session]
    session_yaml = json.dumps(formatted_session, ensure_ascii=False, sort_keys=False)
    query = (
        json.dumps(session[-1].content, ensure_ascii=False, sort_keys=False).strip().removeprefix("'").removesuffix("'")
    )  # Extract serialized query from chat session

    # Extract chat metadata for session
    uid, cid, mid = tracer.get("uid", "main"), tracer.get("cid", "main"), tracer.get("mid")

    try:
        # Prepare git repository
        os.makedirs(repo_path, exist_ok=True)
        repo = Repo.init(repo_path)

        # Remove post-commit hook if it exists
        hooks_dir = os.path.join(repo_path, ".git", "hooks")
        post_commit_hook = os.path.join(hooks_dir, "post-commit")
        if os.path.exists(post_commit_hook):
            os.remove(post_commit_hook)

        # Configure git user if not set
        if not repo.config_reader().has_option("user", "email"):
            repo.config_writer().set_value("user", "name", "Prompt Tracer").release()
            repo.config_writer().set_value("user", "email", "promptracer@khoj.dev").release()

        # Create an initial commit if the repository is newly created
        if not repo.head.is_valid():
            repo.index.commit("And then there was a trace")

        # Check out the initial commit
        initial_commit = repo.commit("HEAD~0")
        repo.head.reference = initial_commit
        repo.head.reset(index=True, working_tree=True)

        # Create or switch to user branch from initial commit
        user_branch = f"u_{uid}"
        if user_branch not in repo.branches:
            repo.create_head(user_branch)
        repo.heads[user_branch].checkout()

        # Create or switch to conversation branch from user branch
        conv_branch = f"c_{cid}"
        if conv_branch not in repo.branches:
            repo.create_head(conv_branch)
        repo.heads[conv_branch].checkout()

        # Create or switch to message branch from conversation branch
        msg_branch = f"m_{mid}" if mid else None
        if msg_branch and msg_branch not in repo.branches:
            repo.create_head(msg_branch)
        if msg_branch:
            repo.heads[msg_branch].checkout()

        # Include file with content to commit
        files_to_commit = {"query": session_yaml, "response": response_yaml, "system_prompt": system_message_yaml}

        # Write files and stage them
        for filename, content in files_to_commit.items():
            file_path = os.path.join(repo_path, filename)
            # Unescape special characters in content for better readability
            content = content.strip().replace("\\n", "\n").replace("\\t", "\t")
            with open(file_path, "w", encoding="utf-8") as f:
                f.write(content)
            repo.index.add([filename])

        # Create commit
        metadata_yaml = yaml.dump(tracer, allow_unicode=True, sort_keys=False, default_flow_style=False)
        commit_message = f"""
{query[:250]}

Response:
---
{response[:500]}...

Metadata
---
{metadata_yaml}
""".strip()

        repo.index.commit(commit_message)

        logger.debug(f"Saved conversation trace to repo at {repo_path}")
        return repo_path
    except Exception as e:
        logger.error(f"Failed to add conversation trace to repo: {str(e)}", exc_info=True)
        return None


def merge_message_into_conversation_trace(query: str, response: str, tracer: dict, repo_path=None) -> bool:
    """
    Merge the message branch into its parent conversation branch.

    Args:
        query: User query
        response: Assistant response
        tracer: Dictionary containing uid, cid and mid
        repo_path: Path to the git repository

    Returns:
        bool: True if merge was successful, False otherwise
    """
    try:
        from git import Repo
    except ImportError:
        return False
    try:
        # Extract branch names
        msg_branch = f"m_{tracer['mid']}"
        conv_branch = f"c_{tracer['cid']}"

        # Infer repository path from environment variable or provided path
        repo_path = repo_path if not is_none_or_empty(repo_path) else os.getenv("PROMPTRACE_DIR")
        if not repo_path:
            return None
        repo = Repo(repo_path)

        # Checkout conversation branch
        repo.heads[conv_branch].checkout()

        # Create commit message
        metadata_yaml = yaml.dump(tracer, allow_unicode=True, sort_keys=False, default_flow_style=False)
        commit_message = f"""
{query[:250]}

Response:
---
{response[:500]}...

Metadata
---
{metadata_yaml}
""".strip()

        # Merge message branch into conversation branch
        repo.git.merge(msg_branch, no_ff=True, m=commit_message)

        # Delete message branch after merge
        repo.delete_head(msg_branch, force=True)

        logger.debug(f"Successfully merged {msg_branch} into {conv_branch}")
        return True
    except Exception as e:
        logger.error(f"Failed to merge message {msg_branch} into conversation {conv_branch}: {str(e)}", exc_info=True)
        return False


def messages_to_print(messages: list[ChatMessage], max_length: int = 70) -> str:
    """
    Format and truncate messages to print, ensuring JSON serializable content
    """

    def safe_serialize(content: Any) -> str:
        try:
            # Try JSON serialization
            json.dumps(content)
            return content
        except (TypeError, json.JSONDecodeError):
            # Handle non-serializable types
            if hasattr(content, "format") and content.format == "WEBP":
                return "[WebP Image]"
            elif hasattr(content, "__dict__"):
                return str(content.__dict__)
            return str(content)

    return "\n".join([f"{json.dumps(safe_serialize(message.content))[:max_length]}..." for message in messages])


class StructuredOutputSupport(int, Enum):
    NONE = 0
    OBJECT = 1
    SCHEMA = 2
    TOOL = 3


class ResponseWithThought:
    def __init__(self, text: str = None, thought: str = None, raw_content: list = None):
        self.text = text
        self.thought = thought
        self.raw_content = raw_content
