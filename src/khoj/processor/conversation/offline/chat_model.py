import asyncio
import logging
import os
from datetime import datetime
from threading import Thread
from time import perf_counter
from typing import Any, AsyncGenerator, Dict, List, Union

from langchain_core.messages.chat import ChatMessage
from llama_cpp import Llama

from khoj.database.models import Agent, ChatMessageModel, ChatModel
from khoj.processor.conversation import prompts
from khoj.processor.conversation.offline.utils import download_model
from khoj.processor.conversation.utils import (
    ResponseWithThought,
    commit_conversation_trace,
    generate_chatml_messages_with_context,
    messages_to_print,
)
from khoj.utils import state
from khoj.utils.helpers import (
    is_none_or_empty,
    is_promptrace_enabled,
    truncate_code_context,
)
from khoj.utils.rawconfig import FileAttachment, LocationData
from khoj.utils.yaml import yaml_dump

logger = logging.getLogger(__name__)


async def converse_offline(
    # Query
    user_query: str,
    # Context
    references: list[dict] = [],
    online_results={},
    code_results={},
    query_files: str = None,
    generated_files: List[FileAttachment] = None,
    additional_context: List[str] = None,
    generated_asset_results: Dict[str, Dict] = {},
    location_data: LocationData = None,
    user_name: str = None,
    chat_history: list[ChatMessageModel] = [],
    # Model
    model_name: str = "bartowski/Meta-Llama-3.1-8B-Instruct-GGUF",
    loaded_model: Union[Any, None] = None,
    max_prompt_size=None,
    tokenizer_name=None,
    agent: Agent = None,
    tracer: dict = {},
) -> AsyncGenerator[ResponseWithThought, None]:
    """
    Converse with user using Llama (Async Version)
    """
    # Initialize Variables
    assert loaded_model is None or isinstance(loaded_model, Llama), "loaded_model must be of type Llama, if configured"
    offline_chat_model = loaded_model or download_model(model_name, max_tokens=max_prompt_size)
    tracer["chat_model"] = model_name
    current_date = datetime.now()

    if agent and agent.personality:
        system_prompt = prompts.custom_system_prompt_offline_chat.format(
            name=agent.name,
            bio=agent.personality,
            current_date=current_date.strftime("%Y-%m-%d"),
            day_of_week=current_date.strftime("%A"),
        )
    else:
        system_prompt = prompts.system_prompt_offline_chat.format(
            current_date=current_date.strftime("%Y-%m-%d"),
            day_of_week=current_date.strftime("%A"),
        )

    if location_data:
        location_prompt = prompts.user_location.format(location=f"{location_data}")
        system_prompt = f"{system_prompt}\n{location_prompt}"

    if user_name:
        user_name_prompt = prompts.user_name.format(name=user_name)
        system_prompt = f"{system_prompt}\n{user_name_prompt}"

    # Get Conversation Primer appropriate to Conversation Type
    context_message = ""
    if not is_none_or_empty(references):
        context_message = f"{prompts.notes_conversation_offline.format(references=yaml_dump(references))}\n\n"
    if not is_none_or_empty(online_results):
        simplified_online_results = online_results.copy()
        for result in online_results:
            if online_results[result].get("webpages"):
                simplified_online_results[result] = online_results[result]["webpages"]

        context_message += f"{prompts.online_search_conversation_offline.format(online_results=yaml_dump(simplified_online_results))}\n\n"
    if not is_none_or_empty(code_results):
        context_message += (
            f"{prompts.code_executed_context.format(code_results=truncate_code_context(code_results))}\n\n"
        )
    context_message = context_message.strip()

    # Setup Prompt with Primer or Conversation History
    messages = generate_chatml_messages_with_context(
        user_query,
        system_prompt,
        chat_history,
        context_message=context_message,
        model_name=model_name,
        loaded_model=offline_chat_model,
        max_prompt_size=max_prompt_size,
        tokenizer_name=tokenizer_name,
        model_type=ChatModel.ModelType.OFFLINE,
        query_files=query_files,
        generated_files=generated_files,
        generated_asset_results=generated_asset_results,
        program_execution_context=additional_context,
    )

    logger.debug(f"Conversation Context for {model_name}: {messages_to_print(messages)}")

    # Use asyncio.Queue and a thread to bridge sync iterator
    queue: asyncio.Queue[ResponseWithThought] = asyncio.Queue()
    stop_phrases = ["<s>", "INST]", "Notes:"]

    def _sync_llm_thread():
        """Synchronous function to run in a separate thread."""
        aggregated_response = ""
        start_time = perf_counter()
        state.chat_lock.acquire()
        try:
            response_iterator = send_message_to_model_offline(
                messages,
                loaded_model=offline_chat_model,
                stop=stop_phrases,
                max_prompt_size=max_prompt_size,
                streaming=True,
                tracer=tracer,
            )
            for response in response_iterator:
                response_delta: str = response["choices"][0]["delta"].get("content", "")
                # Log the time taken to start response
                if aggregated_response == "" and response_delta != "":
                    logger.info(f"First response took: {perf_counter() - start_time:.3f} seconds")
                # Handle response chunk
                aggregated_response += response_delta
                # Put chunk into the asyncio queue (non-blocking)
                try:
                    queue.put_nowait(ResponseWithThought(response=response_delta))
                except asyncio.QueueFull:
                    # Should not happen with default queue size unless consumer is very slow
                    logger.warning("Asyncio queue full during offline LLM streaming.")
                    # Potentially block here or handle differently if needed
                    asyncio.run(queue.put(ResponseWithThought(response=response_delta)))

            # Log the time taken to stream the entire response
            logger.info(f"Chat streaming took: {perf_counter() - start_time:.3f} seconds")

            # Save conversation trace
            tracer["chat_model"] = model_name
            if is_promptrace_enabled():
                commit_conversation_trace(messages, aggregated_response, tracer)

        except Exception as e:
            logger.error(f"Error in offline LLM thread: {e}", exc_info=True)
        finally:
            state.chat_lock.release()
            # Signal end of stream
            queue.put_nowait(None)

    # Start the synchronous thread
    thread = Thread(target=_sync_llm_thread)
    thread.start()

    # Asynchronously consume from the queue
    while True:
        chunk = await queue.get()
        if chunk is None:  # End of stream signal
            queue.task_done()
            break
        yield chunk
        queue.task_done()

    # Wait for the thread to finish (optional, ensures cleanup)
    loop = asyncio.get_running_loop()
    await loop.run_in_executor(None, thread.join)


def send_message_to_model_offline(
    messages: List[ChatMessage],
    loaded_model=None,
    model_name="bartowski/Meta-Llama-3.1-8B-Instruct-GGUF",
    temperature: float = 0.2,
    streaming=False,
    stop=[],
    max_prompt_size: int = None,
    response_type: str = "text",
    tracer: dict = {},
):
    assert loaded_model is None or isinstance(loaded_model, Llama), "loaded_model must be of type Llama, if configured"
    offline_chat_model = loaded_model or download_model(model_name, max_tokens=max_prompt_size)
    messages_dict = [{"role": message.role, "content": message.content} for message in messages]
    seed = int(os.getenv("KHOJ_LLM_SEED")) if os.getenv("KHOJ_LLM_SEED") else None
    response = offline_chat_model.create_chat_completion(
        messages_dict,
        stop=stop,
        stream=streaming,
        temperature=temperature,
        response_format={"type": response_type},
        seed=seed,
    )

    if streaming:
        return response

    response_text: str = response["choices"][0]["message"].get("content", "")

    # Save conversation trace for non-streaming responses
    # Streamed responses need to be saved by the calling function
    tracer["chat_model"] = model_name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(messages, response_text, tracer)

    return response_text
