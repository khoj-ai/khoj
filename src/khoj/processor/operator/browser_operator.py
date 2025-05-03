import asyncio
import base64
import json
import logging
import os
from copy import deepcopy
from datetime import datetime
from typing import Callable, List, Literal, Optional

import requests
from anthropic.types.beta import BetaContentBlock
from langchain.schema import ChatMessage
from openai.types.responses import ResponseOutputItem, response_computer_tool_call
from playwright.async_api import Page, async_playwright
from pydantic import BaseModel

from khoj.database.adapters import AgentAdapters, ConversationAdapters
from khoj.database.models import Agent, ChatModel, KhojUser
from khoj.processor.conversation.utils import commit_conversation_trace
from khoj.routers.helpers import ChatEvent
from khoj.utils.helpers import (
    convert_image_to_webp,
    get_anthropic_async_client,
    get_chat_usage_metrics,
    get_openai_async_client,
    is_promptrace_enabled,
    timer,
)
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


async def operate_browser(
    query: str,
    user: KhojUser,
    conversation_log: dict,
    location_data: LocationData,
    send_status_func: Optional[Callable] = None,
    query_images: Optional[List[str]] = None,
    agent: Agent = None,
    query_files: str = None,
    cancellation_event: Optional[asyncio.Event] = None,
    tracer: dict = {},
):
    response, safety_check_message = None, None

    # chat_model = await ConversationAdapters.aget_chat_model(user)
    agent_chat_model = await AgentAdapters.aget_agent_chat_model(agent, user) if agent else None
    chat_model: ChatModel = await ConversationAdapters.aget_default_chat_model(user, agent_chat_model)
    supported_operator_model_types = [ChatModel.ModelType.OPENAI, ChatModel.ModelType.ANTHROPIC]
    if not chat_model or chat_model.model_type not in supported_operator_model_types:
        # If a computer use capable model has not configured, return an unsupported on server error
        raise ValueError(
            f"Unsupported AI model. Configure and use chat model of type {supported_operator_model_types} to enable Browser use."
        )

    if send_status_func:
        async for event in send_status_func(f"**Launching Browser**:\n{query}"):
            yield {ChatEvent.STATUS: event}

    # Start the browser
    width, height = 1024, 768
    playwright, browser, page = await start_browser(width, height)

    # Operate the browser
    max_iterations = 40
    max_tokens = 4096
    compiled_operator_messages: List[ChatMessage] = []
    run_summarize = False
    task_completed = False
    iterations = 0
    messages = [{"role": "user", "content": query}]
    final_compiled_response = ""
    error_msg_template = "Browser use with {model_type} model failed due to an {error_type} error: {e}"

    with timer(f"Operating browser with {chat_model.model_type} {chat_model.name}", logger):
        try:
            while iterations < max_iterations:
                # Check for cancellation at the start of each iteration
                if cancellation_event and cancellation_event.is_set():
                    logger.info(f"Browser operator cancelled by client disconnect")
                    break

                iterations += 1
                tool_results = []
                compiled_response = ""

                if chat_model.model_type == ChatModel.ModelType.OPENAI:
                    (
                        agent_response,
                        compiled_response,
                        tool_results,
                        safety_check_message,
                    ) = await _openai_iteration(
                        messages=messages,
                        chat_model=chat_model,
                        page=page,
                        width=width,
                        height=height,
                        max_tokens=max_tokens,
                        max_iterations=max_iterations,
                        compiled_operator_messages=compiled_operator_messages,
                        tracer=tracer,
                    )
                    messages += agent_response.output
                    rendered_response = compiled_response
                    final_compiled_response = compiled_response

                elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                    (
                        agent_response,
                        compiled_response,
                        tool_results,
                        safety_check_message,
                    ) = await _anthropic_iteration(
                        messages=messages,
                        chat_model=chat_model,
                        page=page,
                        width=width,
                        height=height,
                        max_tokens=max_tokens,
                        max_iterations=max_iterations,
                        compiled_operator_messages=compiled_operator_messages,
                        tracer=tracer,
                    )
                    messages.append({"role": "assistant", "content": agent_response.content})
                    rendered_response = await render_claude_response(agent_response.content, page)
                    final_compiled_response = compiled_response

                if send_status_func:
                    async for event in send_status_func(f"**Operating Browser**:\n{rendered_response}"):
                        yield {ChatEvent.STATUS: event}

                # Check summarization conditions
                summarize_prompt = (
                    f"Collate all relevant information from your research so far to answer the target query:\n{query}."
                )
                task_completed = not tool_results and not run_summarize
                trigger_iteration_limit = iterations == max_iterations and not run_summarize

                if task_completed or trigger_iteration_limit:
                    iterations = max_iterations - 1  # Ensure one more iteration for summarization
                    run_summarize = True

                    # Model specific handling for appending the summarize prompt
                    if chat_model.model_type == ChatModel.ModelType.OPENAI:
                        # Pop the last tool result if max iterations reached and agent attempted a tool call
                        any_tool_calls = any(
                            block.type in ["computer_call", "function_call"] for block in agent_response.output
                        )
                        if trigger_iteration_limit and any_tool_calls and tool_results:
                            tool_results.pop()  # Remove the action that couldn't be processed due to limit

                        # Append summarize prompt
                        tool_results.append({"role": "user", "content": summarize_prompt})

                    elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                        # No specific action needed for Anthropic on iteration limit besides setting task_completed = False
                        # Append summarize prompt
                        tool_results.append({"type": "text", "text": summarize_prompt})

                # Add tool results to messages for the next iteration
                if tool_results:
                    if chat_model.model_type == ChatModel.ModelType.OPENAI:
                        messages += tool_results
                    elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                        # Mark the final tool result as a cache break point
                        tool_results[-1]["cache_control"] = {"type": "ephemeral"}
                        # Remove all previous cache break points
                        for msg in messages:
                            if isinstance(msg["content"], list):
                                for tool_result in msg["content"]:
                                    if isinstance(tool_result, dict) and "cache_control" in tool_result:
                                        del tool_result["cache_control"]
                            elif isinstance(msg["content"], dict) and "cache_control" in msg["content"]:
                                del msg["content"]["cache_control"]
                        messages.append({"role": "user", "content": tool_results})

                # Exit if safety checks are pending
                if safety_check_message:
                    break

            # Determine final response message
            if task_completed:
                response = final_compiled_response
            else:
                response = f"Operator hit iteration limit. If the results seems incomplete try again, assign a smaller task or try a different approach.\nThese were the results till now:\n{final_compiled_response}"

        except requests.RequestException as e:
            error_msg = error_msg_template.format(model_type=chat_model.model_type, error_type="network", e=e)
            raise ValueError(error_msg)
        except Exception as e:
            error_msg = error_msg_template.format(model_type=chat_model.model_type, error_type="unknown", e=e)
            logger.exception(error_msg)
            raise ValueError(error_msg)
        finally:
            if not safety_check_message:
                # Close the browser
                await browser.close()
                await playwright.stop()

    yield safety_check_message or response


async def start_browser(width: int = 1024, height: int = 768):
    playwright = await async_playwright().start()

    if cdp_url := os.getenv("KHOJ_CDP_URL"):
        browser = await playwright.chromium.connect_over_cdp(cdp_url)
    else:
        launch_args = [f"--window-size={width},{height}", "--disable-extensions", "--disable-file-system"]
        browser = await playwright.chromium.launch(chromium_sandbox=True, headless=False, args=launch_args, env={})

    # Get the initial browser, page or create one if none exist
    default_context = browser.contexts[0] if browser.contexts else await browser.new_context()
    page = default_context.pages[0] if default_context.pages else await default_context.new_page()

    # Define a handler for new pages
    async def handle_new_page(new_page: Page):
        # Get the target URL of the new page
        target_url = new_page.url
        # Close the new page if it is not closed
        if not new_page.is_closed():
            await new_page.close()
        # Open the target url in the current page instead
        if target_url and target_url != "about:blank":
            logger.debug(f"Load {target_url} in current page instead of new tab to stay in operator context.")
            await page.goto(target_url)

    # Listen for new pages being created in the context
    default_context.on("page", handle_new_page)

    # If page url is blank, navigate to DuckDuckGo
    if page.url == "about:blank":
        await page.goto("https://duckduckgo.com")
    await page.set_viewport_size({"width": width, "height": height})
    return playwright, browser, page


async def _openai_iteration(
    messages: list,
    chat_model: ChatModel,
    page: Page,
    width: int,
    height: int,
    max_tokens: int,
    max_iterations: int,
    compiled_operator_messages: List[ChatMessage],
    tracer: dict = {},
):
    """Performs one iteration of the OpenAI browser interaction loop."""
    client = get_openai_async_client(chat_model.ai_model_api.api_key, chat_model.ai_model_api.api_base_url)
    safety_check_prefix = "The user needs to say 'continue' after resolving the following safety checks to proceed:"
    safety_check = None
    system_prompt = f"""<SYSTEM_CAPABILITY>
* You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
* You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more.
* You can use the additional back() and goto() helper functions to navigate the browser. If you see nothing, try goto duckduckgo.com
* When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
* Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
* The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
</SYSTEM_CAPABILITY>

<IMPORTANT>
* You are allowed upto {max_iterations} iterations to complete the task.
</IMPORTANT>
"""

    # Configure tools available to Openai
    tools = [
        {
            "type": "computer_use_preview",
            "display_width": width,
            "display_height": height,
            "environment": "browser",
        },
        {
            "type": "function",
            "name": "back",
            "description": "Go back to the previous page.",
            "parameters": {},
        },
        {
            "type": "function",
            "name": "goto",
            "description": "Go to a specific URL.",
            "parameters": {
                "type": "object",
                "properties": {
                    "url": {
                        "type": "string",
                        "description": "Fully qualified URL to navigate to.",
                    },
                },
                "additionalProperties": False,
                "required": ["url"],
            },
        },
    ]

    response = await client.responses.create(
        model="computer-use-preview",
        input=messages,
        instructions=system_prompt,
        tools=tools,
        parallel_tool_calls=False,
        max_output_tokens=max_tokens,
        truncation="auto",
    )

    logger.debug(f"Openai response: {response.model_dump_json()}")
    compiled_response = compile_openai_response(response.output)

    # Add Openai's response to the tracer conversation history
    compiled_operator_messages.append(ChatMessage(role="assistant", content=compiled_response))

    # Check if any tools used
    tool_call_blocks = [
        block for block in response.output if block.type == "computer_call" or block.type == "function_call"
    ]
    tool_results: list[dict[str, str | dict]] = []
    last_call_id = None
    # Run the tool calls in order
    for block in tool_call_blocks:
        block_input: ActionBack | ActionGoto | response_computer_tool_call.Action = None
        if block.type == "function_call":
            last_call_id = block.call_id
            if hasattr(block, "name") and block.name == "goto":
                url = json.loads(block.arguments).get("url")
                block_input = ActionGoto(type="goto", url=url)
            elif hasattr(block, "name") and block.name == "back":
                block_input = ActionBack(type="back")
        # Exit tool processing if safety check needed
        elif block.type == "computer_call" and block.pending_safety_checks:
            for check in block.pending_safety_checks:
                if safety_check:
                    safety_check += f"\n- {check.message}"
                else:
                    safety_check = f"{safety_check_prefix}\n- {check.message}"
            break
        elif block.type == "computer_call":
            last_call_id = block.call_id
            block_input = block.action

        result = await handle_browser_action_openai(page, block_input)
        content_text = result.get("output") or result.get("error")
        compiled_operator_messages.append(ChatMessage(role="browser", content=content_text))

        # Take a screenshot after computer action
        if block.type == "computer_call":
            screenshot_base64 = await get_screenshot(page)
            content = {"type": "input_image", "image_url": f"data:image/webp;base64,{screenshot_base64}"}
            content["current_url"] = page.url if block.type == "computer_call" else None
        elif block.type == "function_call":
            content = content_text

        # Format the tool call results
        tool_results.append(
            {
                "type": f"{block.type}_output",
                "output": content,
                "call_id": last_call_id,
            }
        )

    # Calculate cost of chat
    input_tokens = response.usage.input_tokens
    output_tokens = response.usage.output_tokens
    tracer["usage"] = get_chat_usage_metrics(chat_model.name, input_tokens, output_tokens, usage=tracer.get("usage"))
    logger.debug(f"Operator usage by Openai: {tracer['usage']}")

    # Save conversation trace
    tracer["chat_model"] = chat_model.name
    if is_promptrace_enabled():
        commit_conversation_trace(compiled_operator_messages[:-1], compiled_operator_messages[-1].content, tracer)

    return response, compiled_response, tool_results, safety_check


async def _anthropic_iteration(
    messages: list,
    chat_model: ChatModel,
    page: Page,
    width: int,
    height: int,
    max_tokens: int,
    max_iterations: int,
    compiled_operator_messages: List[ChatMessage],
    thinking_budget: int | None = 1024,
    tracer: dict = {},
):
    """Performs one iteration of the Anthropic browser interaction loop."""
    client = get_anthropic_async_client(chat_model.ai_model_api.api_key, chat_model.ai_model_api.api_base_url)
    tool_version = "2025-01-24"
    betas = [f"computer-use-{tool_version}", "token-efficient-tools-2025-02-19"]
    temperature = 1.0
    safety_check = None  # Anthropic doesn't have explicit safety checks in the same way yet
    system_prompt = f"""<SYSTEM_CAPABILITY>
* You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
* You operate a Chromium browser using Playwright.
* You cannot access the OS or filesystem.
* You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more.
* You can use the additional back() and goto() helper functions to ease navigating the browser. If you see nothing, try goto duckduckgo.com
* When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
* When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
* Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
* The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
</SYSTEM_CAPABILITY>

<IMPORTANT>
* You are allowed upto {max_iterations} iterations to complete the task.
* Do not loop on wait, screenshot for too many turns without taking any action.
* After initialization if the browser is blank, enter a website URL using the goto() function instead of waiting
</IMPORTANT>
"""

    # Configure tools available to Claude
    tools = [
        {"type": f"computer_20250124", "name": "computer", "display_width_px": width, "display_height_px": height},
        {
            "name": "back",
            "description": "Go back to the previous page.",
            "input_schema": {
                "type": "object",
                "properties": {},
            },
        },
        {
            "name": "goto",
            "description": "Go to a specific URL.",
            "input_schema": {
                "type": "object",
                "properties": {
                    "url": {
                        "type": "string",
                        "description": "Fully qualified URL to navigate to.",
                    },
                },
                "required": ["url"],
            },
        },
        # {"type": f"text_editor_20250124", "name": "str_replace_editor"},
        # {"type": f"bash_20250124", "name": "bash"}
    ]

    # Set up optional thinking parameter
    thinking = {"type": "disabled"}
    if chat_model.name.startswith("claude-3-7") and thinking_budget:
        thinking = {"type": "enabled", "budget_tokens": thinking_budget}

    # Call the Claude API
    response = await client.beta.messages.create(
        messages=messages,
        model=chat_model.name,
        system=system_prompt,
        tools=tools,
        betas=betas,
        thinking=thinking,
        max_tokens=max_tokens,
        temperature=temperature,
    )

    response_content = response.content
    compiled_response = compile_claude_response(response_content)

    # Add Claude's response to the conversation history
    compiled_operator_messages.append(ChatMessage(role="assistant", content=compiled_response))
    logger.debug(f"Claude response: {response.model_dump_json()}")

    # Check if Claude used any tools
    tool_results = []
    for block in response_content:
        if block.type == "tool_use":
            if hasattr(block, "name") and block.name == "goto":
                block_input = {"action": block.name, "url": block.input.get("url")}
            elif hasattr(block, "name") and block.name == "back":
                block_input = {"action": block.name}
            else:
                block_input = block.input
            result = await handle_browser_action_anthropic(page, block_input)
            content = result.get("output") or result.get("error")
            if isinstance(content, str):
                compiled_operator_messages.append(ChatMessage(role="browser", content=content))
            elif isinstance(content, list) and content and content[0]["type"] == "image":
                compiled_operator_messages.append(ChatMessage(role="browser", content="[placeholder for screenshot]"))

            # Format the result for Claude
            tool_results.append(
                {
                    "type": "tool_result",
                    "tool_use_id": block.id,
                    "content": content,
                    "is_error": "error" in result,
                }
            )

    # Calculate cost of chat
    input_tokens = response.usage.input_tokens
    output_tokens = response.usage.output_tokens
    cache_read_tokens = response.usage.cache_read_input_tokens
    cache_write_tokens = response.usage.cache_creation_input_tokens
    tracer["usage"] = get_chat_usage_metrics(
        chat_model.name,
        input_tokens,
        output_tokens,
        cache_read_tokens,
        cache_write_tokens,
        usage=tracer.get("usage"),
    )
    logger.debug(f"Operator usage by {chat_model.model_type}: {tracer['usage']}")

    # Save conversation trace
    tracer["chat_model"] = chat_model.name
    tracer["temperature"] = temperature
    if is_promptrace_enabled():
        commit_conversation_trace(compiled_operator_messages[:-1], compiled_operator_messages[-1].content, tracer)

    return (
        response,
        compiled_response,
        tool_results,
        safety_check,
    )


async def browser_use_openai(*args, **kwargs):
    """
    Deprecated: Use operate_browser directly.
    This function is kept for potential backward compatibility checks but should be removed.
    """
    logger.warning("browser_use_openai is deprecated. Use operate_browser instead.")
    # The logic is now within operate_browser and _openai_iteration
    raise NotImplementedError("browser_use_openai is deprecated.")


async def browser_use_anthropic(*args, **kwargs):
    """
    Deprecated: Use operate_browser directly.
    This function is kept for potential backward compatibility checks but should be removed.
    """
    logger.warning("browser_use_anthropic is deprecated. Use operate_browser instead.")
    # The logic is now within operate_browser and _anthropic_iteration
    raise NotImplementedError("browser_use_anthropic is deprecated.")


# Mapping of CUA keys to Playwright keys
CUA_KEY_TO_PLAYWRIGHT_KEY = {
    "/": "Divide",
    "\\": "Backslash",
    "alt": "Alt",
    "arrowdown": "ArrowDown",
    "arrowleft": "ArrowLeft",
    "arrowright": "ArrowRight",
    "arrowup": "ArrowUp",
    "backspace": "Backspace",
    "capslock": "CapsLock",
    "cmd": "Meta",
    "ctrl": "Control",
    "delete": "Delete",
    "end": "End",
    "enter": "Enter",
    "return": "Enter",
    "esc": "Escape",
    "home": "Home",
    "insert": "Insert",
    "option": "Alt",
    "pagedown": "PageDown",
    "pageup": "PageUp",
    "shift": "Shift",
    "space": " ",
    "super": "Meta",
    "tab": "Tab",
    "win": "Meta",
}


AnthropicAction = Literal[
    "key",
    "type",
    "mouse_move",
    "left_click",
    "left_click_drag",
    "right_click",
    "middle_click",
    "double_click",
    "screenshot",
    "cursor_position",
    "left_mouse_down",
    "left_mouse_up",
    "scroll",
    "hold_key",
    "wait",
    "triple_click",
]


def parse_key_combination(text: str) -> list[str]:
    """
    Parse an xdotool-style key combination (e.g., "ctrl+o", "shift+tab")
    and return a list of Playwright-compatible key names.
    """
    if "+" in text:
        keys = text.split("+")
        # Map each key to its Playwright equivalent
        return [CUA_KEY_TO_PLAYWRIGHT_KEY.get(k.lower(), k) for k in keys]
    else:
        # Single key
        return [CUA_KEY_TO_PLAYWRIGHT_KEY.get(text.lower(), text)]


class ActionBack(BaseModel):
    type: Literal["back"]
    """Specifies the event type.

    For a back action, this property is always set to `back`.
    """


class ActionGoto(BaseModel):
    type: Literal["goto"]
    """Specifies the event type.

    For a goto action, this property is always set to `goto`.
    """
    url: str
    """The URL to navigate to.

    This property is required for the `goto` action.
    """


async def handle_browser_action_openai(
    page: Page, action: response_computer_tool_call.Action | ActionBack | ActionGoto
) -> dict[str, str]:
    """
    Given a computer action (e.g., click, double_click, scroll, etc.),
    execute the corresponding operation on the Playwright page.
    """
    action_type = action.type
    action_error = None

    try:
        match action_type:
            case "click":
                x, y = action.x, action.y
                button = action.button
                match button:
                    case "wheel":
                        await page.mouse.wheel(x, y)
                        logger.debug(f"Action: Click {button} and scroll by ({x}, {y})")
                        return {"output": f"Click {button} and Scroll by ({x}, {y})"}
                    case _:
                        button_mapping = {"left": "left", "right": "right"}
                        button_type = button_mapping.get(button, "left")
                        await page.mouse.click(x, y, button=button_type)
                        logger.debug(f"Action: {button.capitalize()} click at ({x}, {y})")
                        return {"output": f"{button_type} clicked at ({x}, {y})"}

            case "double_click":
                await page.mouse.dblclick(x, y)
                return {"output": f"Double clicked at ({x}, {y})"}

            case "scroll":
                x, y = action.x, action.y
                scroll_x, scroll_y = action.scroll_x, action.scroll_y
                await page.mouse.move(x, y)
                await page.evaluate(f"window.scrollBy({scroll_x}, {scroll_y})")
                logger.debug(f"Action: scroll at ({x}, {y}) with offsets (scroll_x={scroll_x}, scroll_y={scroll_y})")
                return {"output": f"Scroll at ({x}, {y}) with offsets (scroll_x={scroll_x}, scroll_y={scroll_y})"}

            case "keypress":
                keys = action.keys
                for key in keys:
                    logger.debug(f"Action: keypress '{key}'")
                    mapped_key = CUA_KEY_TO_PLAYWRIGHT_KEY.get(key.lower(), key)
                    await page.keyboard.press(mapped_key)
                return {"output": f"Pressed key: {text}"}

            case "type":
                text = action.text
                await page.keyboard.type(text)
                logger.debug(f"Action: type '{text}'")
                return {"output": f"Typed text: {text}"}

            case "wait":
                duration = 2
                await asyncio.sleep(duration)
                return {"output": f"Waited for {duration} seconds"}

            case "screenshot":
                # Nothing to do as screenshot is taken at each turn
                logger.debug(f"Action: screenshot")
                return {"output": "[placeholder for screenshot]"}

            case "move":
                x, y = action.x, action.y
                logger.debug(f"Action: move to ({x}, {y})")
                await page.mouse.move(x, y)
                return {"output": f"Moved mouse to ({x}, {y})"}

            case "drag":
                path = action.path
                logger.debug(f"Action: drag with path: {path}")
                if not path:
                    return {"error": "Missing path for drag action"}
                await page.mouse.move(path[0].x, path[0].y)
                await page.mouse.down()
                for point in path[1:]:
                    await page.mouse.move(point.x, point.y)
                await page.mouse.up()
                return {"output": f"Drag along path {path}"}

            case "goto":
                url = action.url
                if not url:
                    return {"error": "Missing URL for goto action"}
                await page.goto(url)
                return {"output": f"Navigated to {url}"}

            case "back":
                await page.go_back()
                return {"output": "Navigated back to the previous page."}

            case _:
                action_error = f"Unrecognized action: {action}"
                logger.warning(action_error)
        return {"error": action_error}
    except Exception as e:
        action_error = f"Error handling action {action}: {e}"
        logger.error(action_error)
        return {"error": action_error}


def compile_openai_response(response_content: list[ResponseOutputItem]) -> str:
    """
    Compile the response from Open AI model into a single string.
    """
    compiled_response = [""]
    for block in response_content:
        if block.type == "message":
            compiled_response.append(block.model_dump_json())
        elif block.type == "function_call":
            if block.name == "goto":
                if isinstance(block.arguments, str):
                    block_args = json.loads(block.arguments)
                block_input = {"action": block.name, "url": block_args.get("url")}
            elif hasattr(block, "name") and block.name == "back":
                block_input = {"action": block.name}
            compiled_response.append(f"**Action**: {json.dumps(block_input)}")
        elif block.type == "computer_call":
            block_input = block.action
            compiled_response.append(f"**Action**: {block_input.model_dump_json()}")
        elif block.type == "reasoning" and block.summary:
            compiled_response.append(f"**Thought**: {block.summary}")
    return "\n- ".join(compiled_response)


async def handle_browser_action_anthropic(page: Page, action_input: dict):
    """
    Given a computer action from Anthropic's computer use API,
    execute the corresponding operation on the Playwright page.
    """
    action_type = action_input.get("action")
    if not action_type:
        return {"error": "Missing action type in input"}

    try:
        render_action_input = action_input.copy()
        if render_action_input.get("image"):
            render_action_input["image"] = "[placeholder for screenshot data]"
        logger.debug(f"Anthropic Action: {action_type} with input: {render_action_input}")

        match action_type:
            case "mouse_move":
                if not action_input.get("coordinate"):
                    return {"error": "Missing coordinate for mouse_move"}
                x, y = action_input["coordinate"]
                await page.mouse.move(x, y)
                return {"output": f"Moved mouse to ({x}, {y})"}

            case "left_click":
                coordinate = action_input.get("coordinate")
                if coordinate:
                    x, y = coordinate
                    await page.mouse.move(x, y)
                # Handle optional key modifier
                key = action_input.get("key")
                if key:
                    mapped_key = CUA_KEY_TO_PLAYWRIGHT_KEY.get(key.lower(), key)
                    await page.keyboard.down(mapped_key)
                await page.mouse.click(x, y, button="left")
                if key:
                    await page.keyboard.up(mapped_key)
                return {"output": f"Left clicked at ({x}, {y})"}

            case "right_click":
                coordinate = action_input.get("coordinate")
                if coordinate:
                    x, y = coordinate
                    await page.mouse.move(x, y)
                await page.mouse.click(x, y, button="right")
                return {"output": f"Right clicked at ({x}, {y})"}

            case "middle_click":
                coordinate = action_input.get("coordinate")
                if coordinate:
                    coordinate = json.loads(coordinate) if isinstance(coordinate, str) else coordinate
                    x, y = coordinate
                    await page.mouse.move(x, y)
                await page.mouse.click(x, y, button="middle")
                return {"output": f"Middle clicked at ({x}, {y})"}

            case "double_click":
                coordinate = action_input.get("coordinate")
                if coordinate:
                    coordinate = json.loads(coordinate) if isinstance(coordinate, str) else coordinate
                    x, y = coordinate
                    await page.mouse.move(x, y)
                await page.mouse.dblclick(x, y)
                return {"output": f"Double clicked at ({x}, {y})"}

            case "triple_click":
                coordinate = action_input.get("coordinate")
                if coordinate:
                    coordinate = json.loads(coordinate) if isinstance(coordinate, str) else coordinate
                    x, y = coordinate
                    await page.mouse.move(x, y)
                # Playwright doesn't have a triple-click method, so we implement it manually
                await page.mouse.click(x, y, click_count=3)
                return {"output": f"Triple clicked at ({x}, {y})"}

            case "left_click_drag":
                if not action_input.get("coordinate"):
                    return {"error": "Missing coordinate for left_click_drag"}
                coordinate = action_input["coordinate"]
                coordinate = json.loads(coordinate) if isinstance(coordinate, str) else coordinate
                x, y = action_input["coordinate"]
                await page.mouse.move(x, y)
                await page.mouse.down()
                # We'd need to get the target position, but it's not clear from the reference how this is passed
                # For now, we'll end the drag at the same position
                await page.mouse.up()
                return {"output": f"Drag operation from ({x}, {y})"}

            case "left_mouse_down":
                await page.mouse.down(button="left")
                return {"output": "Left mouse button down"}

            case "left_mouse_up":
                await page.mouse.up(button="left")
                return {"output": "Left mouse button up"}

            case "type":
                text = action_input.get("text")
                if not text:
                    return {"error": "Missing text for type action"}
                await page.keyboard.type(text)
                return {"output": f"Typed text: {text}"}

            case "scroll":
                scroll_direction = action_input.get("scroll_direction")
                scroll_amount = action_input.get("scroll_amount", 1)
                if not scroll_direction:
                    return {"error": "Missing scroll_direction for scroll action"}

                coordinate = action_input.get("coordinate")
                if coordinate:
                    coordinate = json.loads(coordinate) if isinstance(coordinate, str) else coordinate
                    x, y = coordinate
                    await page.mouse.move(x, y)

                # Convert direction to x/y values
                dx, dy = 0, 0
                if scroll_direction == "up":
                    dy = -100 * scroll_amount
                elif scroll_direction == "down":
                    dy = 100 * scroll_amount
                elif scroll_direction == "left":
                    dx = -100 * scroll_amount
                elif scroll_direction == "right":
                    dx = 100 * scroll_amount

                await page.mouse.wheel(dx, dy)
                return {"output": f"Scrolled {scroll_direction} by {scroll_amount}"}

            case "key":
                text = action_input.get("text")
                if not text:
                    return {"error": "Missing text for key action"}

                # Parse xdotool style key combinations
                keys = parse_key_combination(text)

                if len(keys) > 1:
                    # For key combinations (e.g., ctrl+a)
                    # Press all modifier keys down
                    for key in keys[:-1]:
                        await page.keyboard.down(key)

                    # Press and release the last key
                    await page.keyboard.press(keys[-1])

                    # Release all modifier keys in reverse order
                    for key in reversed(keys[:-1]):
                        await page.keyboard.up(key)
                else:
                    # For single keys
                    await page.keyboard.press(keys[0])

                logger.debug(f"Pressed key: {keys} from original: {text}")
                return {"output": f"Pressed key: {text}"}

            case "hold_key":
                text = action_input.get("text")
                duration = action_input.get("duration", 1)
                if not text:
                    return {"error": "Missing text for hold_key action"}

                # Parse and handle xdotool style key combinations
                keys = parse_key_combination(text)

                # Press all keys down
                for key in keys:
                    await page.keyboard.down(key)

                # Hold for duration
                await asyncio.sleep(duration)

                # Release all keys in reverse order
                for key in reversed(keys):
                    await page.keyboard.up(key)

                return {"output": f"Held key{'s' if len(keys) > 1 else ''} {text} for {duration} seconds"}

            case "wait":
                duration = action_input.get("duration", 1)
                await asyncio.sleep(duration)
                return {"output": f"Waited for {duration} seconds"}

            case "screenshot":
                screenshot_base64 = await get_screenshot(page)
                return {
                    "output": [
                        {
                            "type": "image",
                            "source": {
                                "type": "base64",
                                "media_type": "image/webp",
                                "data": screenshot_base64,
                            },
                        }
                    ]
                }

            case "cursor_position":
                mouse_position = await page.evaluate("({x: window.mouseX, y: window.mouseY})")
                return {"output": f"Cursor position: {mouse_position}"}

            case "goto":
                url = action_input.get("url")
                if not url:
                    return {"error": "Missing URL for goto action"}
                await page.goto(url)
                return {"output": f"Navigated to {url}"}

            case "back":
                await page.go_back()
                return {"output": "Navigated back to the previous page."}

            case _:
                return {"error": f"Unrecognized action: {action_type}"}

    except Exception as e:
        error_message = f"Error handling action {action_type}: {str(e)}"
        logger.error(error_message)
        return {"error": error_message}


def compile_claude_response(response_content: list[BetaContentBlock]) -> str:
    """
    Compile the response from Anthropic AI model into a single string.
    """
    compiled_response = [""]
    for block in response_content:
        if block.type == "text":
            compiled_response.append(block.text)
        elif block.type == "tool_use":
            if hasattr(block, "name") and block.name == "goto":
                block_input = {"action": block.name, "url": block.input.get("url")}
            elif hasattr(block, "name") and block.name == "back":
                block_input = {"action": block.name}
            else:
                block_input = block.input
            compiled_response.append(f"**Action**: {json.dumps(block_input)}")
        elif block.type == "thinking":
            compiled_response.append(f"**Thought**: {block.thinking}")
    return "\n- ".join(compiled_response)


async def render_claude_response(response_content: list[BetaContentBlock], page: Page) -> str:
    """
    Share the response from Anthropic AI model to be rendered by the client.
    """
    compiled_response = [""]
    for block in deepcopy(response_content):
        if block.type == "text":
            compiled_response.append(block.text)
        elif block.type == "tool_use":
            if hasattr(block, "name") and block.name == "goto":
                block_input = {"action": block.name, "url": block.input.get("url")}
            elif hasattr(block, "name") and block.name == "back":
                block_input = {"action": block.name}
            else:
                block_input = block.input

            if block_input.get("action") == "screenshot":
                screenshot_base64 = await get_screenshot(page)
                block_input["image"] = f"data:image/webp;base64,{screenshot_base64}"

            compiled_response.append(f"**Action**: {json.dumps(block_input)}")
        elif block.type == "thinking":
            compiled_response.append(f"**Thought**: {block.thinking}")
    return "\n- ".join(compiled_response)


async def get_screenshot(page: Page):
    """
    Take a viewport screenshot using Playwright and return as base64 encoded webp image.
    """
    screenshot_bytes = await page.screenshot(caret="initial", full_page=False, type="png")
    screenshot_webp_bytes = convert_image_to_webp(screenshot_bytes)
    return base64.b64encode(screenshot_webp_bytes).decode("utf-8")
