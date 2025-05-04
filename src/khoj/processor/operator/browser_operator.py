import asyncio
import base64
import json
import logging
import os
from abc import ABC, abstractmethod
from copy import deepcopy
from datetime import datetime
from typing import Callable, List, Literal, Optional, Union

import requests
from anthropic.types.beta import BetaContentBlock, BetaMessage
from langchain.schema import ChatMessage
from openai.types.responses import Response, ResponseOutputItem
from playwright.async_api import Browser, Page, Playwright, async_playwright
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


# --- Standardized Action Models ---
class Point(BaseModel):
    x: int
    y: int


class BaseAction(BaseModel):
    type: str


class ClickAction(BaseAction):
    type: Literal["click"] = "click"
    x: int
    y: int
    button: Literal["left", "right", "middle", "wheel"] = "left"
    modifiers: str = None


class DoubleClickAction(BaseAction):
    type: Literal["double_click"] = "double_click"
    x: int
    y: int


class TripleClickAction(BaseAction):
    type: Literal["triple_click"] = "triple_click"
    x: int
    y: int


class ScrollAction(BaseAction):
    type: Literal["scroll"] = "scroll"
    x: Optional[int] = None
    y: Optional[int] = None
    scroll_x: Optional[int] = None
    scroll_y: Optional[int] = None
    scroll_direction: Optional[Literal["up", "down", "left", "right"]] = None
    scroll_amount: Optional[int] = 1


class KeypressAction(BaseAction):
    type: Literal["keypress"] = "keypress"
    keys: List[str]  # Standardized on list of keys


class TypeAction(BaseAction):
    type: Literal["type"] = "type"
    text: str


class WaitAction(BaseAction):
    type: Literal["wait"] = "wait"
    duration: float = 1.0


class ScreenshotAction(BaseAction):
    type: Literal["screenshot"] = "screenshot"


class MoveAction(BaseAction):
    type: Literal["move"] = "move"
    x: int
    y: int


class DragAction(BaseAction):
    type: Literal["drag"] = "drag"
    path: List[Point]


class MouseDownAction(BaseAction):
    type: Literal["mouse_down"] = "mouse_down"
    button: Literal["left", "right", "middle"] = "left"


class MouseUpAction(BaseAction):
    type: Literal["mouse_up"] = "mouse_up"
    button: Literal["left", "right", "middle"] = "left"


class HoldKeyAction(BaseAction):
    type: Literal["hold_key"] = "hold_key"
    text: str  # xdotool style key combination string
    duration: float = 1.0


class CursorPositionAction(BaseAction):
    type: Literal["cursor_position"] = "cursor_position"


class GotoAction(BaseAction):
    type: Literal["goto"] = "goto"
    url: str


class BackAction(BaseAction):
    type: Literal["back"] = "back"


BrowserAction = Union[
    ClickAction,
    DoubleClickAction,
    TripleClickAction,
    ScrollAction,
    KeypressAction,
    TypeAction,
    WaitAction,
    ScreenshotAction,
    MoveAction,
    DragAction,
    MouseDownAction,
    MouseUpAction,
    HoldKeyAction,
    CursorPositionAction,
    GotoAction,
    BackAction,
]


class EnvState(BaseModel):
    url: str
    screenshot: Optional[str] = None


class StepResult(BaseModel):
    output: Optional[str | List[dict]] = None  # Output message or screenshot data
    error: Optional[str] = None
    current_url: Optional[str] = None
    screenshot_base64: Optional[str] = None


class AgentActResult(BaseModel):
    compiled_response: str
    actions: List[BrowserAction] = []
    tool_results_for_history: List[dict] = []  # Model-specific format for history
    raw_agent_response: BetaMessage | Response  # Store the raw response for history formatting
    usage: dict = {}
    safety_check_message: Optional[str] = None


# --- Abstract Classes ---
class Environment(ABC):
    @abstractmethod
    async def start(self, width: int, height: int) -> None:
        pass

    @abstractmethod
    async def step(self, action: BrowserAction) -> StepResult:
        pass

    @abstractmethod
    async def close(self) -> None:
        pass

    @abstractmethod
    async def get_state(self) -> EnvState:
        pass


class OperatorAgent(ABC):
    def __init__(self, chat_model: ChatModel, max_iterations: int, tracer: dict):
        self.chat_model = chat_model
        self.max_iterations = max_iterations
        self.tracer = tracer
        self.compiled_operator_messages: List[ChatMessage] = []

    @abstractmethod
    async def act(self, messages: List[dict], current_state: EnvState) -> AgentActResult:
        pass

    def _update_usage(self, input_tokens: int, output_tokens: int, cache_read: int = 0, cache_write: int = 0):
        self.tracer["usage"] = get_chat_usage_metrics(
            self.chat_model.name, input_tokens, output_tokens, cache_read, cache_write, usage=self.tracer.get("usage")
        )
        logger.debug(f"Operator usage by {self.chat_model.model_type}: {self.tracer['usage']}")

    def _commit_trace(self):
        self.tracer["chat_model"] = self.chat_model.name
        if is_promptrace_enabled() and self.compiled_operator_messages:
            commit_conversation_trace(
                self.compiled_operator_messages[:-1], self.compiled_operator_messages[-1].content, self.tracer
            )


# --- Concrete BrowserEnvironment ---
class BrowserEnvironment(Environment):
    def __init__(self):
        self.playwright: Optional[Playwright] = None
        self.browser: Optional[Browser] = None
        self.page: Optional[Page] = None
        self.width: int = 1024
        self.height: int = 768

    async def start(self, width: int = 1024, height: int = 768) -> None:
        self.width = width
        self.height = height
        self.playwright = await async_playwright().start()

        if cdp_url := os.getenv("KHOJ_CDP_URL"):
            self.browser = await self.playwright.chromium.connect_over_cdp(cdp_url)
        else:
            launch_args = [f"--window-size={width},{height}", "--disable-extensions", "--disable-file-system"]
            self.browser = await self.playwright.chromium.launch(
                chromium_sandbox=True, headless=False, args=launch_args, env={}
            )

        # Get the initial browser, page or create one if none exist
        default_context = self.browser.contexts[0] if self.browser.contexts else await self.browser.new_context()
        self.page = default_context.pages[0] if default_context.pages else await default_context.new_page()

        # Define a handler for new pages
        async def handle_new_page(new_page: Page):
            # Get the target URL of the new page
            target_url = new_page.url
            # Close the new page if it is not closed
            if not new_page.is_closed():
                await new_page.close()
            # Open the target url in the current page instead
            if target_url and target_url != "about:blank" and self.page:
                logger.debug(f"Load {target_url} in current page instead of new tab.")
                await self.page.goto(target_url)

        # Listen for new pages being created in the context
        default_context.on("page", handle_new_page)

        # If page url is blank, navigate to DuckDuckGo
        if self.page.url == "about:blank":
            await self.page.goto("https://duckduckgo.com")
        await self.page.set_viewport_size({"width": self.width, "height": self.height})
        logger.info("Browser environment started.")

    async def _get_screenshot(self) -> Optional[str]:
        if not self.page or self.page.is_closed():
            return None
        try:
            screenshot_bytes = await self.page.screenshot(caret="initial", full_page=False, type="png")
            screenshot_webp_bytes = convert_image_to_webp(screenshot_bytes)
            return base64.b64encode(screenshot_webp_bytes).decode("utf-8")
        except Exception as e:
            logger.error(f"Failed to get screenshot: {e}")
            return None

    async def get_state(self) -> EnvState:
        if not self.page or self.page.is_closed():
            return "about:blank", None
        url = self.page.url
        screenshot = await self._get_screenshot()
        return EnvState(url=url, screenshot=screenshot)

    async def step(self, action: BrowserAction) -> StepResult:
        if not self.page or self.page.is_closed():
            return StepResult(error="Browser page is not available or closed.")

        output, error = None, None
        try:
            match action.type:
                case "click":
                    x, y, button = action.x, action.y, action.button
                    if button == "wheel":
                        await self.page.mouse.wheel(x, y)
                        output = f"Scrolled wheel at ({x}, {y})"
                    else:
                        modifiers = self.parse_key_combination(action.modifiers) if action.modifiers else []
                        for modifier in modifiers:
                            await self.page.keyboard.down(modifier)
                        await self.page.mouse.click(x, y, button=button)
                        for modifier in reversed(modifiers):
                            await self.page.keyboard.up(modifier)
                        output = f"{button.capitalize()} clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} {button} at ({x},{y})")

                case "double_click":
                    x, y = action.x, action.y
                    await self.page.mouse.dblclick(x, y)
                    output = f"Double clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "triple_click":
                    x, y = action.x, action.y
                    await self.page.mouse.click(x, y, click_count=3)
                    output = f"Triple clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "scroll":
                    # Prefer explicit scroll_x/y if provided (from OpenAI style)
                    if action.scroll_x is not None or action.scroll_y is not None:
                        scroll_x = action.scroll_x or 0
                        scroll_y = action.scroll_y or 0
                        if action.x is not None and action.y is not None:
                            await self.page.mouse.move(action.x, action.y)
                        await self.page.evaluate(f"window.scrollBy({scroll_x}, {scroll_y})")
                        output = f"Scrolled by ({scroll_x}, {scroll_y})"
                        logger.debug(f"Action: {action.type} by ({scroll_x},{scroll_y}) at ({action.x},{action.y})")
                    # Otherwise use direction/amount (from Anthropic style)
                    elif action.scroll_direction:
                        dx, dy = 0, 0
                        amount = action.scroll_amount or 1
                        if action.scroll_direction == "up":
                            dy = -100 * amount
                        elif action.scroll_direction == "down":
                            dy = 100 * amount
                        elif action.scroll_direction == "left":
                            dx = -100 * amount
                        elif action.scroll_direction == "right":
                            dx = 100 * amount

                        if action.x is not None and action.y is not None:
                            await self.page.mouse.move(action.x, action.y)
                        await self.page.mouse.wheel(dx, dy)
                        output = f"Scrolled {action.scroll_direction} by {amount}"
                        logger.debug(
                            f"Action: {action.type} {action.scroll_direction} by {amount} at ({action.x},{action.y})"
                        )
                    else:
                        error = "Scroll action requires either scroll_x/y or scroll_direction"

                case "keypress":
                    keys = action.keys
                    if len(keys) > 1:  # Handle combinations like ctrl+a
                        modifiers = [BrowserEnvironment.CUA_KEY_TO_PLAYWRIGHT_KEY.get(k.lower(), k) for k in keys[:-1]]
                        main_key = BrowserEnvironment.CUA_KEY_TO_PLAYWRIGHT_KEY.get(keys[-1].lower(), keys[-1])
                        key_string = "+".join(modifiers + [main_key])
                        await self.page.keyboard.press(key_string)
                    elif keys:  # Single key
                        key_string = BrowserEnvironment.CUA_KEY_TO_PLAYWRIGHT_KEY.get(keys[0].lower(), keys[0])
                        await self.page.keyboard.press(key_string)
                    else:
                        error = "Keypress action requires at least one key"
                        key_string = "N/A"
                    output = f"Pressed key(s): {key_string}"
                    logger.debug(f"Action: {action.type} '{key_string}'")

                case "type":
                    text = action.text
                    await self.page.keyboard.type(text)
                    output = f"Typed text: {text}"
                    logger.debug(f"Action: {action.type} '{text}'")

                case "wait":
                    duration = action.duration
                    await asyncio.sleep(duration)
                    output = f"Waited for {duration} seconds"
                    logger.debug(f"Action: {action.type} for {duration}s")

                case "screenshot":
                    # Screenshot is taken after every step, so this action might just confirm it
                    output = "[Screenshot taken]"
                    logger.debug(f"Action: {action.type}")

                case "move":
                    x, y = action.x, action.y
                    await self.page.mouse.move(x, y)
                    output = f"Moved mouse to ({x}, {y})"
                    logger.debug(f"Action: {action.type} to ({x},{y})")

                case "drag":
                    path = action.path
                    if not path:
                        error = "Missing path for drag action"
                    else:
                        await self.page.mouse.move(path[0].x, path[0].y)
                        await self.page.mouse.down()
                        for point in path[1:]:
                            await self.page.mouse.move(point.x, point.y)
                        await self.page.mouse.up()
                        output = f"Drag along path starting at ({path[0].x},{path[0].y})"
                        logger.debug(f"Action: {action.type} with {len(path)} points")

                case "mouse_down":
                    await self.page.mouse.down(button=action.button)
                    output = f"{action.button.capitalize()} mouse button down"
                    logger.debug(f"Action: {action.type} {action.button}")

                case "mouse_up":
                    await self.page.mouse.up(button=action.button)
                    output = f"{action.button.capitalize()} mouse button up"
                    logger.debug(f"Action: {action.type} {action.button}")

                case "hold_key":
                    keys_to_parse = action.text
                    duration = action.duration
                    keys = self.parse_key_combination(keys_to_parse)
                    for key in keys:
                        await self.page.keyboard.down(key)
                    await asyncio.sleep(duration)
                    for key in reversed(keys):
                        await self.page.keyboard.up(key)
                    output = f"Held key{'s' if len(keys) > 1 else ''} {keys_to_parse} for {duration} seconds"
                    logger.debug(f"Action: {action.type} '{keys_to_parse}' for {duration}s")

                case "cursor_position":
                    # Playwright doesn't directly expose mouse position easily without JS injection
                    # Returning a placeholder for now
                    output = "Cursor position requested (not directly available)"
                    logger.debug(f"Action: {action.type}")

                case "goto":
                    url = action.url
                    if not url:
                        error = "Missing URL for goto action"
                    else:
                        await self.page.goto(url)
                        output = f"Navigated to {url}"
                        logger.debug(f"Action: {action.type} to {url}")

                case "back":
                    await self.page.go_back()
                    output = "Navigated back"
                    logger.debug(f"Action: {action.type}")

                case _:
                    error = f"Unrecognized action type: {action.type}"
                    logger.warning(error)

        except Exception as e:
            error = f"Error executing action {action.type}: {e}"
            logger.exception(f"Error during step execution for action: {action.model_dump_json()}")

        state = await self.get_state()

        # Special handling for screenshot action result to include image data
        if action.type == "screenshot" and state.screenshot:
            output = [
                {
                    "type": "image",
                    "source": {
                        "type": "base64",
                        "media_type": "image/webp",
                        "data": state.screenshot,
                    },
                }
            ]

        return StepResult(
            output=output,
            error=error,
            current_url=state.url,
            screenshot_base64=state.screenshot,
        )

    async def close(self) -> None:
        if self.browser:
            await self.browser.close()
            logger.info("Browser closed.")
        if self.playwright:
            await self.playwright.stop()
            logger.info("Playwright stopped.")
        self.browser = None
        self.playwright = None
        self.page = None

    # Mapping of Operator Agent keys to Playwright keys
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
        "ctrl": "ControlOrMeta",
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

    @staticmethod
    def parse_key_combination(text: str) -> list[str]:
        """
        Parse an xdotool-style key combination (e.g., "ctrl+o", "shift+tab")
        and return a list of Playwright-compatible key names.
        """
        if "+" in text:
            keys = text.split("+")
            # Map each key to its Playwright equivalent
            return [BrowserEnvironment.CUA_KEY_TO_PLAYWRIGHT_KEY.get(k.lower(), k) for k in keys]
        else:
            # Single key
            return [BrowserEnvironment.CUA_KEY_TO_PLAYWRIGHT_KEY.get(text.lower(), text)]


# --- Concrete Operator Agents ---
class OpenAIOperatorAgent(OperatorAgent):
    async def act(self, messages: List[dict], current_state: EnvState) -> AgentActResult:
        client = get_openai_async_client(
            self.chat_model.ai_model_api.api_key, self.chat_model.ai_model_api.api_base_url
        )
        safety_check_prefix = "The user needs to say 'continue' after resolving the following safety checks to proceed:"
        safety_check_message = None
        actions: List[BrowserAction] = []
        tool_results_for_history: List[dict] = []
        self._commit_trace()  # Commit trace before next action

        system_prompt = f"""<SYSTEM_CAPABILITY>
* You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
* You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more using the computer_use_preview tool.
* You can use the additional back() and goto() helper functions to navigate the browser. If you see nothing, try goto duckduckgo.com
* When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
* Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
* The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
* The current URL is {current_state.url}.
</SYSTEM_CAPABILITY>

<IMPORTANT>
* You are allowed upto {self.max_iterations} iterations to complete the task.
</IMPORTANT>
"""
        tools = [
            {
                "type": "computer_use_preview",
                "display_width": 1024,  # TODO: Get from env
                "display_height": 768,  # TODO: Get from env
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

        response: Response = await client.responses.create(
            model="computer-use-preview",
            input=messages,
            instructions=system_prompt,
            tools=tools,
            parallel_tool_calls=False,  # Keep sequential for now
            max_output_tokens=4096,  # TODO: Make configurable?
            truncation="auto",
        )

        logger.debug(f"Openai response: {response.model_dump_json()}")
        compiled_response = self.compile_openai_response(response.output)
        self.compiled_operator_messages.append(ChatMessage(role="assistant", content=compiled_response))

        last_call_id = None
        for block in response.output:
            action_to_run: Optional[BrowserAction] = None
            content_for_history: Optional[Union[str, dict]] = None

            if block.type == "function_call":
                last_call_id = block.call_id
                if block.name == "goto":
                    try:
                        args = json.loads(block.arguments)
                        url = args.get("url")
                        if url:
                            action_to_run = GotoAction(url=url)
                            content_for_history = (
                                f"Navigated to {url}"  # Placeholder, actual result comes from env.step
                            )
                        else:
                            logger.warning("Goto function called without URL argument.")
                    except json.JSONDecodeError:
                        logger.warning(f"Failed to parse arguments for goto: {block.arguments}")
                elif block.name == "back":
                    action_to_run = BackAction()
                    content_for_history = "Navigated back"  # Placeholder

            elif block.type == "computer_call":
                last_call_id = block.call_id
                if block.pending_safety_checks:
                    for check in block.pending_safety_checks:
                        if safety_check_message:
                            safety_check_message += f"\n- {check.message}"
                        else:
                            safety_check_message = f"{safety_check_prefix}\n- {check.message}"
                    break  # Stop processing actions if safety check needed

                openai_action = block.action
                content_for_history = "[placeholder for screenshot]"  # Placeholder

                # Convert OpenAI action to standardized BrowserAction
                action_type = openai_action.type
                try:
                    if action_type == "click":
                        action_to_run = ClickAction(x=openai_action.x, y=openai_action.y, button=openai_action.button)
                    elif action_type == "double_click":
                        action_to_run = DoubleClickAction(x=openai_action.x, y=openai_action.y)
                    elif action_type == "scroll":
                        action_to_run = ScrollAction(
                            x=openai_action.x,
                            y=openai_action.y,
                            scroll_x=openai_action.scroll_x,
                            scroll_y=openai_action.scroll_y,
                        )
                    elif action_type == "keypress":
                        action_to_run = KeypressAction(keys=openai_action.keys)
                    elif action_type == "type":
                        action_to_run = TypeAction(text=openai_action.text)
                    elif action_type == "wait":
                        action_to_run = WaitAction(duration=2.0)  # OpenAI doesn't specify duration, default?
                    elif action_type == "screenshot":
                        action_to_run = ScreenshotAction()
                    elif action_type == "move":
                        action_to_run = MoveAction(x=openai_action.x, y=openai_action.y)
                    elif action_type == "drag":
                        action_to_run = DragAction(path=[Point(x=p.x, y=p.y) for p in openai_action.path])
                    else:
                        logger.warning(f"Unsupported OpenAI computer action type: {action_type}")
                except Exception as e:
                    logger.error(f"Error converting OpenAI action {action_type}: {e}")

            if action_to_run:
                actions.append(action_to_run)
                # Prepare the result structure expected in the message history
                tool_results_for_history.append(
                    {
                        "type": f"{block.type}_output",
                        "output": content_for_history,  # This will be updated after env.step
                        "call_id": last_call_id,
                    }
                )

        self._update_usage(response.usage.input_tokens, response.usage.output_tokens)

        return AgentActResult(
            compiled_response=compiled_response,
            actions=actions,
            tool_results_for_history=tool_results_for_history,
            raw_agent_response=response,
            usage=self.tracer.get("usage", {}),
            safety_check_message=safety_check_message,
        )

    @staticmethod
    def compile_openai_response(response_content: list[ResponseOutputItem]) -> str:
        """Compile the response from Open AI model into a single string."""
        compiled_response = [""]
        for block in deepcopy(response_content):
            if block.type == "message":
                # Extract text content if available
                text_content = block.text if hasattr(block, "text") else block.model_dump_json()
                compiled_response.append(text_content)
            elif block.type == "function_call":
                block_input = {"action": block.name}
                if block.name == "goto":
                    try:
                        args = json.loads(block.arguments)
                        block_input["url"] = args.get("url", "[Missing URL]")
                    except json.JSONDecodeError:
                        block_input["arguments"] = block.arguments  # Show raw args on error
                compiled_response.append(f"**Action**: {json.dumps(block_input)}")
            elif block.type == "computer_call":
                block_input = block.action
                # If it's a screenshot action and we have a screenshot, render it
                if block_input.type == "screenshot":
                    block_input_render = block_input.model_dump()
                    block_input_render["image"] = "[placeholder for screenshot]"
                    compiled_response.append(f"**Action**: {json.dumps(block_input_render)}")
                else:
                    compiled_response.append(f"**Action**: {block_input.model_dump_json()}")
            elif block.type == "reasoning" and block.summary:
                compiled_response.append(f"**Thought**: {block.summary}")
        return "\n- ".join(filter(None, compiled_response))  # Filter out empty strings

    @staticmethod
    async def render_response(response_content: list[ResponseOutputItem], screenshot: Optional[str] = None) -> str:
        """Render OpenAI response for display, potentially including screenshots."""
        compiled_response = [""]
        for block in deepcopy(response_content):  # Use deepcopy to avoid modifying original
            if block.type == "message":
                text_content = block.text if hasattr(block, "text") else block.model_dump_json()
                compiled_response.append(text_content)
            elif block.type == "function_call":
                block_input = {"action": block.name}
                if block.name == "goto":
                    try:
                        args = json.loads(block.arguments)
                        block_input["url"] = args.get("url", "[Missing URL]")
                    except json.JSONDecodeError:
                        block_input["arguments"] = block.arguments
                compiled_response.append(f"**Action**: {json.dumps(block_input)}")
            elif block.type == "computer_call":
                block_input = block.action
                # If it's a screenshot action and we have a screenshot, render it
                if block_input.type == "screenshot":
                    block_input_render = block_input.model_dump()
                    if screenshot:
                        block_input_render["image"] = f"data:image/webp;base64,{screenshot}"
                    else:
                        block_input_render["image"] = "[Failed to get screenshot]"
                    compiled_response.append(f"**Action**: {json.dumps(block_input_render)}")
                else:
                    compiled_response.append(f"**Action**: {block_input.model_dump_json()}")
            elif block.type == "reasoning" and block.summary:
                compiled_response.append(f"**Thought**: {block.summary}")
        return "\n- ".join(filter(None, compiled_response))


class AnthropicOperatorAgent(OperatorAgent):
    async def act(self, messages: List[dict], current_state: EnvState) -> AgentActResult:
        client = get_anthropic_async_client(
            self.chat_model.ai_model_api.api_key, self.chat_model.ai_model_api.api_base_url
        )
        tool_version = "2025-01-24"
        betas = [f"computer-use-{tool_version}", "token-efficient-tools-2025-02-19"]
        temperature = 1.0
        actions: List[BrowserAction] = []
        tool_results_for_history: List[dict] = []
        self._commit_trace()  # Commit trace before next action

        system_prompt = f"""<SYSTEM_CAPABILITY>
* You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
* You operate a Chromium browser using Playwright via the 'computer' tool.
* You cannot access the OS or filesystem.
* You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more.
* You can use the additional back() and goto() helper functions to ease navigating the browser. If you see nothing, try goto duckduckgo.com
* When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
* When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
* Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
* The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
* The current URL is {current_state.url}.
</SYSTEM_CAPABILITY>

<IMPORTANT>
* You are allowed upto {self.max_iterations} iterations to complete the task.
* Do not loop on wait, screenshot for too many turns without taking any action.
* After initialization if the browser is blank, enter a website URL using the goto() function instead of waiting
</IMPORTANT>
"""
        # Add latest screenshot if available
        if current_state.screenshot:
            # Ensure last message content is a list
            if not isinstance(messages[-1]["content"], list):
                messages[-1]["content"] = [{"type": "text", "text": messages[-1]["content"]}]

        tools = [
            {
                "type": f"computer_20250124",
                "name": "computer",
                "display_width_px": 1024,
                "display_height_px": 768,
            },  # TODO: Get from env
            {
                "name": "back",
                "description": "Go back to the previous page.",
                "input_schema": {"type": "object", "properties": {}},
            },
            {
                "name": "goto",
                "description": "Go to a specific URL.",
                "input_schema": {
                    "type": "object",
                    "properties": {"url": {"type": "string", "description": "Fully qualified URL to navigate to."}},
                    "required": ["url"],
                },
            },
        ]

        thinking = {"type": "disabled"}
        if self.chat_model.name.startswith("claude-3-7"):
            thinking = {"type": "enabled", "budget_tokens": 1024}

        response = await client.beta.messages.create(
            messages=messages,
            model=self.chat_model.name,
            system=system_prompt,
            tools=tools,
            betas=betas,
            thinking=thinking,
            max_tokens=4096,  # TODO: Make configurable?
            temperature=temperature,
        )

        logger.debug(f"Anthropic response: {response.model_dump_json()}")
        compiled_response = self.compile_response(response.content)
        self.compiled_operator_messages.append(
            ChatMessage(role="assistant", content=compiled_response)
        )  # Add raw response text

        for block in response.content:
            if block.type == "tool_use":
                action_to_run: Optional[BrowserAction] = None
                tool_input = block.input
                tool_name = block.name
                tool_use_id = block.id
                content_for_history: Optional[Union[str, List[dict]]] = None

                try:
                    if tool_name == "computer":
                        action_type = tool_input.get("action")
                        content_for_history = "[placeholder for screenshot]"  # Default placeholder
                        if action_type == "mouse_move":
                            coord = tool_input.get("coordinate")
                            if coord:
                                action_to_run = MoveAction(x=coord[0], y=coord[1])
                        elif action_type == "left_click":
                            coord = tool_input.get("coordinate")
                            if coord:
                                action_to_run = ClickAction(
                                    x=coord[0], y=coord[1], button="left", modifier=tool_input.get("text")
                                )
                        elif action_type == "right_click":
                            coord = tool_input.get("coordinate")
                            if coord:
                                action_to_run = ClickAction(x=coord[0], y=coord[1], button="right")
                        elif action_type == "middle_click":
                            coord = tool_input.get("coordinate")
                            if coord:
                                action_to_run = ClickAction(x=coord[0], y=coord[1], button="middle")
                        elif action_type == "double_click":
                            coord = tool_input.get("coordinate")
                            if coord:
                                action_to_run = DoubleClickAction(x=coord[0], y=coord[1])
                        elif action_type == "triple_click":
                            coord = tool_input.get("coordinate")
                            if coord:
                                action_to_run = TripleClickAction(x=coord[0], y=coord[1])
                        elif action_type == "left_click_drag":
                            start_coord = tool_input.get("start_coordinate")
                            end_coord = tool_input.get("coordinate")
                            if start_coord and end_coord:
                                action_to_run = DragAction(
                                    path=[Point(x=p[0], y=p[1]) for p in [start_coord, end_coord]]
                                )
                        elif action_type == "left_mouse_down":
                            action_to_run = MouseDownAction(button="left")
                        elif action_type == "left_mouse_up":
                            action_to_run = MouseUpAction(button="left")
                        elif action_type == "type":
                            text = tool_input.get("text")
                            if text:
                                action_to_run = TypeAction(text=text)
                        elif action_type == "scroll":
                            direction = tool_input.get("scroll_direction")
                            amount = tool_input.get("scroll_amount", 1)
                            coord = tool_input.get("coordinate")
                            x = coord[0] if coord else None
                            y = coord[1] if coord else None
                            if direction:
                                action_to_run = ScrollAction(scroll_direction=direction, scroll_amount=amount, x=x, y=y)
                        elif action_type == "key":
                            text: str = tool_input.get("text")
                            if text:
                                action_to_run = KeypressAction(keys=text.split("+"))  # Split xdotool style
                        elif action_type == "hold_key":
                            text = tool_input.get("text")
                            duration = tool_input.get("duration", 1.0)
                            if text:
                                action_to_run = HoldKeyAction(text=text, duration=duration)
                        elif action_type == "wait":
                            duration = tool_input.get("duration", 1.0)
                            action_to_run = WaitAction(duration=duration)
                        elif action_type == "screenshot":
                            action_to_run = ScreenshotAction()
                            content_for_history = [
                                {
                                    "type": "image",
                                    "source": {"type": "base64", "media_type": "image/webp", "data": "[placeholder]"},
                                }
                            ]
                        elif action_type == "cursor_position":
                            action_to_run = CursorPositionAction()
                        else:
                            logger.warning(f"Unsupported Anthropic computer action type: {action_type}")

                    elif tool_name == "goto":
                        url = tool_input.get("url")
                        if url:
                            action_to_run = GotoAction(url=url)
                            content_for_history = f"Navigated to {url}"
                        else:
                            logger.warning("Goto tool called without URL.")
                    elif tool_name == "back":
                        action_to_run = BackAction()
                        content_for_history = "Navigated back"

                except Exception as e:
                    logger.error(f"Error converting Anthropic action {tool_name} ({tool_input}): {e}")

                if action_to_run:
                    actions.append(action_to_run)
                    # Prepare the result structure expected in the message history
                    tool_results_for_history.append(
                        {
                            "type": "tool_result",
                            "tool_use_id": tool_use_id,
                            "content": content_for_history,  # This will be updated after env.step
                            "is_error": False,  # Will be updated after env.step
                        }
                    )

        self._update_usage(
            response.usage.input_tokens,
            response.usage.output_tokens,
            response.usage.cache_read_input_tokens,
            response.usage.cache_creation_input_tokens,
        )
        self.tracer["temperature"] = temperature

        return AgentActResult(
            compiled_response=compiled_response,
            actions=actions,
            tool_results_for_history=tool_results_for_history,
            raw_agent_response=response,
            usage=self.tracer.get("usage", {}),
            safety_check_message=None,  # Anthropic doesn't have this yet
        )

    def compile_response(self, response_content: list[BetaContentBlock]) -> str:
        """Compile Anthropic response into a single string."""
        compiled_response = [""]
        for block in deepcopy(response_content):
            if block.type == "text":
                compiled_response.append(block.text)
            elif block.type == "tool_use":
                block_input = {"action": block.name}
                if block.name == "computer":
                    block_input = block.input  # Computer action details are in input dict
                elif block.name == "goto":
                    block_input["url"] = block.input.get("url", "[Missing URL]")

                # Avoid showing large image data in compiled text log
                if isinstance(block_input, dict) and block_input.get("action") == "screenshot":
                    block_input["image"] = "[placeholder for screenshot]"
                    compiled_response.append(f"**Action**: {json.dumps(block_input)}")
                else:
                    compiled_response.append(f"**Action**: {json.dumps(block_input)}")
            elif block.type == "thinking":
                # Check if thinking content exists before appending
                thinking_content = getattr(block, "thinking", None)
                if thinking_content:
                    compiled_response.append(f"**Thought**: {thinking_content}")

        return "\n- ".join(filter(None, compiled_response))  # Filter out empty strings

    @staticmethod
    async def render_response(response_content: list[BetaContentBlock], screenshot: Optional[str] = None) -> str:
        """Render Anthropic response, potentially including actual screenshots."""
        compiled_response = [""]
        for block in deepcopy(response_content):  # Use deepcopy to avoid modifying original
            if block.type == "text":
                compiled_response.append(block.text)
            elif block.type == "tool_use":
                block_input = {"action": block.name}
                if block.name == "computer":
                    block_input = block.input
                elif block.name == "goto":
                    block_input["url"] = block.input.get("url", "[Missing URL]")

                # If it's a screenshot action and we have a page, get the actual screenshot
                if isinstance(block_input, dict) and block_input.get("action") == "screenshot":
                    if screenshot:
                        block_input["image"] = f"data:image/webp;base64,{screenshot}"
                    else:
                        block_input["image"] = "[Failed to get screenshot]"

                compiled_response.append(f"**Action**: {json.dumps(block_input)}")
            elif block.type == "thinking":
                thinking_content = getattr(block, "thinking", None)
                if thinking_content:
                    compiled_response.append(f"**Thought**: {thinking_content}")

        return "\n- ".join(filter(None, compiled_response))


# --- Main Operator Function ---
async def operate_browser(
    query: str,
    user: KhojUser,
    conversation_log: dict,
    location_data: LocationData,
    send_status_func: Optional[Callable] = None,
    query_images: Optional[List[str]] = None,  # TODO: Handle query images
    agent: Agent = None,
    query_files: str = None,  # TODO: Handle query files
    cancellation_event: Optional[asyncio.Event] = None,
    tracer: dict = {},
):
    response, safety_check_message = None, None
    final_compiled_response = ""
    environment: Optional[BrowserEnvironment] = None

    try:
        agent_chat_model = await AgentAdapters.aget_agent_chat_model(agent, user) if agent else None
        chat_model: ChatModel = await ConversationAdapters.aget_default_chat_model(user, agent_chat_model)
        supported_operator_model_types = [ChatModel.ModelType.OPENAI, ChatModel.ModelType.ANTHROPIC]

        if not chat_model or chat_model.model_type not in supported_operator_model_types:
            raise ValueError(
                f"Unsupported AI model. Configure and use chat model of type {supported_operator_model_types} to enable Browser use."
            )

        if send_status_func:
            async for event in send_status_func(f"**Launching Browser**"):
                yield {ChatEvent.STATUS: event}

        # Initialize Environment
        environment = BrowserEnvironment()
        await environment.start(width=1024, height=768)

        # Initialize Agent
        max_iterations = 40  # TODO: Configurable?
        operator_agent: OperatorAgent
        if chat_model.model_type == ChatModel.ModelType.OPENAI:
            operator_agent = OpenAIOperatorAgent(chat_model, max_iterations, tracer)
        elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
            operator_agent = AnthropicOperatorAgent(chat_model, max_iterations, tracer)
        else:  # Should not happen due to check above, but satisfy type checker
            raise ValueError("Invalid model type for operator agent.")

        messages = [{"role": "user", "content": query}]
        run_summarize = False
        task_completed = False
        iterations = 0

        with timer(f"Operating browser with {chat_model.model_type} {chat_model.name}", logger):
            while iterations < max_iterations:
                if cancellation_event and cancellation_event.is_set():
                    logger.info(f"Browser operator cancelled by client disconnect")
                    break

                iterations += 1

                # Get current environment state
                browser_state = await environment.get_state()

                # Agent decides action(s)
                agent_result = await operator_agent.act(deepcopy(messages), browser_state)

                final_compiled_response = agent_result.compiled_response  # Update final response each turn
                safety_check_message = agent_result.safety_check_message

                # Update conversation history with agent's response (before tool results)
                if chat_model.model_type == ChatModel.ModelType.OPENAI:
                    # OpenAI expects list of blocks in 'content' for assistant message with tool calls
                    messages += agent_result.raw_agent_response.output
                elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                    messages.append({"role": "assistant", "content": agent_result.raw_agent_response.content})

                # Render status update
                rendered_response = agent_result.compiled_response  # Default rendering
                if chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                    rendered_response = await operator_agent.render_response(
                        agent_result.raw_agent_response.content, browser_state.screenshot
                    )
                elif chat_model.model_type == ChatModel.ModelType.OPENAI:
                    rendered_response = await operator_agent.render_response(
                        agent_result.raw_agent_response.output, browser_state.screenshot
                    )
                if send_status_func:
                    async for event in send_status_func(f"**Operating Browser**:\n{rendered_response}"):
                        yield {ChatEvent.STATUS: event}

                # Execute actions in the environment
                step_results: List[StepResult] = []
                if not safety_check_message:
                    for action in agent_result.actions:
                        if cancellation_event and cancellation_event.is_set():
                            break
                        step_result = await environment.step(action)
                        step_results.append(step_result)

                # Gather results from actions
                for step_result in step_results:
                    # Update the placeholder content in the history structure
                    result_for_history = agent_result.tool_results_for_history[len(step_results) - 1]
                    result_content = step_result.error or step_result.output or "[Action completed]"
                    if chat_model.model_type == ChatModel.ModelType.OPENAI:
                        if result_for_history["type"] == "computer_call_output":
                            result_for_history["output"] = {
                                "type": "input_image",
                                "image_url": f"data:image/webp;base64,{step_result.screenshot_base64}",
                            }
                            result_for_history["output"]["current_url"] = step_result.current_url
                        else:
                            result_for_history["output"] = result_content
                    elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                        result_for_history["content"] = result_content
                        if step_result.error:
                            result_for_history["is_error"] = True

                    # Add browser message to compiled log for tracing
                    operator_agent.compiled_operator_messages.append(
                        ChatMessage(role="browser", content=str(result_content))
                    )

                # Check summarization conditions
                summarize_prompt = (
                    f"Collate all relevant information from your research so far to answer the target query:\n{query}."
                )
                task_completed = not agent_result.actions and not run_summarize  # No actions requested by agent
                trigger_iteration_limit = iterations == max_iterations and not run_summarize

                if task_completed or trigger_iteration_limit:
                    iterations = max_iterations - 1  # Ensure one more iteration for summarization
                    run_summarize = True
                    logger.info(
                        f"Triggering summarization. Task completed: {task_completed}, Iteration limit: {trigger_iteration_limit}"
                    )

                    # Append summarize prompt differently based on model
                    if chat_model.model_type == ChatModel.ModelType.OPENAI:
                        # Pop the last tool result if max iterations reached and agent attempted a tool call
                        if trigger_iteration_limit and agent_result.tool_results_for_history:
                            agent_result.tool_results_for_history.pop()

                        # Append summarize prompt as a user message after tool results
                        messages += agent_result.tool_results_for_history  # Add results first
                        messages.append({"role": "user", "content": summarize_prompt})
                        agent_result.tool_results_for_history = []  # Clear results as they are now in messages

                    elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                        # Append summarize prompt as text within the tool results user message
                        agent_result.tool_results_for_history.append({"type": "text", "text": summarize_prompt})

                # Add tool results to messages for the next iteration (if not handled above for OpenAI summarize)
                if agent_result.tool_results_for_history:
                    if chat_model.model_type == ChatModel.ModelType.OPENAI:
                        messages += agent_result.tool_results_for_history
                    elif chat_model.model_type == ChatModel.ModelType.ANTHROPIC:
                        # Mark the final tool result as a cache break point
                        agent_result.tool_results_for_history[-1]["cache_control"] = {"type": "ephemeral"}
                        # Remove previous cache controls (Anthropic specific)
                        for msg in messages:
                            if msg["role"] == "user" and isinstance(msg["content"], list):
                                for block in msg["content"]:
                                    if isinstance(block, dict) and "cache_control" in block:
                                        del block["cache_control"]
                        messages.append({"role": "user", "content": agent_result.tool_results_for_history})

                # Exit if safety checks are pending
                if safety_check_message:
                    logger.warning(f"Safety check triggered: {safety_check_message}")
                    break

            # Determine final response message
            if task_completed and not safety_check_message:
                response = final_compiled_response
            elif safety_check_message:
                response = safety_check_message  # Return safety message if that's why we stopped
            else:  # Hit iteration limit
                response = f"Operator hit iteration limit ({max_iterations}). If the results seem incomplete try again, assign a smaller task or try a different approach.\nThese were the results till now:\n{final_compiled_response}"

    except requests.RequestException as e:
        error_msg = f"Browser use failed due to a network error: {e}"
        logger.error(error_msg)
        raise ValueError(error_msg)
    except Exception as e:
        error_msg = f"Browser use failed due to an unexpected error: {e}"
        logger.exception(error_msg)  # Log full traceback for unexpected errors
        raise ValueError(error_msg)
    finally:
        if environment and not safety_check_message:  # Don't close browser if safety check pending
            await environment.close()

    yield safety_check_message or response
