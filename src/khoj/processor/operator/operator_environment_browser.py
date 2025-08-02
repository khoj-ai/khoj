import asyncio
import base64
import io
import logging
import os
from typing import Optional, Set, Union

from khoj.processor.operator.operator_actions import DragAction, OperatorAction, Point
from khoj.processor.operator.operator_environment_base import (
    Environment,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import convert_image_to_webp

logger = logging.getLogger(__name__)

try:
    from playwright.async_api import Browser, Page, Playwright, async_playwright
except ImportError:
    logger.debug(
        "Playwright not found. To use browser operator, run 'pip install playwright' and 'playwright install' first."
    )


# --- Concrete BrowserEnvironment ---
class BrowserEnvironment(Environment):
    def __init__(self):
        self.playwright: Optional[Playwright] = None
        self.browser: Optional[Browser] = None
        self.page: Optional[Page] = None
        self.width: int = 1024
        self.height: int = 768
        self.visited_urls: Set[str] = set()
        self.excluded_urls = {"about:blank", "https://duckduckgo.com", "https://www.bing.com", "https://www.google.com"}
        self.navigation_history: list[str] = []
        self.mouse_pos = Point(x=self.width / 2, y=self.height / 2)

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

        # Define a handler for page load events to capture URLs
        async def handle_load(loaded_page: Page):
            url = loaded_page.url
            if not url:
                return

            if not self.navigation_history or self.navigation_history[-1] != url:
                self.navigation_history.append(url)

            if url not in self.excluded_urls and url not in self.visited_urls:
                logger.debug(f"Page loaded: {url}")
                self.visited_urls.add(url)

        # Listen for load events on the main page
        self.page.on("load", handle_load)

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
            # Draw mouse position on the screenshot image
            if self.mouse_pos:
                screenshot_bytes = await self._draw_mouse_position(screenshot_bytes, self.mouse_pos)
            screenshot_webp_bytes = convert_image_to_webp(screenshot_bytes)
            return base64.b64encode(screenshot_webp_bytes).decode("utf-8")
        except Exception as e:
            logger.error(f"Failed to get screenshot: {e}")
            return None

    async def _draw_mouse_position(self, screenshot_bytes: bytes, mouse_pos: Point) -> bytes:
        from PIL import Image, ImageDraw

        # Load the screenshot into a PIL image
        image = Image.open(io.BytesIO(screenshot_bytes))

        # Draw a red circle at the mouse position
        draw = ImageDraw.Draw(image)
        radius = 5
        draw.ellipse(
            (mouse_pos.x - radius, mouse_pos.y - radius, mouse_pos.x + radius, mouse_pos.y + radius), fill="red"
        )

        # Save the modified image to a bytes buffer
        output_buffer = io.BytesIO()
        image.save(output_buffer, format="PNG")
        return output_buffer.getvalue()

    async def get_state(self) -> EnvState:
        if not self.page or self.page.is_closed():
            return EnvState(url="about:blank", screenshot=None, height=self.height, width=self.width)
        url = self.page.url
        screenshot = await self._get_screenshot()
        return EnvState(url=url, screenshot=screenshot, height=self.height, width=self.width)

    async def step(self, action: OperatorAction) -> EnvStepResult:
        if not self.page or self.page.is_closed():
            return EnvStepResult(error="Browser page is not available or closed.")

        before_state = await self.get_state()
        output: Optional[Union[str, dict]] = None
        error: Optional[str] = None
        step_type: str = "text"
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
                    self.mouse_pos = Point(x=x, y=y)
                    logger.debug(f"Action: {action.type} {button} at ({x},{y})")

                case "double_click":
                    x, y = action.x, action.y
                    await self.page.mouse.dblclick(x, y)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Double clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "triple_click":
                    x, y = action.x, action.y
                    await self.page.mouse.click(x, y, click_count=3)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Triple clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "scroll":
                    # Prefer explicit scroll_x/y if provided (from OpenAI style)
                    if action.scroll_x is not None or action.scroll_y is not None:
                        scroll_x = action.scroll_x or 0
                        scroll_y = action.scroll_y or 0
                        if action.x is not None and action.y is not None:
                            await self.page.mouse.move(action.x, action.y)
                            self.mouse_pos = Point(x=action.x, y=action.y)
                        await self.page.evaluate(f"window.scrollBy({scroll_x}, {scroll_y})")
                        output = f"Scrolled by ({scroll_x}, {scroll_y})"
                        logger.debug(f"Action: {action.type} by ({scroll_x},{scroll_y}) at ({action.x},{action.y})")
                    # Otherwise use direction/amount (from Anthropic style)
                    elif action.scroll_direction:
                        scale = 40.0
                        dx, dy = 0.0, 0.0
                        amount = action.scroll_amount or 1
                        if action.scroll_direction == "up":
                            dy = -scale * amount
                        elif action.scroll_direction == "down":
                            dy = scale * amount
                        elif action.scroll_direction == "left":
                            dx = -scale * amount
                        elif action.scroll_direction == "right":
                            dx = scale * amount

                        if action.x is not None and action.y is not None:
                            await self.page.mouse.move(action.x, action.y)
                            self.mouse_pos = Point(x=action.x, y=action.y)
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
                    step_type = "image"
                    output = {"image": before_state.screenshot, "url": before_state.url}
                    logger.debug(f"Action: {action.type}")

                case "move":
                    x, y = action.x, action.y
                    await self.page.mouse.move(x, y)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Moved mouse to ({x}, {y})"
                    logger.debug(f"Action: {action.type} to ({x},{y})")

                case "drag":
                    if not isinstance(action, DragAction):
                        raise TypeError("Invalid action type for drag")
                    path = action.path
                    if not path:
                        error = "Missing path for drag action"
                    else:
                        await self.page.mouse.move(path[0].x, path[0].y)
                        await self.page.mouse.down()
                        for point in path[1:]:
                            await self.page.mouse.move(point.x, point.y)
                        await self.page.mouse.up()
                        self.mouse_pos = Point(x=path[-1].x, y=path[-1].y)
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

                case "key_down":
                    key = action.key
                    await self.page.keyboard.down(key)
                    output = f"Key down: {key}"
                    logger.debug(f"Action: {action.type} {key}")

                case "key_up":
                    key = action.key
                    await self.page.keyboard.up(key)
                    output = f"Key up: {key}"
                    logger.debug(f"Action: {action.type} {key}")

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
                    if len(self.navigation_history) > 1:
                        self.navigation_history.pop()
                        previous_url = self.navigation_history[-1]
                        await self.page.goto(previous_url)
                        output = f"Navigated back to {previous_url}"
                    else:
                        output = "No previous URL to navigate back"
                        previous_url = "about:blank"
                    logger.debug(f"Action: {action.type} to {previous_url}")

                case _:
                    error = f"Unrecognized action type: {action.type}"
                    logger.warning(error)

        except Exception as e:
            error = f"Error executing action {action.type}: {e}"
            logger.exception(f"Error during step execution for action: {action.model_dump_json()}")

        after_state = await self.get_state()
        return EnvStepResult(
            type=step_type,
            output=output,
            error=error,
            current_url=after_state.url,
            screenshot_base64=after_state.screenshot,
        )

    def reset(self) -> None:
        self.visited_urls.clear()

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
