import asyncio
import base64
import io
import logging
import platform  # To help with key mapping
from typing import Optional, Union

from PIL import Image, ImageDraw

from khoj.processor.operator.operator_actions import OperatorAction, Point
from khoj.processor.operator.operator_environment_base import (
    Environment,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import convert_image_to_webp

logger = logging.getLogger(__name__)

try:
    import pyautogui
except ImportError:
    pyautogui = None
    logging.getLogger(__name__).warning(
        "Pyautogui not found. ComputerEnvironment will not be available. " "Install with 'pip install pyautogui'."
    )


# --- Concrete Computer Environment ---
class ComputerEnvironment(Environment):
    def __init__(self):
        if pyautogui is None:
            raise ImportError("Pyautogui is not installed. ComputerEnvironment cannot be initialized.")

        self.width: int = 0
        self.height: int = 0
        self.mouse_pos: Point = Point(x=0, y=0)
        pyautogui.FAILSAFE = True  # Abort by moving mouse to a corner
        # pyautogui.PAUSE = 0.05  # Optional: slight pause after each pyautogui call

    async def _execute(self, func, *args, **kwargs):
        """
        Executes a pyautogui function, abstracting the execution context.
        Currently runs locally using asyncio.to_thread.
        """
        # TODO: Support executing in local/remote docker container or remote computer
        if pyautogui:
            try:
                # Use asyncio.to_thread to have pyautogui calls not block the event loop
                return await asyncio.to_thread(func, *args, **kwargs)
            except pyautogui.FailSafeException as e:
                raise KeyboardInterrupt("User interrupt") from e

    async def start(self, width: int, height: int) -> None:
        """
        Initializes the computer environment.
        The width and height parameters are logged, but actual screen dimensions are used.
        """
        screen_width, screen_height = await self._execute(pyautogui.size)
        self.width = screen_width
        self.height = screen_height
        # Initialize mouse position to center, or current if available
        try:
            current_x, current_y = await self._execute(pyautogui.position)
            self.mouse_pos = Point(x=current_x, y=current_y)
        except Exception:  # Fallback if position cannot be obtained initially
            self.mouse_pos = Point(x=self.width / 2, y=self.height / 2)

        logger.info(
            f"Computer environment started. Screen size: {self.width}x{self.height}. "
            f"Input width/height ({width}x{height}) are noted but screen dimensioning uses actual screen size. "
            f"Initial mouse position: ({self.mouse_pos.x},{self.mouse_pos.y})"
        )

    async def _get_screenshot(self) -> Optional[str]:
        try:
            screenshot_pil = await self._execute(pyautogui.screenshot)

            img_byte_arr = io.BytesIO()
            screenshot_pil.save(img_byte_arr, format="PNG")
            screenshot_bytes = img_byte_arr.getvalue()

            # Get current mouse position to draw accurately
            try:
                current_mouse_x, current_mouse_y = await self._execute(pyautogui.position)
                draw_pos = Point(x=current_mouse_x, y=current_mouse_y)
            except Exception:  # Fallback to stored mouse_pos
                draw_pos = self.mouse_pos

            screenshot_bytes = await self._draw_mouse_position(screenshot_bytes, draw_pos)

            screenshot_webp_bytes = convert_image_to_webp(screenshot_bytes)
            return base64.b64encode(screenshot_webp_bytes).decode("utf-8")
        except Exception as e:
            logger.error(f"Failed to get screenshot: {e}")
            return None

    async def _draw_mouse_position(self, screenshot_bytes: bytes, mouse_pos: Point) -> bytes:
        if Image is None or ImageDraw is None:
            return screenshot_bytes
        try:
            image = Image.open(io.BytesIO(screenshot_bytes))
            draw = ImageDraw.Draw(image)
            radius = 8
            # Red circle with black border for better visibility
            draw.ellipse(
                (mouse_pos.x - radius, mouse_pos.y - radius, mouse_pos.x + radius, mouse_pos.y + radius),
                outline="black",
                fill="red",
                width=2,
            )
            output_buffer = io.BytesIO()
            image.save(output_buffer, format="PNG")
            return output_buffer.getvalue()
        except Exception as e:
            logger.error(f"Failed to draw mouse position: {e}")
            return screenshot_bytes

    async def get_state(self) -> EnvState:
        screenshot = await self._get_screenshot()
        return EnvState(screenshot=screenshot, height=self.height, width=self.width)

    async def step(self, action: OperatorAction) -> EnvStepResult:
        output: Optional[Union[str, dict]] = None
        error: Optional[str] = None
        step_type: str = "text"

        try:
            match action.type:
                case "click":
                    x, y, button_name = action.x, action.y, action.button
                    modifiers_to_press = self.parse_key_combination(action.modifiers) if action.modifiers else []
                    for mod_key in modifiers_to_press:
                        await self._execute(pyautogui.keyDown, mod_key)

                    if button_name == "wheel":
                        # Perform a small scroll action at this position (e.g., one "tick" down)
                        # Pyautogui scroll: positive up, negative down.
                        # Let's make it scroll down by a small amount (e.g. 3 units for pyautogui)
                        await self._execute(pyautogui.scroll, -3, x=x, y=y)
                        output = f"Scrolled wheel at ({x}, {y})"
                    else:
                        pyautogui_button = button_name.lower() if button_name else "left"
                        await self._execute(pyautogui.click, x=x, y=y, button=pyautogui_button)
                        output = f"{button_name.capitalize() if button_name else 'Left'} clicked at ({x}, {y})"

                    for mod_key in reversed(modifiers_to_press):
                        await self._execute(pyautogui.keyUp, mod_key)

                    self.mouse_pos = Point(x=x, y=y)
                    logger.debug(f"Action: {action.type} {button_name} at ({x},{y}) with modifiers {action.modifiers}")

                case "double_click":
                    x, y = action.x, action.y
                    await self._execute(pyautogui.doubleClick, x=x, y=y)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Double clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "triple_click":
                    x, y = action.x, action.y
                    await self._execute(pyautogui.click, x=x, y=y, clicks=3)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Triple clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "scroll":
                    current_x_pos, current_y_pos = await self._execute(pyautogui.position)
                    target_x = action.x if action.x is not None else current_x_pos
                    target_y = action.y if action.y is not None else current_y_pos

                    if target_x != current_x_pos or target_y != current_y_pos:
                        await self._execute(pyautogui.moveTo, target_x, target_y)

                    self.mouse_pos = Point(x=target_x, y=target_y)  # Update mouse pos to scroll location

                    if action.scroll_x is not None or action.scroll_y is not None:
                        scroll_x_amount = action.scroll_x or 0
                        scroll_y_amount = action.scroll_y or 0

                        if scroll_x_amount != 0:
                            await self._execute(pyautogui.hscroll, scroll_x_amount)  # pyautogui.hscroll: positive right
                        if scroll_y_amount != 0:
                            await self._execute(pyautogui.scroll, -scroll_y_amount)  # pyautogui.scroll: positive up
                        output = f"Scrolled by (x:{scroll_x_amount}, y:{scroll_y_amount}) at ({target_x}, {target_y})"
                        logger.debug(
                            f"Action: {action.type} by ({scroll_x_amount},{scroll_y_amount}) at ({target_x},{target_y})"
                        )
                    elif action.scroll_direction:
                        # Define scroll unit (number of pyautogui scroll 'clicks')
                        # This might need tuning based on desired sensitivity.
                        pyautogui_scroll_clicks_per_unit = 20
                        amount = action.scroll_amount or 1
                        total_scroll_clicks = pyautogui_scroll_clicks_per_unit * amount

                        if action.scroll_direction == "up":
                            await self._execute(pyautogui.scroll, total_scroll_clicks)
                        elif action.scroll_direction == "down":
                            await self._execute(pyautogui.scroll, -total_scroll_clicks)
                        elif action.scroll_direction == "left":
                            await self._execute(pyautogui.hscroll, -total_scroll_clicks)
                        elif action.scroll_direction == "right":
                            await self._execute(pyautogui.hscroll, total_scroll_clicks)
                        output = f"Scrolled {action.scroll_direction} by {amount} units at ({target_x}, {target_y})"
                        logger.debug(
                            f"Action: {action.type} {action.scroll_direction} by {amount} at ({target_x},{target_y})"
                        )
                    else:
                        error = "Scroll action requires either scroll_x/y or scroll_direction"

                case "keypress":
                    mapped_keys = [self.CUA_KEY_TO_PYAUTOGUI_KEY.get(k.lower(), k) for k in action.keys]
                    key_string = "N/A"
                    if not mapped_keys:
                        error = "Keypress action requires at least one key"
                    elif len(mapped_keys) > 1:
                        await self._execute(pyautogui.hotkey, *mapped_keys)
                        key_string = "+".join(mapped_keys)
                    else:
                        await self._execute(pyautogui.press, mapped_keys[0])
                        key_string = mapped_keys[0]
                    if not error:
                        output = f"Pressed key(s): {key_string}"
                    logger.debug(f"Action: {action.type} '{key_string}'")

                case "type":
                    text_to_type = action.text
                    await self._execute(pyautogui.typewrite, text_to_type, interval=0.02)  # Small interval
                    output = f"Typed text: {text_to_type}"
                    logger.debug(f"Action: {action.type} '{text_to_type}'")

                case "wait":
                    duration = action.duration
                    await asyncio.sleep(duration)
                    output = f"Waited for {duration} seconds"
                    logger.debug(f"Action: {action.type} for {duration}s")

                case "screenshot":
                    step_type = "image"
                    # The actual screenshot data is added from after_state later
                    output = {"message": "Screenshot captured", "url": "desktop"}
                    logger.debug(f"Action: {action.type}")

                case "move":
                    x, y = action.x, action.y
                    await self._execute(pyautogui.moveTo, x, y, duration=0.2)  # Small duration for smooth move
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Moved mouse to ({x}, {y})"
                    logger.debug(f"Action: {action.type} to ({x},{y})")

                case "drag":
                    path = action.path
                    if not path:
                        error = "Missing path for drag action"
                    else:
                        start_x, start_y = path[0].x, path[0].y
                        await self._execute(pyautogui.moveTo, start_x, start_y, duration=0.1)
                        await self._execute(pyautogui.mouseDown)
                        for point in path[1:]:
                            await self._execute(pyautogui.moveTo, point.x, point.y, duration=0.05)
                        await self._execute(pyautogui.mouseUp)
                        self.mouse_pos = Point(x=path[-1].x, y=path[-1].y)
                        output = f"Drag along path starting at ({start_x},{start_y})"
                        logger.debug(f"Action: {action.type} with {len(path)} points")

                case "mouse_down":
                    pyautogui_button = action.button.lower() if action.button else "left"
                    await self._execute(pyautogui.mouseDown, button=pyautogui_button)
                    output = f"{action.button.capitalize() if action.button else 'Left'} mouse button down"
                    logger.debug(f"Action: {action.type} {action.button}")

                case "mouse_up":
                    pyautogui_button = action.button.lower() if action.button else "left"
                    await self._execute(pyautogui.mouseUp, button=pyautogui_button)
                    output = f"{action.button.capitalize() if action.button else 'Left'} mouse button up"
                    logger.debug(f"Action: {action.type} {action.button}")

                case "hold_key":
                    keys_to_hold_str = action.text
                    duration = action.duration
                    parsed_keys = self.parse_key_combination(keys_to_hold_str)
                    if not parsed_keys:
                        error = f"No valid keys found in '{keys_to_hold_str}' for hold_key"
                    else:
                        for key_to_hold in parsed_keys:
                            await self._execute(pyautogui.keyDown, key_to_hold)
                        await asyncio.sleep(duration)  # Non-pyautogui, direct sleep
                        for key_to_hold in reversed(parsed_keys):  # Release in reverse order
                            await self._execute(pyautogui.keyUp, key_to_hold)
                        output = (
                            f"Held key{'s' if len(parsed_keys) > 1 else ''} {keys_to_hold_str} for {duration} seconds"
                        )
                        logger.debug(f"Action: {action.type} '{keys_to_hold_str}' for {duration}s")

                case "key_down":
                    key_to_press = self.CUA_KEY_TO_PYAUTOGUI_KEY.get(action.key.lower(), action.key)
                    await self._execute(pyautogui.keyDown, key_to_press)
                    output = f"Key down: {key_to_press}"
                    logger.debug(f"Action: {action.type} {key_to_press}")

                case "key_up":
                    key_to_release = self.CUA_KEY_TO_PYAUTOGUI_KEY.get(action.key.lower(), action.key)
                    await self._execute(pyautogui.keyUp, key_to_release)
                    output = f"Key up: {key_to_release}"
                    logger.debug(f"Action: {action.type} {key_to_release}")

                case "cursor_position":
                    pos_x, pos_y = await self._execute(pyautogui.position)
                    self.mouse_pos = Point(x=pos_x, y=pos_y)
                    output = f"Cursor position is ({pos_x}, {pos_y})"
                    logger.debug(f"Action: {action.type}, position: ({pos_x},{pos_y})")

                case "goto":
                    output = f"Goto action (URL: {action.url}) is not applicable for ComputerEnvironment."
                    logger.warning(f"Unsupported action: {action.type} for ComputerEnvironment.")

                case "back":
                    output = "Back action is not applicable for ComputerEnvironment."
                    logger.warning(f"Unsupported action: {action.type} for ComputerEnvironment.")

                case _:
                    error = f"Unrecognized action type: {action.type}"
                    logger.warning(error)
        except KeyboardInterrupt as e:
            error = "User interrupt. Operation aborted."
            logger.error(error)
        except Exception as e:
            error = f"Unexpected error executing action {action.type}: {str(e)}"
            logger.exception(
                f"Unexpected error during step execution for action: {action.model_dump_json(exclude_none=True)}"
            )

        after_state = await self.get_state()

        if action.type == "screenshot" and step_type == "image":
            output = {"image": after_state.screenshot, "url": after_state.url}

        return EnvStepResult(
            type=step_type,
            output=output,
            error=error,
            current_url=after_state.url,
            screenshot_base64=after_state.screenshot,
        )

    async def close(self) -> None:
        logger.debug("Computer environment closed. No specific resources to release for PyAutoGUI.")

    CUA_KEY_TO_PYAUTOGUI_KEY = {
        # Modifiers
        "option": "alt",
        "control": "ctrl",
        "cmd": "command",
        "super": "win",
        "meta": "command" if platform.system() == "Darwin" else "win",
        # Navigation & Editing
        "arrowdown": "down",
        "arrowleft": "left",
        "arrowright": "right",
        "arrowup": "up",
        "caps_lock": "capslock",
        "del": "delete",
        "return": "enter",
        "esc": "escape",
        "pgdn": "pagedown",
        "pgup": "pageup",
        " ": "space",
        # Numpad keys (example, pyautogui uses 'num0', 'add', 'subtract', etc.)
        "numpad0": "num0",
        "numpad_0": "num0",
    }

    @staticmethod
    def parse_key_combination(text: str) -> list[str]:
        if not text:
            return []

        keys_str_list = text.lower().split("+")
        mapped_keys = []
        for k_str in keys_str_list:
            # Use the mapped key if found, otherwise use the string itself (e.g. 'a', '1')
            mapped_keys.append(ComputerEnvironment.CUA_KEY_TO_PYAUTOGUI_KEY.get(k_str.strip(), k_str.strip()))
        return mapped_keys
