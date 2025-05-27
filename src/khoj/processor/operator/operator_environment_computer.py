import ast
import asyncio
import base64
import io
import logging
import platform
import subprocess
from typing import Literal, Optional, Union

from PIL import Image, ImageDraw

from khoj.processor.operator.operator_actions import OperatorAction, Point
from khoj.processor.operator.operator_environment_base import (
    Environment,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import convert_image_to_webp

logger = logging.getLogger(__name__)


# --- Concrete Computer Environment ---
class ComputerEnvironment(Environment):
    def __init__(
        self,
        provider: Literal["local", "docker"] = "local",
        docker_display: str = ":99",
        docker_container_name: str = "khoj-computer",
    ):
        self.provider = provider
        self.docker_display = docker_display
        self.docker_container_name = docker_container_name

        self.width: int = 0
        self.height: int = 0
        self.mouse_pos: Point = Point(x=0, y=0)

    async def _execute(self, func_name, *args, **kwargs):
        """
        Executes a pyautogui function, abstracting the execution context.
        Currently runs locally using asyncio.to_thread.
        """
        python_command_str = self.generate_pyautogui_command(func_name, *args, **kwargs)
        # Docker execution
        if self.provider == "docker":
            try:
                output_str = await self.docker_execute(python_command_str)
            except RuntimeError as e:  # Catch other Docker execution errors
                logger.error(f"Error during Docker execution of {func_name}: {e}")
                raise  # Re-raise as a general error for the caller to handle
        # Local execution
        else:
            process = await asyncio.to_thread(
                subprocess.run,
                ["python3", "-c", python_command_str],
                capture_output=True,
                text=True,
                check=False,  # We check returncode manually
            )
            output_str = process.stdout.strip()
            if process.returncode != 0:
                if "FailSafeException" in process.stderr or "FailSafeException" in process.stdout:
                    # Extract the message if possible, otherwise use generic
                    fs_msg = process.stderr or process.stdout
                    raise KeyboardInterrupt(fs_msg)
                else:
                    error_msg = (
                        f'Local script execution failed:\nCmd: python3 -c "{python_command_str[:200]}...{python_command_str[-200:]}\n'
                        f"Return Code: {process.returncode}\nStderr: {process.stderr}\nStdout: {process.stdout}"
                    )
                    logger.error(error_msg)
                    raise RuntimeError(f"Local script execution error: {process.stderr or process.stdout}")
        if not output_str or output_str == "None":
            return None

        try:
            return ast.literal_eval(output_str)
        except (ValueError, SyntaxError):
            # If not a literal (e.g., some other string output), return as is
            return output_str

    async def start(self, width: int, height: int) -> None:
        """
        Initializes the computer environment.
        The width and height parameters are logged, but actual screen dimensions are used.
        """
        screen_width, screen_height = await self._execute("size")

        self.width = screen_width
        self.height = screen_height
        # Initialize mouse position to center, or current if available
        try:
            current_x, current_y = await self._execute("position")
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
            # Get screenshot
            base64_png_str = await self._execute("screenshot")
            screenshot_bytes = base64.b64decode(base64_png_str)

            # Get current mouse position
            current_mouse_x, current_mouse_y = await self._execute("position")
            draw_pos = Point(x=current_mouse_x, y=current_mouse_y)

            # Add mouse position to screenshot
            screenshot_bytes_with_mouse = await self._draw_mouse_position(screenshot_bytes, draw_pos)
            screenshot_webp_bytes = convert_image_to_webp(screenshot_bytes_with_mouse)
            return base64.b64encode(screenshot_webp_bytes).decode("utf-8")
        except KeyboardInterrupt:  # Propagate keyboard interrupts
            raise
        except Exception as e:
            logger.error(f"Failed to get screenshot: {e}", exc_info=True)
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
                        await self._execute("keyDown", mod_key)

                    if button_name == "wheel":
                        # Perform a small scroll action at this position (e.g., one "tick" down)
                        # Pyautogui scroll: positive up, negative down.
                        await self._execute("scroll", -1, x=x, y=y)
                        output = f"Scrolled wheel at ({x}, {y})"
                    else:
                        pyautogui_button = button_name.lower() if button_name else "left"
                        await self._execute("click", x=x, y=y, button=pyautogui_button)
                        output = f"{button_name.capitalize() if button_name else 'Left'} clicked at ({x}, {y})"

                    for mod_key in reversed(modifiers_to_press):
                        await self._execute("keyUp", mod_key)

                    self.mouse_pos = Point(x=x, y=y)
                    logger.debug(f"Action: {action.type} {button_name} at ({x},{y}) with modifiers {action.modifiers}")

                case "double_click":
                    x, y = action.x, action.y
                    await self._execute("doubleClick", x=x, y=y)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Double clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "triple_click":
                    x, y = action.x, action.y
                    await self._execute("click", x=x, y=y, clicks=3)
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Triple clicked at ({x}, {y})"
                    logger.debug(f"Action: {action.type} at ({x},{y})")

                case "scroll":
                    current_x_pos, current_y_pos = await self._execute("position")
                    target_x = action.x if action.x is not None else current_x_pos
                    target_y = action.y if action.y is not None else current_y_pos

                    if target_x != current_x_pos or target_y != current_y_pos:
                        await self._execute("moveTo", target_x, target_y)

                    self.mouse_pos = Point(x=target_x, y=target_y)  # Update mouse pos to scroll location

                    if action.scroll_x is not None or action.scroll_y is not None:
                        scroll_x_amount = action.scroll_x or 0
                        scroll_y_amount = action.scroll_y or 0

                        if scroll_x_amount != 0:
                            await self._execute("hscroll", scroll_x_amount)
                        if scroll_y_amount != 0:
                            # pyautogui scroll: positive up, so negate for typical "scroll down" meaning positive y
                            await self._execute("scroll", -scroll_y_amount)
                        output = f"Scrolled by (x:{scroll_x_amount}, y:{scroll_y_amount}) at ({target_x}, {target_y})"
                    elif action.scroll_direction:
                        # Define scroll unit (number of pyautogui scroll 'clicks')
                        # This might need tuning based on desired sensitivity.
                        pyautogui_scroll_clicks_per_unit = 1
                        amount = action.scroll_amount or 1
                        total_scroll_clicks = pyautogui_scroll_clicks_per_unit * amount

                        if action.scroll_direction == "up":
                            await self._execute("scroll", total_scroll_clicks)
                        elif action.scroll_direction == "down":
                            await self._execute("scroll", -total_scroll_clicks)
                        elif action.scroll_direction == "left":
                            await self._execute("hscroll", -total_scroll_clicks)
                        elif action.scroll_direction == "right":
                            await self._execute("hscroll", total_scroll_clicks)
                        output = f"Scrolled {action.scroll_direction} by {amount} units at ({target_x}, {target_y})"
                    else:
                        error = "Scroll action requires either scroll_x/y or scroll_direction"
                    logger.debug(f"Action: {action.type} details: {output or error}")

                case "keypress":
                    mapped_keys = [self.CUA_KEY_TO_PYAUTOGUI_KEY.get(k.lower(), k) for k in action.keys]
                    key_string = "N/A"
                    if not mapped_keys:
                        error = "Keypress action requires at least one key"
                    elif len(mapped_keys) > 1:
                        await self._execute("hotkey", *mapped_keys)
                        key_string = "+".join(mapped_keys)
                    else:
                        await self._execute("press", mapped_keys[0])
                        key_string = mapped_keys[0]
                    if not error:
                        output = f"Pressed key(s): {key_string}"
                    logger.debug(f"Action: {action.type} '{key_string}'")

                case "type":
                    text_to_type = action.text
                    await self._execute("typewrite", text_to_type, interval=0.02)  # Small interval
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
                    await self._execute("moveTo", x, y, duration=0.2)  # Small duration for smooth move
                    self.mouse_pos = Point(x=x, y=y)
                    output = f"Moved mouse to ({x}, {y})"
                    logger.debug(f"Action: {action.type} to ({x},{y})")

                case "drag":
                    path = action.path
                    if not path:
                        error = "Missing path for drag action"
                    else:
                        start_x, start_y = path[0].x, path[0].y
                        await self._execute("moveTo", start_x, start_y, duration=0.1)
                        await self._execute("mouseDown")
                        for point in path[1:]:
                            await self._execute("moveTo", point.x, point.y, duration=0.05)
                        await self._execute("mouseUp")
                        self.mouse_pos = Point(x=path[-1].x, y=path[-1].y)
                        output = f"Drag along path starting at ({start_x},{start_y})"
                        logger.debug(f"Action: {action.type} with {len(path)} points")

                case "mouse_down":
                    pyautogui_button = action.button.lower() if action.button else "left"
                    await self._execute("mouseDown", button=pyautogui_button)
                    output = f"{action.button.capitalize() if action.button else 'Left'} mouse button down"
                    logger.debug(f"Action: {action.type} {action.button}")

                case "mouse_up":
                    pyautogui_button = action.button.lower() if action.button else "left"
                    await self._execute("mouseUp", button=pyautogui_button)
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
                            await self._execute("keyDown", key_to_hold)
                        await asyncio.sleep(duration)  # Non-pyautogui, direct sleep
                        for key_to_hold in reversed(parsed_keys):  # Release in reverse order
                            await self._execute("keyUp", key_to_hold)
                        output = (
                            f"Held key{'s' if len(parsed_keys) > 1 else ''} {keys_to_hold_str} for {duration} seconds"
                        )
                        logger.debug(f"Action: {action.type} '{keys_to_hold_str}' for {duration}s")

                case "key_down":
                    key_to_press = self.CUA_KEY_TO_PYAUTOGUI_KEY.get(action.key.lower(), action.key)
                    await self._execute("keyDown", key_to_press)
                    output = f"Key down: {key_to_press}"
                    logger.debug(f"Action: {action.type} {key_to_press}")

                case "key_up":
                    key_to_release = self.CUA_KEY_TO_PYAUTOGUI_KEY.get(action.key.lower(), action.key)
                    await self._execute("keyUp", key_to_release)
                    output = f"Key up: {key_to_release}"
                    logger.debug(f"Action: {action.type} {key_to_release}")

                case "cursor_position":
                    pos_x, pos_y = await self._execute("position")
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
        except KeyboardInterrupt:
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

    def generate_pyautogui_command(self, func_name: str, *args, **kwargs) -> str:
        args_repr = [repr(arg) for arg in args]
        kwargs_repr = [f"{k}={repr(v)}" for k, v in kwargs.items()]
        all_params_repr = ", ".join(args_repr + kwargs_repr)

        # Base script setup
        script_lines = [
            "import os",
            "import pyautogui",
        ]

        if self.provider == "docker":
            script_lines.extend(
                [
                    # Display export for Docker.
                    f"os.environ['DISPLAY']='{self.docker_display}'",
                    # Disable failsafe in Docker to avoid accidental exits
                    "pyautogui.FAILSAFE = False",
                ]
            )

        # Function-specific logic
        if func_name == "screenshot":
            script_lines.extend(
                [
                    "import io",
                    "import base64",
                    "img = pyautogui.screenshot()",
                    "buf = io.BytesIO()",
                    "img.save(buf, format='PNG')",
                    "print(base64.b64encode(buf.getvalue()).decode('utf-8'))",
                ]
            )
        elif func_name == "size":
            script_lines.extend(["size = pyautogui.size()", "print(f'({size.width}, {size.height})')"])
        elif func_name == "position":
            script_lines.extend(["pos = pyautogui.position()", "print(f'({pos.x}, {pos.y})')"])
        else:  # General command structure
            script_lines.extend(
                [f"result = pyautogui.{func_name}({all_params_repr})", "print(result if result is not None else '')"]
            )

        return "; ".join(script_lines)

    async def docker_execute(self, python_command_str: str) -> Optional[str]:
        if not self.docker_container_name or not self.docker_display:
            logger.error("Container name or Docker display not set for Docker execution.")
            return None

        safe_python_cmd = python_command_str.replace('"', '\\"')
        docker_full_cmd = (
            f'docker exec -e DISPLAY={self.docker_display} "{self.docker_container_name}" '
            f'python3 -c "{safe_python_cmd}"'
        )

        try:
            process = await asyncio.to_thread(
                subprocess.run,
                docker_full_cmd,
                shell=True,
                capture_output=True,
                text=True,
                check=False,  # We check returncode manually
            )
            if process.returncode != 0:
                if "FailSafeException" in process.stderr or "FailSafeException" in process.stdout:
                    raise KeyboardInterrupt(process.stderr or process.stdout)
                else:
                    error_msg = (
                        f"Docker command failed:\nCmd: {docker_full_cmd}\n"
                        f"Return Code: {process.returncode}\nStderr: {process.stderr}\nStdout: {process.stdout}"
                    )
                    logger.error(error_msg)
                    raise RuntimeError(f"Docker exec error: {process.stderr or process.stdout}")
            return process.stdout.strip()
        except KeyboardInterrupt:  # Re-raise if caught from above
            raise
        except Exception as e:
            logger.error(f"Unexpected error running command in Docker '{docker_full_cmd}': {e}")
            # Encapsulate as RuntimeError to avoid leaking subprocess errors directly
            raise RuntimeError(f"Unexpected Docker error: {e}") from e
