import json
import logging
from textwrap import dedent
from typing import List, Optional

from openai import AzureOpenAI, OpenAI
from openai.types.chat import ChatCompletion, ChatCompletionMessage

from khoj.database.models import ChatModel
from khoj.processor.conversation.utils import construct_structured_message
from khoj.processor.operator.operator_actions import (
    BackAction,
    ClickAction,
    DoubleClickAction,
    DragAction,
    GotoAction,
    KeypressAction,
    OperatorAction,
    Point,
    ScreenshotAction,
    ScrollAction,
    TypeAction,
    WaitAction,
)
from khoj.processor.operator.operator_environment_base import EnvironmentType, EnvState
from khoj.utils.helpers import get_chat_usage_metrics

logger = logging.getLogger(__name__)


class GroundingAgent:
    def __init__(
        self,
        model: ChatModel,
        environment_type: EnvironmentType,
        client: OpenAI | AzureOpenAI,
        max_iterations: int,
        tracer: dict = None,
    ):
        self.model = model
        self.client = client
        self.max_iterations = max_iterations
        self.tracer = tracer
        self.environment_type = environment_type
        self.action_tools = self.get_tools(self.environment_type)

    async def act(self, instruction: str, current_state: EnvState) -> tuple[str, list[OperatorAction]]:
        """Call the grounding LLM to get the next action based on the current state and instruction."""
        # Format the message for the API call
        messages_for_api = self._format_message_for_api(instruction, current_state)
        try:
            grounding_response: ChatCompletion = await self.client.chat.completions.create(
                messages=messages_for_api,
                model=self.model.name,
                tools=self.action_tools,
                tool_choice="required",
                temperature=0.0,  # Grounding should be precise
                max_completion_tokens=1000,  # Allow for thoughts + actions
            )
            if not isinstance(grounding_response, ChatCompletion):
                raise ValueError("Grounding LLM response is not of type ChatCompletion.")
            logger.debug(f"Grounding LLM response: {grounding_response.model_dump_json()}")

            # Parse tool calls
            grounding_message = grounding_response.choices[0].message
            rendered_response, actions = self._parse_action(grounding_message, instruction, current_state)

            # Update usage by grounding model
            self.tracer["usage"] = get_chat_usage_metrics(
                self.model.name,
                input_tokens=grounding_response.usage.prompt_tokens,
                output_tokens=grounding_response.usage.completion_tokens,
                usage=self.tracer.get("usage"),
            )
        except Exception as e:
            logger.error(f"Error calling Grounding LLM: {e}")
            rendered_response = f"**Error**: Error contacting Grounding LLM: {e}"
            actions = []

        return rendered_response, actions

    def _format_message_for_api(self, instruction: str, current_state: EnvState) -> List:
        """Format the message for the API call."""
        # Construct grounding LLM input (using only the latest user prompt + image)
        # We don't pass the full history here, as grounding depends on the *current* state + NL action
        grounding_user_prompt = self.get_instruction(instruction, self.environment_type)
        screenshots = [f"data:image/webp;base64,{current_state.screenshot}"]
        grounding_messages_content = construct_structured_message(
            grounding_user_prompt, screenshots, self.model.model_type, vision_enabled=True
        )
        return [{"role": "user", "content": grounding_messages_content}]

    def _parse_action(
        self, grounding_message: ChatCompletionMessage, instruction: str, current_state: EnvState
    ) -> tuple[str, list[OperatorAction]]:
        """Parse the tool calls from the grounding LLM response and convert them to action objects."""
        actions: List[OperatorAction] = []
        action_results: List[dict] = []

        if grounding_message.tool_calls:
            rendered_parts = []
            for tool_call in grounding_message.tool_calls:
                function_name = tool_call.function.name
                try:
                    arguments = json.loads(tool_call.function.arguments)
                    action_to_run: Optional[OperatorAction] = None
                    action_render_str = f"**Action ({function_name})**: {tool_call.function.arguments}"

                    if function_name == "click":
                        action_to_run = ClickAction(**arguments)
                    elif function_name == "left_double":
                        action_to_run = DoubleClickAction(**arguments)
                    elif function_name == "right_single":
                        action_to_run = ClickAction(button="right", **arguments)
                    elif function_name == "type":
                        content = arguments.get("content")
                        action_to_run = TypeAction(text=content)
                    elif function_name == "scroll":
                        direction = arguments.get("direction", "down")
                        amount = 3
                        action_to_run = ScrollAction(scroll_direction=direction, scroll_amount=amount, **arguments)
                    elif function_name == "hotkey":
                        action_to_run = KeypressAction(**arguments)
                    elif function_name == "goto":
                        action_to_run = GotoAction(**arguments)
                    elif function_name == "back":
                        action_to_run = BackAction(**arguments)
                    elif function_name == "wait":
                        action_to_run = WaitAction(**arguments)
                    elif function_name == "screenshot":
                        action_to_run = ScreenshotAction(**arguments)
                    elif function_name == "drag":
                        # Need to convert list of dicts to list of Point objects
                        path_dicts = arguments.get("path", [])
                        path_points = [Point(**p) for p in path_dicts]
                        if path_points:
                            action_to_run = DragAction(path=path_points)
                        else:
                            logger.warning(f"Drag action called with empty path: {arguments}")
                            action_render_str += " [Skipped - empty path]"
                    elif function_name == "finished":
                        action_to_run = None
                    else:
                        logger.warning(f"Grounding LLM called unhandled tool: {function_name}")
                        action_render_str += " [Unhandled]"

                    if action_to_run:
                        actions.append(action_to_run)
                        action_results.append(
                            {
                                "type": "tool_result",
                                "tool_call_id": tool_call.id,
                                "content": None,  # Updated after environment step
                            }
                        )
                        rendered_parts.append(action_render_str)
                except (json.JSONDecodeError, TypeError, ValueError) as arg_err:
                    logger.error(
                        f"Error parsing arguments for tool {function_name}: {arg_err} - Args: {tool_call.function.arguments}"
                    )
                    rendered_parts.append(f"**Error**: Failed to parse arguments for {function_name}")
            rendered_response = "\n- ".join(rendered_parts)
        else:
            # Grounding LLM responded but didn't call a tool
            logger.warning("Grounding LLM did not produce a tool call.")
            rendered_response = f"{grounding_message.content or 'No action required.'}"

        # Render the response
        return rendered_response, actions

    def get_instruction(self, instruction: str, environment_type: EnvironmentType) -> str:
        """
        Get the instruction for the agent based on the environment type.
        """
        UITARS_COMPUTER_PREFIX_PROMPT = """
        You are a GUI agent. You are given a task and your action history, with screenshots. You need to perform the next action to complete the task.
        """
        UITARS_BROWSER_PREFIX_PROMPT = """
        You are a GUI agent. You are given a task and a screenshot of the web browser tab you operate. You need to decide the next action to complete the task.
        You control a single tab in a Chromium browser. You cannot access the OS, filesystem or the application window.
        Always use the `goto` function to navigate to a specific URL. Ctrl+t, Ctrl+w, Ctrl+q, Ctrl+Shift+T, Ctrl+Shift+W are not allowed.
        """

        UITARS_USR_COMPUTER_PROMPT_THOUGHT = f"""
        Try fulfill the user instruction to the best of your ability, especially when the instruction is given multiple times. Do not ignore the instruction.

        ## Output Format
        ```
        Thought: ...
        Action: ...
        ```

        ## Action Space
        click(start_box='<|box_start|>(x1,y1)<|box_end|>')
        left_double(start_box='<|box_start|>(x1,y1)<|box_end|>')
        right_single(start_box='<|box_start|>(x1,y1)<|box_end|>')
        drag(start_box='<|box_start|>(x1,y1)<|box_end|>', end_box='<|box_start|>(x3,y3)<|box_end|>')
        hotkey(key='')
        type(content='xxx') # Use escape characters \\', \\\", and \\n in content part to ensure we can parse the content in normal python string format. If you want to submit your input, use \\n at the end of content.
        scroll(start_box='<|box_start|>(x1,y1)<|box_end|>', direction='down or up or right or left')
        wait(duration='time') # Sleep for specified time. Default is 1s and take a screenshot to check for any changes.

        ## Note
        - Use English in `Thought` part.
        - Write a small plan and finally summarize your next action (with its target element) in one sentence in `Thought` part.

        ## User Instruction
        {instruction}
        """
        UITARS_USR_BROWSER_PROMPT_THOUGHT = f"""
        Try fulfill the user instruction to the best of your ability, especially when the instruction is given multiple times. Do not ignore the instruction.

        ## Output Format
        ```
        Thought: ...
        Action: ...
        ```

        ## Action Space
        click(start_box='<|box_start|>(x1,y1)<|box_end|>')
        left_double(start_box='<|box_start|>(x1,y1)<|box_end|>')
        right_single(start_box='<|box_start|>(x1,y1)<|box_end|>')
        drag(start_box='<|box_start|>(x1,y1)<|box_end|>', end_box='<|box_start|>(x3,y3)<|box_end|>')
        hotkey(key='')
        type(content='xxx') # Use escape characters \\', \\\", and \\n in content part to ensure we can parse the content in normal python string format. If you want to submit your input, use \\n at the end of content.
        scroll(start_box='<|box_start|>(x1,y1)<|box_end|>', direction='down or up or right or left')
        wait(duration='time') # Sleep for specified time. Default is 1s and take a screenshot to check for any changes.
        goto(url='xxx') # Always use this to navigate to a specific URL. Use escape characters \\', \\", and \\n in url part to ensure we can parse the url in normal python string format.
        back() # Use this to go back to the previous page.

        ## Note
        - Use English in `Thought` part.
        - Write a small plan and finally summarize your next action (with its target element) in one sentence in `Thought` part.

        ## User Instruction
        {instruction}
        """

        if environment_type == EnvironmentType.BROWSER:
            return dedent(UITARS_BROWSER_PREFIX_PROMPT + UITARS_USR_BROWSER_PROMPT_THOUGHT).lstrip()
        elif environment_type == EnvironmentType.COMPUTER:
            return dedent(UITARS_COMPUTER_PREFIX_PROMPT + UITARS_USR_COMPUTER_PROMPT_THOUGHT).lstrip()
        else:
            raise ValueError(f"Expected environment type: Computer or Browser. Got {environment_type}.")

    def get_tools(self, environment_type: EnvironmentType) -> list[dict]:
        """Get tools for the grounding LLM, in OpenAI API tool format"""
        tools = [
            {
                "type": "function",
                "function": {
                    "name": "click",
                    "description": "Click on a specific coordinate.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "x": {"type": "integer", "description": "X coordinate"},
                            "y": {"type": "integer", "description": "Y coordinate"},
                        },
                        "required": ["x", "y"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "left_double",
                    "description": "Double click on a specific coordinate.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "x": {"type": "integer", "description": "X coordinate"},
                            "y": {"type": "integer", "description": "Y coordinate"},
                        },
                        "required": ["x", "y"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "right_single",
                    "description": "Right click on a specific coordinate.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "x": {"type": "integer", "description": "X coordinate"},
                            "y": {"type": "integer", "description": "Y coordinate"},
                        },
                        "required": ["x", "y"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "drag",
                    "description": "Perform a drag-and-drop operation along a path.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "path": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "x": {"type": "integer"},
                                        "y": {"type": "integer"},
                                    },
                                    "required": ["x", "y"],
                                },
                                "description": "List of points (x, y coordinates) defining the drag path.",
                            }
                        },
                        "required": ["path"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "hotkey",
                    "description": "Press a key or key combination.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "keys": {
                                "type": "array",
                                "items": {"type": "string"},
                                "description": "List of keys to press (e.g., ['Control', 'a'], ['Enter'])",
                            }
                        },
                        "required": ["keys"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "type",
                    "description": "Type text, usually into a focused input field.",
                    "parameters": {
                        "type": "object",
                        "properties": {"content": {"type": "string", "description": "Text to type"}},
                        "required": ["content"],
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "scroll",
                    "description": "Scroll the page.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "x": {"type": "integer", "description": "X coordinate to scroll from"},
                            "y": {"type": "integer", "description": "Y coordinate to scroll from"},
                            "direction": {
                                "type": "string",
                                "enum": ["up", "down", "left", "right"],
                                "default": "down",
                            },
                        },
                        "required": [],  # None is strictly required
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "wait",
                    "description": "Pause execution for a specified duration.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "duration": {"type": "number", "description": "Duration in seconds", "default": 1.0}
                        },
                        "required": [],
                    },
                },
            },
        ]
        if environment_type == EnvironmentType.BROWSER:
            tools += [
                {
                    "type": "function",
                    "function": {
                        "name": "goto",
                        "description": "Navigate to a specific URL.",
                        "parameters": {
                            "type": "object",
                            "properties": {"url": {"type": "string", "description": "Fully qualified URL"}},
                            "required": ["url"],
                        },
                    },
                },
                {
                    "type": "function",
                    "function": {
                        "name": "back",
                        "description": "navigate back to the previous page.",
                        "parameters": {"type": "object", "properties": {}},
                    },
                },
            ]

        return tools

    def reset(self):
        """Reset the agent state."""
        pass
