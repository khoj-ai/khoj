import json
import logging
from copy import deepcopy
from datetime import datetime
from textwrap import dedent
from typing import List, Optional

from langchain_core.messages import ChatMessage

from khoj.database.models import ChatModel
from khoj.processor.conversation.google.gemini_chat import gemini_send_message_to_model
from khoj.processor.conversation.utils import (
    AgentMessage,
    ToolCall,
    construct_structured_message,
)
from khoj.processor.operator.operator_actions import *
from khoj.processor.operator.operator_agent_base import AgentActResult, OperatorAgent
from khoj.processor.operator.operator_environment_base import (
    EnvironmentType,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import ToolDefinition, is_none_or_empty

logger = logging.getLogger(__name__)


class GeminiOperatorAgent(OperatorAgent):
    async def act(self, current_state: EnvState) -> AgentActResult:
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
        self._commit_trace()  # Commit trace before next action

        system_prompt = self.get_instructions(self.environment_type, current_state)
        tools = self.get_tools(self.environment_type, current_state)

        if is_none_or_empty(self.messages):
            # add current screenshot from environment state
            if current_state.screenshot:
                initial_environment_screenshot = f"data:image/webp;base64,{current_state.screenshot}"
                first_message_content = construct_structured_message(
                    message=self.query, images=[initial_environment_screenshot]
                )
                self.messages = [AgentMessage(role="user", content=first_message_content)]

        # Trigger trajectory compression if exceed size limit
        if len(self.messages) > self.message_limit:
            logger.debug("Compacting operator trajectory.")
            await self._compress()

        response = await self._call_model(
            messages=self.messages,
            model=self.vision_model,
            system_prompt=system_prompt,
            tools=tools,
        )

        response_content = response.thought
        tool_calls: List[ToolCall] = []
        if response.text and response.text.strip().startswith("[") and response.text.strip().endswith("]"):
            tool_calls: list[ToolCall] = [ToolCall(**tool_call) for tool_call in json.loads(response.text)]

        self.messages.append(
            AgentMessage(
                role="assistant",
                content=response_content,
                raw_response=response.raw_content,
            )
        )
        rendered_response = self._render_response(response_content, tool_calls, current_state.screenshot)

        # Parse actions from response
        for tool_call in tool_calls:
            first_message_content = None
            is_error = False

            action_to_run: Optional[OperatorAction] = None
            tool_input = {k: v for k, v in tool_call.args.items()}
            tool_name = tool_call.name

            # Normalize x, y coordinates to image dimensions
            if "x" in tool_input and "y" in tool_input:
                if current_state.width and current_state.height:
                    tool_input["x"] = tool_input["x"] / 1000 * current_state.width
                    tool_input["y"] = tool_input["y"] / 1000 * current_state.height

            try:
                if tool_name == "click":
                    action_to_run = ClickAction(**tool_input)
                elif tool_name == "double_click":
                    action_to_run = DoubleClickAction(**tool_input)
                elif tool_name == "triple_click":
                    action_to_run = TripleClickAction(**tool_input)
                elif tool_name == "right_click":
                    action_to_run = ClickAction(button="right", **tool_input)
                elif tool_name == "type":
                    action_to_run = TypeAction(**tool_input)
                elif tool_name == "scroll":
                    action_to_run = ScrollAction(**tool_input)
                elif tool_name == "keypress":
                    action_to_run = KeypressAction(**tool_input)
                elif tool_name == "goto":
                    action_to_run = GotoAction(**tool_input)
                elif tool_name == "back":
                    action_to_run = BackAction()
                elif tool_name == "wait":
                    action_to_run = WaitAction(**tool_input)
                elif tool_name == "screenshot":
                    action_to_run = ScreenshotAction()
                elif tool_name == "drag":
                    path_points = [Point(**p) for p in tool_input.get("path", [])]
                    if path_points:
                        action_to_run = DragAction(path=path_points)
                elif tool_name == "mouse_down":
                    action_to_run = MouseDownAction(**tool_input)
                elif tool_name == "mouse_up":
                    action_to_run = MouseUpAction(**tool_input)
                elif tool_name == "hold_key":
                    action_to_run = HoldKeyAction(**tool_input)
                elif tool_name == "key_up":
                    action_to_run = KeyUpAction(**tool_input)
                elif tool_name == "key_down":
                    action_to_run = KeyDownAction(**tool_input)
                elif tool_name == "cursor_position":
                    action_to_run = CursorPositionAction()
                elif tool_name == "text_editor_view":
                    action_to_run = TextEditorViewAction(**tool_input)
                elif tool_name == "text_editor_create":
                    action_to_run = TextEditorCreateAction(**tool_input)
                elif tool_name == "text_editor_str_replace":
                    action_to_run = TextEditorStrReplaceAction(**tool_input)
                elif tool_name == "text_editor_insert":
                    action_to_run = TextEditorInsertAction(**tool_input)
                elif tool_name == "terminal":
                    action_to_run = TerminalAction(**tool_input)
                else:
                    logger.warning(f"Unsupported Gemini computer action type: {tool_name}")

            except Exception as e:
                error_msg = f"Error converting Gemini action {tool_name} ({tool_input}): {e}"
                logger.error(error_msg)
                first_message_content = error_msg
                is_error = True
                action_to_run = NoopAction()

            if action_to_run:
                actions.append(action_to_run)
                action_results.append(
                    {
                        "type": "tool_result",
                        "name": tool_call.name,
                        "content": first_message_content,
                        "is_error": is_error,
                    }
                )

        return AgentActResult(
            actions=actions,
            action_results=action_results,
            rendered_response=rendered_response,
        )

    def add_action_results(self, env_steps: list[EnvStepResult], agent_action: AgentActResult):
        if not agent_action.action_results:
            return

        tool_responses = []
        for idx, env_step in enumerate(env_steps):
            action_result = agent_action.action_results[idx]
            result_content = env_step.error or env_step.output or "[Action completed]"
            tool_responses.append(
                {
                    "type": action_result["type"],
                    "name": action_result["name"],
                    "content": result_content,
                    "is_error": action_result.get("is_error", False),
                }
            )

        # add current screenshot from environment state
        if env_step.screenshot_base64:
            env_screenshot = f"data:image/png;base64,{env_step.screenshot_base64}"
            screenshot_message = construct_structured_message(images=[env_screenshot], message="")
            self.messages.append(AgentMessage(role="user", content=screenshot_message))

        self.messages.append(AgentMessage(role="environment", content=tool_responses))

    def _format_message_for_api(self, messages: list[AgentMessage]) -> list[ChatMessage]:
        """Format messages for the Gemini API."""
        formatted_messages: list[ChatMessage] = []
        for msg in messages:
            role = "model" if msg.role in ["assistant", "environment"] else msg.role
            if msg.role == "assistant" and msg.raw_response:
                formatted_messages.append(
                    ChatMessage(content=msg.raw_response, role=role, additional_kwargs={"message_type": "tool_call"})
                )
            elif msg.role == "environment":
                formatted_messages.append(
                    ChatMessage(content=msg.content, role=role, additional_kwargs={"message_type": "tool_result"})
                )
            else:
                formatted_messages.append(ChatMessage(content=msg.content, role=role))
        return formatted_messages

    def _compile_response(self, response_content: str | list[str | dict], tool_calls: list[ToolCall] = []) -> str:
        """Compile Gemini response into a single string."""
        compiled_response = (
            [json.dumps(item) for item in response_content]
            if isinstance(response_content, list)
            else [response_content]
        )
        for tool_call in tool_calls:
            compiled_response.append(
                f"**Action**: {json.dumps({'name': tool_call.name, 'args': {k: v for k, v in tool_call.args.items()}})}"
            )
        return "\n- ".join(filter(None, compiled_response))

    def _render_response(self, response_content: str, tool_calls: list[ToolCall], screenshot: str | None) -> dict:
        """Render Gemini response, potentially including actual screenshots."""
        render_texts = [response_content]
        for tool_call in deepcopy(tool_calls):
            if tool_call.name == "goto":
                render_texts += [f"Open URL: {tool_call.args.get('url', '[Missing URL]')}"]
            elif tool_call.name == "back":
                render_texts += ["Go back to the previous page."]
            elif tool_call.name == "type":
                render_texts += [f'Type "{tool_call.args.get("text")}"']
            elif tool_call.name == "keypress":
                render_texts += [f"Press {tool_call.args.get('keys')}"]
            elif tool_call.name == "hold_key":
                render_texts += [f"Hold {tool_call.args.get('text')} for {tool_call.args.get('duration', 1.0)} seconds"]
            elif tool_call.name == "text_editor_view":
                render_texts += [f"View file: {tool_call.args.get('path')} (lines {tool_call.args.get('view_range')})"]
            elif tool_call.name == "text_editor_create":
                render_texts += [
                    f"Create file: {tool_call.args.get('path')} with content:\n{tool_call.args.get('file_text', '')}"
                ]
            elif tool_call.name == "text_editor_str_replace":
                render_texts += [
                    f"File: {tool_call.args.get('path')}\n**Find**\n{tool_call.args.get('old_str')}\n**Replace**\n{tool_call.args.get('new_str')}'"
                ]
            elif tool_call.name == "text_editor_insert":
                render_texts += [
                    f"In file: {tool_call.args.get('path')} at line {tool_call.args.get('insert_line')} insert\n{tool_call.args.get('new_str')}"
                ]
            elif tool_call.name == "terminal":
                render_texts += [f"Run command:\n{tool_call.args.get('command')}"]
            else:
                render_texts += [f"{tool_call.name.replace('_', ' ').capitalize()}"]

            if tool_call.name == "screenshot" and not screenshot:
                render_texts += ["Failed to get screenshot"]

        if not tool_calls:
            screenshot = None

        return {
            "text": "\n- ".join(filter(None, render_texts)),
            "image": f"data:image/webp;base64,{screenshot}" if screenshot else None,
        }

    async def _call_model(
        self,
        messages: list[AgentMessage],
        model: ChatModel,
        system_prompt: str,
        tools: list[dict] = [],
    ):
        system_message = ChatMessage(role="system", content=system_prompt)
        messages_for_api = self._format_message_for_api(messages) + [system_message]
        tool_definitions = [ToolDefinition(**tool) for tool in tools]

        response = gemini_send_message_to_model(
            messages=messages_for_api,
            tools=tool_definitions,
            model=model.name,
            api_key=model.ai_model_api.api_key,
            api_base_url=model.ai_model_api.api_base_url,
            deepthought=True,
            tracer=self.tracer,
        )
        return response

    async def _compress(self):
        # TODO: Implement trajectory compression for Gemini
        pass

    def get_instructions(self, environment_type: EnvironmentType, current_state: EnvState) -> str:
        """Return system instructions for the Gemini operator."""
        if environment_type == EnvironmentType.BROWSER:
            return dedent(
                f"""
                You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
                You operate a Chromium browser using Playwright.
                You cannot access the OS or filesystem.
                You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more.
                When doing mouse actions like click the points are normalized to 0-1000.
                You can use the additional back() and goto() helper functions to ease navigating the browser. If you see nothing, try goto duckduckgo.com
                When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
                When using your function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
                Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
                The current URL is {current_state.url}.
                You are allowed upto {self.max_iterations} iterations to complete the task.
                Do not loop on wait, screenshot for too many turns without taking any action.
                After initialization if the browser is blank, enter a website URL using the goto() function instead of waiting
                """
            ).lstrip()
        elif environment_type == EnvironmentType.COMPUTER:
            return dedent(
                f"""
                You are Khoj, a smart computer operating assistant. You help the users accomplish tasks using a computer.
                You can interact with the computer to perform tasks like clicking, typing, scrolling, and more.
                When doing mouse actions like click the points are normalized to 0-1000.
                When viewing a document or webpage it can be helpful to zoom out or scroll down to ensure you see everything before deciding something isn't available.
                When using your function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
                Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                Do not loop on wait, screenshot for too many turns without taking any action.
                You are allowed upto {self.max_iterations} iterations to complete the task.
                The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
                """
            ).lstrip()
        else:
            raise ValueError(f"Unsupported environment type for Gemini operator: {environment_type}")

    def get_tools(self, environment_type: EnvironmentType, current_state: EnvState) -> list[dict]:
        """Get tools for the grounding LLM, in Gemini API tool format"""
        tools = [
            {
                "name": "click",
                "description": "Click on a specific coordinate.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "x": {"type": "integer", "description": "X coordinate"},
                        "y": {"type": "integer", "description": "Y coordinate"},
                        "button": {
                            "type": "string",
                            "description": "Mouse button to click",
                            "enum": ["left", "right", "middle"],
                            "default": "left",
                        },
                    },
                    "required": ["x", "y"],
                },
            },
            {
                "name": "double_click",
                "description": "Double click on a specific coordinate.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "x": {"type": "integer", "description": "X coordinate"},
                        "y": {"type": "integer", "description": "Y coordinate"},
                    },
                    "required": ["x", "y"],
                },
            },
            {
                "name": "triple_click",
                "description": "Triple click on a specific coordinate.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "x": {"type": "integer", "description": "X coordinate"},
                        "y": {"type": "integer", "description": "Y coordinate"},
                    },
                    "required": ["x", "y"],
                },
            },
            {
                "name": "right_click",
                "description": "Right click on a specific coordinate.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "x": {"type": "integer", "description": "X coordinate"},
                        "y": {"type": "integer", "description": "Y coordinate"},
                    },
                    "required": ["x", "y"],
                },
            },
            {
                "name": "drag",
                "description": "Perform a drag-and-drop operation along a path.",
                "schema": {
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
            {
                "name": "keypress",
                "description": "Press a key or key combination.",
                "schema": {
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
            {
                "name": "type",
                "description": "Type text, usually into a focused input field.",
                "schema": {
                    "type": "object",
                    "properties": {"text": {"type": "string", "description": "Text to type"}},
                    "required": ["text"],
                },
            },
            {
                "name": "scroll",
                "description": "Scroll the page.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "x": {"type": "integer", "description": "X coordinate to scroll from"},
                        "y": {"type": "integer", "description": "Y coordinate to scroll from"},
                        "scroll_direction": {
                            "type": "string",
                            "enum": ["up", "down", "left", "right"],
                            "default": "down",
                        },
                        "scroll_amount": {"type": "number", "description": "Amount to scroll", "default": 2.0},
                    },
                    "required": [],  # None is strictly required
                },
            },
            {
                "name": "wait",
                "description": "Pause execution for a specified duration.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "number", "description": "Duration in seconds", "default": 1.0}
                    },
                    "required": [],
                },
            },
            {"name": "screenshot", "description": "Take a screenshot of the current screen.", "schema": {}},
            {
                "name": "mouse_down",
                "description": "Press and hold a mouse button.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "button": {
                            "type": "string",
                            "description": "Mouse button to press",
                            "enum": ["left", "right", "middle"],
                            "default": "left",
                        }
                    },
                    "required": [],
                },
            },
            {
                "name": "mouse_up",
                "description": "Release a mouse button.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "button": {
                            "type": "string",
                            "description": "Mouse button to release",
                            "enum": ["left", "right", "middle"],
                            "default": "left",
                        }
                    },
                    "required": [],
                },
            },
            {
                "name": "hold_key",
                "description": "Press and hold a key for a duration.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "text": {"type": "string", "description": "Key to hold"},
                        "duration": {"type": "number", "description": "Duration in seconds", "default": 1.0},
                    },
                    "required": ["text"],
                },
            },
            {
                "name": "key_up",
                "description": "Release a key.",
                "schema": {
                    "type": "object",
                    "properties": {"key": {"type": "string", "description": "Key to release"}},
                    "required": ["key"],
                },
            },
            {
                "name": "key_down",
                "description": "Press a key.",
                "schema": {
                    "type": "object",
                    "properties": {"key": {"type": "string", "description": "Key to press"}},
                    "required": ["key"],
                },
            },
            {"name": "cursor_position", "description": "Get the current cursor position.", "schema": {}},
            {
                "name": "text_editor_view",
                "description": "View contents of a file.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "path": {"type": "string", "description": "Path to the file"},
                        "view_range": {
                            "type": "array",
                            "items": {"type": "integer"},
                            "description": "[start_line, end_line]",
                        },
                    },
                    "required": ["path"],
                },
            },
            {
                "name": "text_editor_create",
                "description": "Create a new file with specified contents.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "path": {"type": "string", "description": "Path to the file"},
                        "file_text": {"type": "string", "description": "Content of the file"},
                    },
                    "required": ["path", "file_text"],
                },
            },
            {
                "name": "text_editor_str_replace",
                "description": "Execute an exact string match replacement on a file.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "path": {"type": "string", "description": "Path to the file"},
                        "old_str": {"type": "string", "description": "String to be replaced"},
                        "new_str": {"type": "string", "description": "String to replace with"},
                    },
                    "required": ["path", "old_str", "new_str"],
                },
            },
            {
                "name": "text_editor_insert",
                "description": "Insert new text after a specified line number.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "path": {"type": "string", "description": "Path to the file"},
                        "insert_line": {"type": "integer", "description": "Line number to insert after"},
                        "new_str": {"type": "string", "description": "String to insert"},
                    },
                    "required": ["path", "insert_line", "new_str"],
                },
            },
            {
                "name": "terminal",
                "description": "Execute a command in the terminal.",
                "schema": {
                    "type": "object",
                    "properties": {
                        "command": {"type": "string", "description": "Command to execute"},
                        "restart": {"type": "boolean", "description": "Restart the terminal", "default": False},
                    },
                    "required": ["command"],
                },
            },
        ]
        if environment_type == EnvironmentType.BROWSER:
            tools += [
                {
                    "name": "goto",
                    "description": "Navigate to a specific URL.",
                    "schema": {
                        "type": "object",
                        "properties": {"url": {"type": "string", "description": "Fully qualified URL"}},
                        "required": ["url"],
                    },
                },
                {
                    "name": "back",
                    "description": "navigate back to the previous page.",
                    "schema": {},
                },
            ]

        return tools
