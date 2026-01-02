import ast
import json
import logging
from copy import deepcopy
from datetime import datetime
from textwrap import dedent
from typing import List, Literal, Optional, cast

from anthropic.types.beta import BetaContentBlock, BetaTextBlock, BetaToolUseBlock

from khoj.database.models import ChatModel
from khoj.processor.conversation.anthropic.utils import is_reasoning_model
from khoj.processor.conversation.utils import AgentMessage
from khoj.processor.operator.operator_actions import (
    BackAction,
    ClickAction,
    CursorPositionAction,
    DoubleClickAction,
    DragAction,
    GotoAction,
    HoldKeyAction,
    KeypressAction,
    MouseDownAction,
    MouseUpAction,
    MoveAction,
    NoopAction,
    OperatorAction,
    Point,
    ScreenshotAction,
    ScrollAction,
    TerminalAction,
    TextEditorCreateAction,
    TextEditorInsertAction,
    TextEditorStrReplaceAction,
    TextEditorViewAction,
    TripleClickAction,
    TypeAction,
    WaitAction,
)
from khoj.processor.operator.operator_agent_base import AgentActResult, OperatorAgent
from khoj.processor.operator.operator_environment_base import (
    EnvironmentType,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import get_anthropic_async_client, is_none_or_empty

logger = logging.getLogger(__name__)


# --- Anthropic Operator Agent ---
class AnthropicOperatorAgent(OperatorAgent):
    async def act(self, current_state: EnvState) -> AgentActResult:
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
        self._commit_trace()  # Commit trace before next action

        system_prompt = self.get_instructions(self.environment_type, current_state)
        tools = self.get_tools(self.environment_type, current_state)

        if is_none_or_empty(self.messages):
            self.messages = [AgentMessage(role="user", content=self.query)]

        # Trigger trajectory compression if exceed size limit
        if len(self.messages) > self.message_limit:
            logger.debug("Compacting operator trajectory.")
            await self._compress()

        response_content = await self._call_model(
            messages=self.messages,
            model=self.vision_model,
            system_prompt=system_prompt,
            tools=tools,
            headers=self.model_default_headers(),
        )

        self.messages.append(AgentMessage(role="assistant", content=response_content))
        rendered_response = self._render_response(response_content, current_state.screenshot)

        # Parse actions from response
        for block in response_content:
            if block.type == "tool_use":
                content = None
                is_error = False

                action_to_run: Optional[OperatorAction] = None
                tool_input = block.input
                tool_name = block.input.get("action") if block.name == "computer" else block.name
                tool_use_id = block.id

                try:
                    if tool_name == "mouse_move":
                        coord = self.get_coordinates(tool_input)
                        if coord:
                            action_to_run = MoveAction(x=coord[0], y=coord[1])
                    elif tool_name == "left_click":
                        coord = self.get_coordinates(tool_input)
                        if coord:
                            action_to_run = ClickAction(
                                x=coord[0], y=coord[1], button="left", modifier=tool_input.get("text")
                            )
                    elif tool_name == "right_click":
                        coord = self.get_coordinates(tool_input)
                        if coord:
                            action_to_run = ClickAction(x=coord[0], y=coord[1], button="right")
                    elif tool_name == "middle_click":
                        coord = self.get_coordinates(tool_input)
                        if coord:
                            action_to_run = ClickAction(x=coord[0], y=coord[1], button="middle")
                    elif tool_name == "double_click":
                        coord = self.get_coordinates(tool_input)
                        if coord:
                            action_to_run = DoubleClickAction(x=coord[0], y=coord[1])
                    elif tool_name == "triple_click":
                        coord = self.get_coordinates(tool_input)
                        if coord:
                            action_to_run = TripleClickAction(x=coord[0], y=coord[1])
                    elif tool_name == "left_click_drag":
                        start_coord = self.get_coordinates(tool_input, key="start_coordinate")
                        end_coord = self.get_coordinates(tool_input)
                        if start_coord and end_coord:
                            action_to_run = DragAction(path=[Point(x=p[0], y=p[1]) for p in [start_coord, end_coord]])
                    elif tool_name == "left_mouse_down":
                        action_to_run = MouseDownAction(button="left")
                    elif tool_name == "left_mouse_up":
                        action_to_run = MouseUpAction(button="left")
                    elif tool_name == "type":
                        text: str = tool_input.get("text")
                        if text:
                            action_to_run = TypeAction(text=text)
                    elif tool_name == "scroll":
                        direction = tool_input.get("scroll_direction")
                        amount = int(tool_input.get("scroll_amount", 5))
                        coord = self.get_coordinates(tool_input)
                        x = coord[0] if coord else None
                        y = coord[1] if coord else None
                        if direction:
                            action_to_run = ScrollAction(scroll_direction=direction, scroll_amount=amount, x=x, y=y)
                    elif tool_name == "key":
                        text = tool_input.get("text")
                        if text:
                            action_to_run = KeypressAction(keys=text.split("+"))  # Split xdotool style
                    elif tool_name == "hold_key":
                        text = tool_input.get("text")
                        duration = tool_input.get("duration", 1.0)
                        if text:
                            action_to_run = HoldKeyAction(text=text, duration=duration)
                    elif tool_name == "wait":
                        duration = tool_input.get("duration", 1.0)
                        action_to_run = WaitAction(duration=duration)
                    elif tool_name == "screenshot":
                        action_to_run = ScreenshotAction()
                    elif tool_name == "cursor_position":
                        action_to_run = CursorPositionAction()
                    elif tool_name == "goto":
                        url = tool_input.get("url")
                        if url:
                            action_to_run = GotoAction(url=url)
                        else:
                            logger.warning("Goto tool called without URL.")
                    elif tool_name == "back":
                        action_to_run = BackAction()
                    elif tool_name == self.model_default_tool("terminal")["name"]:
                        command = tool_input.get("command")
                        restart = tool_input.get("restart", False)
                        if command:
                            action_to_run = TerminalAction(command=command, restart=restart)
                    elif tool_name == "str_replace_based_edit_tool":
                        # Handle text editor tool calls
                        command = tool_input.get("command")
                        if command == "view":
                            path = tool_input.get("path")
                            view_range = tool_input.get("view_range")
                            if path:
                                action_to_run = TextEditorViewAction(path=path, view_range=view_range)
                        elif command == "create":
                            path = tool_input.get("path")
                            file_text = tool_input.get("file_text", "")
                            if path:
                                action_to_run = TextEditorCreateAction(path=path, file_text=file_text)
                        elif command == "str_replace":
                            path = tool_input.get("path")
                            old_str = tool_input.get("old_str")
                            new_str = tool_input.get("new_str")
                            if path and old_str is not None and new_str is not None:
                                action_to_run = TextEditorStrReplaceAction(path=path, old_str=old_str, new_str=new_str)
                        elif command == "insert":
                            path = tool_input.get("path")
                            insert_line = tool_input.get("insert_line")
                            new_str = tool_input.get("new_str")
                            if path and insert_line is not None and new_str is not None:
                                action_to_run = TextEditorInsertAction(
                                    path=path, insert_line=insert_line, new_str=new_str
                                )
                        else:
                            logger.warning(f"Unsupported text editor command: {command}")
                    else:
                        logger.warning(f"Unsupported Anthropic computer action type: {tool_name}")

                except Exception as e:
                    error_msg = f"Error converting Anthropic action {tool_name} ({tool_input}): {e}"
                    logger.error(error_msg)
                    content = error_msg
                    is_error = True
                    action_to_run = NoopAction()

                if action_to_run:
                    actions.append(action_to_run)
                    action_results.append(
                        {
                            "type": "tool_result",
                            "tool_use_id": tool_use_id,
                            "content": content,  # Updated after environment step
                            "is_error": is_error,  # Updated after environment step
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

        # Update action results with results of applying suggested actions on the environment
        for idx, env_step in enumerate(env_steps):
            action_result = agent_action.action_results[idx]
            result_content = env_step.error or env_step.output or "[Action completed]"
            if env_step.type == "image" and isinstance(result_content, dict):
                # Add screenshot data in anthropic message format
                action_result["content"] = [
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/webp",
                            "data": result_content["image"],
                        },
                    }
                ]
            else:
                # Add text data
                action_result["content"] = result_content
            if env_step.error:
                action_result["is_error"] = True

        # Remove previous cache controls
        for msg in self.messages:
            if isinstance(msg.content, list):
                for block in msg.content:
                    if isinstance(block, dict) and "cache_control" in block:
                        del block["cache_control"]

        # Mark the final tool result as a cache break point
        agent_action.action_results[-1]["cache_control"] = {"type": "ephemeral"}

        # Append tool results to the message history
        self.messages += [AgentMessage(role="environment", content=agent_action.action_results)]

    def _format_message_for_api(self, messages: list[AgentMessage]) -> list[dict]:
        """Format Anthropic response into a single string."""
        formatted_messages = []
        for message in messages:
            role = "user" if message.role == "environment" else message.role
            content = (
                [{"type": "text", "text": message.content}]
                if not isinstance(message.content, list)
                else message.content
            )
            formatted_messages.append(
                {
                    "role": role,
                    "content": content,
                }
            )
        return formatted_messages

    def _compile_response(self, response_content: list[BetaContentBlock | dict] | str) -> str:
        """Compile Anthropic response into a single string."""
        if isinstance(response_content, str):
            return response_content
        elif is_none_or_empty(response_content):
            return ""
        # action results are a list dictionaries,
        # beta content blocks are objects with a type attribute
        elif isinstance(response_content[0], dict):
            return json.dumps(response_content)

        compiled_response = [""]
        for block in deepcopy(response_content):
            block = cast(BetaContentBlock, block)  # Ensure block is of type BetaContentBlock
            if block.type == "text":
                compiled_response.append(block.text)
            elif block.type == "tool_use":
                block_input = {"action": block.name}
                if block.name in (
                    self.model_default_tool("computer")["name"],
                    self.model_default_tool("editor")["name"],
                    self.model_default_tool("terminal")["name"],
                ):
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

    def _render_response(self, response_content: list[BetaContentBlock], screenshot: str | None) -> dict:
        """Render Anthropic response, potentially including actual screenshots."""
        render_texts = []
        for block in deepcopy(response_content):  # Use deepcopy to avoid modifying original
            if block.type == "thinking":
                thinking_content = getattr(block, "thinking", None)
                if thinking_content:
                    render_texts += [f"**Thought**: {thinking_content}"]
            elif block.type == "text":
                render_texts += [block.text]
            elif block.type == "tool_use":
                if block.name == "goto":
                    render_texts += [f"Open URL: {block.input.get('url', '[Missing URL]')}"]
                elif block.name == "back":
                    render_texts += ["Go back to the previous page."]
                elif block.name == "computer":
                    block_input = block.input
                    if not isinstance(block_input, dict):
                        render_texts += [json.dumps(block_input)]
                    # Handle computer action details
                    elif "action" in block_input:
                        action = block_input["action"]
                        if action == "type":
                            text: str = block_input.get("text")
                            if text:
                                render_texts += [f'Type "{text}"']
                        elif action == "key":
                            text = block_input.get("text")
                            if text:
                                render_texts += [f"Press {text}"]
                        elif action == "hold_key":
                            text = block_input.get("text")
                            duration = block_input.get("duration", 1.0)
                            if text:
                                render_texts += [f"Hold {text} for {duration} seconds"]
                        else:
                            # Handle other actions
                            render_texts += [f"{action.capitalize()}"]
                elif block.name == self.model_default_tool("editor")["name"]:
                    # Handle text editor actions
                    command = block.input.get("command")
                    if command == "view":
                        path = block.input.get("path")
                        view_range = block.input.get("view_range")
                        if path:
                            render_texts += [f"View file: {path} (lines {view_range})"]
                    elif command == "create":
                        path = block.input.get("path")
                        file_text = block.input.get("file_text", "")
                        if path:
                            render_texts += [f"Create file: {path} with content:\n{file_text}"]
                    elif command == "str_replace":
                        path = block.input.get("path")
                        old_str = block.input.get("old_str")
                        new_str = block.input.get("new_str")
                        if path and old_str is not None and new_str is not None:
                            render_texts += [f"File: {path}\n**Find**\n{old_str}\n**Replace**\n{new_str}'"]
                    elif command == "insert":
                        path = block.input.get("path")
                        insert_line = block.input.get("insert_line")
                        new_str = block.input.get("new_str")
                        if path and insert_line is not None and new_str is not None:
                            render_texts += [f"In file: {path} at line {insert_line} insert\n{new_str}"]
                    render_texts += [f"Edit file: {block.input['path']}"]
                elif block.name == self.model_default_tool("terminal")["name"]:
                    render_texts += [f"Run command:\n{block.input['command']}"]
                # If screenshot is not available when screenshot action was requested
                if isinstance(block.input, dict) and block.input.get("action") == "screenshot" and not screenshot:
                    render_texts += ["Failed to get screenshot"]

        # Do not show screenshot if no actions requested
        if all([block.type != "tool_use" for block in response_content]):
            # If all blocks are not tool_use, return None
            screenshot = None

        # Create render payload
        render_payload = {
            # Combine text into a single string and filter out empty strings
            "text": "\n- ".join(filter(None, render_texts)),
            # Add screenshot data if available
            "image": f"data:image/webp;base64,{screenshot}" if screenshot else None,
        }

        return render_payload

    async def _call_model(
        self,
        messages: list[AgentMessage],
        model: ChatModel,
        system_prompt: str,
        tools: list[dict] = [],
        headers: list[str] = [],
        temperature: float = 1.0,
        max_tokens: int = 4096,
    ) -> list[BetaContentBlock]:
        client = get_anthropic_async_client(model.ai_model_api.api_key, model.ai_model_api.api_base_url)
        thinking: dict[str, str | int] = {"type": "disabled"}
        system = [{"type": "text", "text": system_prompt, "cache_control": {"type": "ephemeral"}}]
        kwargs: dict = {}
        if is_reasoning_model(model.name):
            thinking = {"type": "enabled", "budget_tokens": 1024}
        if headers:
            kwargs["betas"] = headers
        if tools:
            tools[-1]["cache_control"] = {"type": "ephemeral"}  # Mark last tool as cache break point
            kwargs["tools"] = tools

        messages_for_api = self._format_message_for_api(messages)
        try:
            response = await client.beta.messages.create(
                messages=messages_for_api,
                model=model.name,
                system=system,
                thinking=thinking,
                max_tokens=max_tokens,
                temperature=temperature,
                **kwargs,
            )
            response_content = response.content
        except Exception as e:
            # create a response block with error message
            logger.error(f"Error during Anthropic API call: {e}")
            error_str = e.message if hasattr(e, "message") else str(e)
            response = None
            response_content = [BetaTextBlock(text=f"Communication Error: {error_str}", type="text")]

        if response:
            logger.debug(f"Anthropic response: {response.model_dump_json()}")
            self._update_usage(
                response.usage.input_tokens,
                response.usage.output_tokens,
                response.usage.cache_read_input_tokens,
                response.usage.cache_creation_input_tokens,
            )
        self.tracer["temperature"] = temperature
        return response_content

    async def _compress(self):
        # 1. Prepare messages for compression
        original_messages = list(self.messages)
        messages_to_summarize = self.messages[: self.compress_length]
        # ensure last message isn't a tool call request
        if messages_to_summarize[-1].role == "assistant" and (
            any(isinstance(block, BetaToolUseBlock) for block in messages_to_summarize[-1].content)
            or any(block["type"] == "tool_use" for block in messages_to_summarize[-1].content)
        ):
            messages_to_summarize.pop()

        summarize_prompt = f"Summarize your research and computer use till now to help answer my query:\n{self.query}"
        summarize_message = AgentMessage(role="user", content=summarize_prompt)
        system_prompt = dedent(
            """
            You are a computer operator with meticulous communication skills. You can condense your partial computer use traces and research into an appropriately detailed summary.
            When requested summarize your key actions, results and findings until now to achieve the user specified task.
            Your summary should help you remember the key information required to both complete the task and later generate a final report.
            """
        )

        # 2. Get summary of operation trajectory
        try:
            response_content = await self._call_model(
                messages=messages_to_summarize + [summarize_message],
                model=self.vision_model,
                system_prompt=system_prompt,
                max_tokens=8192,
            )
        except Exception as e:
            # create a response block with error message
            logger.error(f"Error during Anthropic API call: {e}")
            error_str = e.message if hasattr(e, "message") else str(e)
            response_content = [BetaTextBlock(text=f"Communication Error: {error_str}", type="text")]

        summary_message = AgentMessage(role="assistant", content=response_content)

        # 3. Rebuild message history with condensed trajectory
        primary_task = [original_messages.pop(0)]
        condensed_trajectory = [summarize_message, summary_message]
        recent_trajectory = original_messages[self.compress_length - 1 :]  # -1 since we popped the first message
        # ensure first message isn't a tool result
        if recent_trajectory[0].role == "environment" and any(
            block["type"] == "tool_result" for block in recent_trajectory[0].content
        ):
            recent_trajectory.pop(0)

        self.messages = primary_task + condensed_trajectory + recent_trajectory

    def get_coordinates(self, tool_input: dict, key: str = "coordinate") -> Optional[list | tuple]:
        """Get coordinates from tool input."""
        raw_coord = tool_input.get(key)
        if not raw_coord:
            return None
        try:
            coord = ast.literal_eval(raw_coord) if isinstance(raw_coord, str) else raw_coord
        except (ValueError, SyntaxError):
            logger.warning(f"Could not parse coordinate from value: {raw_coord}")
            return None

        if not isinstance(coord, (list, tuple)) or not len(coord) == 2:
            logger.warning(f"Parsed coordinate string '{raw_coord}' is not a 2-element list/tuple: {coord}")
            return None

        return coord

    def model_default_tool(self, tool_type: Literal["computer", "editor", "terminal"]) -> dict[str, str]:
        """Get the default tool of specified type for the given model."""
        if self.vision_model.name.startswith("claude-3-7-sonnet"):
            if tool_type == "computer":
                return {"name": "computer", "type": "computer_20250124"}
            elif tool_type == "editor":
                return {"name": "str_replace_editor", "type": "text_editor_20250124"}
            elif tool_type == "terminal":
                return {"name": "bash", "type": "bash_20250124"}
        elif self.vision_model.name.startswith("claude-sonnet-4") or self.vision_model.name.startswith("claude-opus-4"):
            if tool_type == "computer":
                return {"name": "computer", "type": "computer_20250124"}
            elif tool_type == "editor":
                return {"name": "str_replace_based_edit_tool", "type": "text_editor_20250728"}
            elif tool_type == "terminal":
                return {"name": "bash", "type": "bash_20250124"}
        raise ValueError(f"Unsupported tool type for model '{self.vision_model.name}': {tool_type}")

    def model_default_headers(self) -> list[str]:
        """Get the default computer use headers for the given model."""
        if self.vision_model.name.startswith("claude-3-7-sonnet"):
            return ["computer-use-2025-01-24", "token-efficient-tools-2025-02-19"]
        elif self.vision_model.name.startswith("claude-sonnet-4") or self.vision_model.name.startswith("claude-opus-4"):
            return ["computer-use-2025-01-24"]
        else:
            return []

    def get_instructions(self, environment_type: EnvironmentType, current_state: EnvState) -> str:
        """Return system instructions for the Anthropic operator."""
        if environment_type == EnvironmentType.BROWSER:
            return dedent(
                f"""
                <SYSTEM_CAPABILITY>
                * You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
                * You operate a Chromium browser using Playwright via the 'computer' tool.
                * You cannot access the OS or filesystem.
                * You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more.
                * You can use the additional back() and goto() helper functions to ease navigating the browser. If you see nothing, try goto duckduckgo.com
                * When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
                * When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
                * Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                * The current date is {datetime.today().strftime("%A, %B %-d, %Y")}.
                * The current URL is {current_state.url}.
                </SYSTEM_CAPABILITY>

                <IMPORTANT>
                * You are allowed upto {self.max_iterations} iterations to complete the task.
                * Do not loop on wait, screenshot for too many turns without taking any action.
                * After initialization if the browser is blank, enter a website URL using the goto() function instead of waiting
                </IMPORTANT>
                """
            ).lstrip()
        elif environment_type == EnvironmentType.COMPUTER:
            return dedent(
                f"""
                <SYSTEM_CAPABILITY>
                * You are Khoj, a smart computer operating assistant. You help the users accomplish tasks using a computer.
                * You can interact with the computer to perform tasks like clicking, typing, scrolling, and more.
                * When viewing a document or webpage it can be helpful to zoom out or scroll down to ensure you see everything before deciding something isn't available.
                * When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
                * Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                * Do not loop on wait, screenshot for too many turns without taking any action.
                * You are allowed upto {self.max_iterations} iterations to complete the task.
                </SYSTEM_CAPABILITY>

                <CONTEXT>
                * The current date is {datetime.today().strftime("%A, %B %-d, %Y")}.
                </CONTEXT>
                """
            ).lstrip()
        else:
            raise ValueError(f"Unsupported environment type for Anthropic operator: {environment_type}")

    def get_tools(self, environment: EnvironmentType, current_state: EnvState) -> list[dict]:
        """Return the tools available for the Anthropic operator."""
        tools: list[dict] = [
            {
                "type": self.model_default_tool("computer")["type"],
                "name": "computer",
                "display_width_px": current_state.width,
                "display_height_px": current_state.height,
            },
            {
                "type": self.model_default_tool("editor")["type"],
                "name": self.model_default_tool("editor")["name"],
            },
            {
                "type": self.model_default_tool("terminal")["type"],
                "name": self.model_default_tool("terminal")["name"],
            },
        ]

        if environment == "browser":
            tools += [
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

        return tools
