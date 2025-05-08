import json
import logging
from copy import deepcopy
from datetime import datetime
from typing import Any, List, Optional

from anthropic.types.beta import BetaContentBlock

from khoj.processor.operator.operator_actions import *
from khoj.processor.operator.operator_agent_base import (
    AgentActResult,
    AgentMessage,
    OperatorAgent,
)
from khoj.processor.operator.operator_environment_base import EnvState, EnvStepResult
from khoj.utils.helpers import get_anthropic_async_client, is_none_or_empty

logger = logging.getLogger(__name__)


# --- Anthropic Operator Agent ---
class AnthropicOperatorAgent(OperatorAgent):
    async def act(self, current_state: EnvState) -> AgentActResult:
        client = get_anthropic_async_client(
            self.vision_model.ai_model_api.api_key, self.vision_model.ai_model_api.api_base_url
        )
        tool_version = "2025-01-24"
        betas = [f"computer-use-{tool_version}", "token-efficient-tools-2025-02-19"]
        temperature = 1.0
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
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
        if is_none_or_empty(self.messages):
            self.messages = [AgentMessage(role="user", content=self.query)]

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
        if self.vision_model.name.startswith("claude-3-7"):
            thinking = {"type": "enabled", "budget_tokens": 1024}

        messages_for_api = self._format_message_for_api(self.messages)
        response = await client.beta.messages.create(
            messages=messages_for_api,
            model=self.vision_model.name,
            system=system_prompt,
            tools=tools,
            betas=betas,
            thinking=thinking,
            max_tokens=4096,  # TODO: Make configurable?
            temperature=temperature,
        )

        logger.debug(f"Anthropic response: {response.model_dump_json()}")
        self.messages.append(AgentMessage(role="assistant", content=response.content))
        rendered_response = await self._render_response(response.content, current_state.screenshot)

        for block in response.content:
            if block.type == "tool_use":
                action_to_run: Optional[OperatorAction] = None
                tool_input = block.input
                tool_name = block.input.get("action") if block.name == "computer" else block.name
                tool_use_id = block.id

                try:
                    if tool_name == "mouse_move":
                        coord = tool_input.get("coordinate")
                        if coord:
                            action_to_run = MoveAction(x=coord[0], y=coord[1])
                    elif tool_name == "left_click":
                        coord = tool_input.get("coordinate")
                        if coord:
                            action_to_run = ClickAction(
                                x=coord[0], y=coord[1], button="left", modifier=tool_input.get("text")
                            )
                    elif tool_name == "right_click":
                        coord = tool_input.get("coordinate")
                        if coord:
                            action_to_run = ClickAction(x=coord[0], y=coord[1], button="right")
                    elif tool_name == "middle_click":
                        coord = tool_input.get("coordinate")
                        if coord:
                            action_to_run = ClickAction(x=coord[0], y=coord[1], button="middle")
                    elif tool_name == "double_click":
                        coord = tool_input.get("coordinate")
                        if coord:
                            action_to_run = DoubleClickAction(x=coord[0], y=coord[1])
                    elif tool_name == "triple_click":
                        coord = tool_input.get("coordinate")
                        if coord:
                            action_to_run = TripleClickAction(x=coord[0], y=coord[1])
                    elif tool_name == "left_click_drag":
                        start_coord = tool_input.get("start_coordinate")
                        end_coord = tool_input.get("coordinate")
                        if start_coord and end_coord:
                            action_to_run = DragAction(path=[Point(x=p[0], y=p[1]) for p in [start_coord, end_coord]])
                    elif tool_name == "left_mouse_down":
                        action_to_run = MouseDownAction(button="left")
                    elif tool_name == "left_mouse_up":
                        action_to_run = MouseUpAction(button="left")
                    elif tool_name == "type":
                        text = tool_input.get("text")
                        if text:
                            action_to_run = TypeAction(text=text)
                    elif tool_name == "scroll":
                        direction = tool_input.get("scroll_direction")
                        amount = tool_input.get("scroll_amount", 5)
                        coord = tool_input.get("coordinate")
                        x = coord[0] if coord else None
                        y = coord[1] if coord else None
                        if direction:
                            action_to_run = ScrollAction(scroll_direction=direction, scroll_amount=amount, x=x, y=y)
                    elif tool_name == "key":
                        text: str = tool_input.get("text")
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
                    else:
                        logger.warning(f"Unsupported Anthropic computer action type: {tool_name}")

                except Exception as e:
                    logger.error(f"Error converting Anthropic action {tool_name} ({tool_input}): {e}")

                if action_to_run:
                    actions.append(action_to_run)
                    action_results.append(
                        {
                            "type": "tool_result",
                            "tool_use_id": tool_use_id,
                            "content": None,  # Updated after environment step
                            "is_error": False,  # Updated after environment step
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
            if env_step.type == "image":
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

        # Append tool results to the message history
        self.messages += [AgentMessage(role="environment", content=agent_action.action_results)]

        # Mark the final tool result as a cache break point
        agent_action.action_results[-1]["cache_control"] = {"type": "ephemeral"}
        # Remove previous cache controls
        for msg in self.messages:
            if msg.role == "environment" and isinstance(msg.content, list):
                for block in msg.content:
                    if isinstance(block, dict) and "cache_control" in block:
                        del block["cache_control"]

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

    def compile_response(self, response_content: list[BetaContentBlock | Any]) -> str:
        """Compile Anthropic response into a single string."""
        if is_none_or_empty(response_content) or not all(hasattr(item, "type") for item in response_content):
            return response_content
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
    async def _render_response(response_content: list[BetaContentBlock], screenshot: Optional[str] = None) -> str:
        """Render Anthropic response, potentially including actual screenshots."""
        rendered_response = [""]
        for block in deepcopy(response_content):  # Use deepcopy to avoid modifying original
            if block.type == "text":
                rendered_response.append(block.text)
            elif block.type == "tool_use":
                block_input = {"action": block.name}
                if block.name == "computer":
                    block_input = block.input
                elif block.name == "goto":
                    block_input["url"] = block.input.get("url", "[Missing URL]")

                # If it's a screenshot action
                if isinstance(block_input, dict) and block_input.get("action") == "screenshot":
                    # Render the screenshot data if available
                    if screenshot:
                        block_input["image"] = f"data:image/webp;base64,{screenshot}"
                    else:
                        block_input["image"] = "[Failed to get screenshot]"

                rendered_response.append(f"**Action**: {json.dumps(block_input)}")
            elif block.type == "thinking":
                thinking_content = getattr(block, "thinking", None)
                if thinking_content:
                    rendered_response.append(f"**Thought**: {thinking_content}")

        return "\n- ".join(filter(None, rendered_response))
