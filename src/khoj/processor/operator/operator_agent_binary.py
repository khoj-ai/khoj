import json
import logging
from datetime import datetime
from typing import List, Optional

from openai.types.chat import ChatCompletion

from khoj.database.models import ChatModel
from khoj.processor.conversation.utils import construct_structured_message
from khoj.processor.operator.operator_actions import *
from khoj.processor.operator.operator_agent_base import (
    AgentActResult,
    AgentMessage,
    OperatorAgent,
)
from khoj.processor.operator.operator_environment_base import EnvState, EnvStepResult
from khoj.routers.helpers import send_message_to_model_wrapper
from khoj.utils.helpers import (
    convert_image_to_png,
    get_openai_async_client,
    is_none_or_empty,
)

logger = logging.getLogger(__name__)


# --- Binary Operator Agent ---
class BinaryOperatorAgent(OperatorAgent):
    """
    An OperatorAgent that uses two LLMs:
    1. Reasoning LLM: Determines the next high-level action based on the objective and current visual reasoning trajectory.
    2. Grounding LLM: Converts the high-level action into specific, executable browser actions.
    """

    def __init__(
        self,
        query: str,
        reasoning_model: ChatModel,
        grounding_model: ChatModel,
        max_iterations: int,
        tracer: dict,
    ):
        super().__init__(query, reasoning_model, max_iterations, tracer)  # Use reasoning model for primary tracking
        self.reasoning_model = reasoning_model
        self.grounding_model = grounding_model
        # Initialize openai api compatible client for grounding model
        self.grounding_client = get_openai_async_client(
            grounding_model.ai_model_api.api_key, grounding_model.ai_model_api.api_base_url
        )

    async def act(self, current_state: EnvState) -> AgentActResult:
        """
        Uses a two-step LLM process to determine and structure the next action.
        """
        self._commit_trace()  # Commit trace before next action

        # --- Step 1: Reasoning LLM determines high-level action ---
        reasoner_response = await self.act_reason(current_state)
        natural_language_action = reasoner_response["message"]
        if reasoner_response["type"] == "error":
            logger.error(natural_language_action)
            return AgentActResult(
                actions=[],
                action_results=[],
                rendered_response=natural_language_action,
            )
        elif reasoner_response["type"] == "done":
            return AgentActResult(
                actions=[],
                action_results=[],
                rendered_response=natural_language_action,
            )

        # --- Step 2: Grounding LLM converts NL action to structured action ---
        return await self.act_ground(natural_language_action, current_state)

    async def act_reason(self, current_state: EnvState) -> dict[str, str]:
        """
        Uses the reasoning LLM to determine the next high-level action based on the operation trajectory.
        """
        reasoning_system_prompt = f"""
# Introduction
* You are Khoj, a smart web browsing assistant. You help the user accomplish their task using a web browser.
* You are given the user's query and screenshots of the browser's state transitions.
* The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
* The current URL is {current_state.url}.

# Your Task
* First look at the screenshots carefully to notice all pertinent information.
* Then instruct a tool AI to perform the single most important next action to progress towards the user's goal.
* Make sure you scroll down to see everything before deciding something isn't available.
* Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.

# Tool AI Capabilities
* The tool AI only has access to the current screenshot and your instructions. It uses your instructions to perform the next action on the page.
* It can interact with the web browser with these actions: click, right click, double click, type, scroll, drag, wait, goto url and go back to previous page.
* It cannot access the OS, filesystem or application window. It just controls a single Chromium browser tab via Playwright.

# IMPORTANT
* You are allowed upto {self.max_iterations} iterations to complete the task.
* Explicitly tell the tool AI to use the `goto` function to navigate to a specific URL.
* Once you've verified that the main objective has been achieved, just say "DONE" (without the quotes). Do not say anything else.

# Examples
## Example 1
- use the 'goto' function to navigate to https://example.com
## Example 2
- 'click the blue login button located at the top right corner'
## Example 3
- 'scroll down the page to find the contact section'
## Example 4
- 'type the username example@email.com into the input field labeled Username')

# Instructions
Now describe a single high-level action to take next to progress towards the user's goal in detail.
Focus on the visual action and provide all necessary context.
""".strip()

        if is_none_or_empty(self.messages):
            query_text = f"**Main Objective**: {self.query}"
            query_screenshot = [f"data:image/png;base64,{convert_image_to_png(current_state.screenshot)}"]
            first_message_content = construct_structured_message(
                message=query_text,
                images=query_screenshot,
                model_type=self.reasoning_model.model_type,
                vision_enabled=True,
            )
            current_message = AgentMessage(role="user", content=first_message_content)
        else:
            current_message = self.messages.pop()
            query_text = self._get_message_text(current_message)
            query_screenshot = self._get_message_images(current_message)

        # Construct input for visual reasoner history
        visual_reasoner_history = self._format_message_for_api(self.messages)
        try:
            natural_language_action = await send_message_to_model_wrapper(
                query=query_text,
                query_images=query_screenshot,
                system_message=reasoning_system_prompt,
                conversation_log=visual_reasoner_history,
                agent_chat_model=self.reasoning_model,
                tracer=self.tracer,
            )
            self.messages.append(current_message)
            self.messages.append(AgentMessage(role="assistant", content=natural_language_action))

            if natural_language_action.strip().endswith("DONE"):
                return {"type": "done", "message": "Completed task."}

            logger.info(f"Reasoning LLM suggested action: {natural_language_action}")

        except Exception as e:
            return {"type": "error", "message": f"Error calling Reasoning LLM: {e}"}

        return {"type": "action", "message": natural_language_action}

    async def act_ground(self, natural_language_action: str, current_state: EnvState) -> AgentActResult:
        """Uses the grounding LLM to convert the high-level action into structured browser actions."""
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
        rendered_response = "No action determined."
        grounding_user_prompt = f"""
You are a GUI agent. You are given a task and a screenshot of the web browser tab you operate. You need to decide the next action to complete the task.
You control a single tab in a Chromium browser. You cannot access the OS, filesystem or the application window.
Always use the `goto` function to navigate to a specific URL. Ctrl+t, Ctrl+w, Ctrl+q, Ctrl+Shift+T, Ctrl+Shift+W are not allowed.

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
{natural_language_action}
"""

        # Define tools for the grounding LLM (OpenAI format)
        grounding_tools = [
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

        # Construct grounding LLM input (using only the latest user prompt + image)
        # We don't pass the full history here, as grounding depends on the *current* state + NL action
        grounding_messages_for_api = [
            {
                "role": "user",
                "content": [
                    {"type": "text", "text": grounding_user_prompt},
                    {
                        "type": "image_url",
                        "image_url": {
                            "url": f"data:image/png;base64,{convert_image_to_png(current_state.screenshot)}",
                            "detail": "high",
                        },
                    },
                ],
            }
        ]

        try:
            grounding_response: ChatCompletion = await self.grounding_client.chat.completions.create(
                model=self.grounding_model.name,
                messages=grounding_messages_for_api,
                tools=grounding_tools,
                tool_choice="required",
                temperature=0.0,  # Grounding should be precise
                max_tokens=1000,  # Allow for thoughts + actions
            )
            logger.debug(f"Grounding LLM response: {grounding_response.model_dump_json()}")

            grounding_message = grounding_response.choices[0].message
            # Parse tool calls
            if grounding_message.tool_calls:
                # Start rendering with vision output
                rendered_parts = [f"**Thought (Vision)**: {natural_language_action}"]
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
                rendered_response = f"**Thought (Vision)**: {natural_language_action}\n- **Response (Grounding)**: {grounding_message.content or '[No tool call]'}"

            # Update usage by grounding model
            self._update_usage(grounding_response.usage.prompt_tokens, grounding_response.usage.completion_tokens)
        except Exception as e:
            logger.error(f"Error calling Grounding LLM: {e}")
            rendered_response = (
                f"**Thought (Vision)**: {natural_language_action}\n- **Error**: Error contacting Grounding LLM: {e}"
            )
        return AgentActResult(
            actions=actions,
            action_results=action_results,
            rendered_response=rendered_response,
        )

    def add_action_results(self, env_steps: list[EnvStepResult], agent_action: AgentActResult) -> None:
        """
        Adds the results of executed actions back into the message history,
        formatted for the next OpenAI vision LLM call.
        """
        if not agent_action.action_results:
            return

        for idx, env_step in enumerate(env_steps):
            result_content = env_step.error or env_step.output or "[Action completed]"
            action_result = agent_action.action_results[idx]
            if env_step.type == "image":
                message = "**Action Result**: Took screenshot"
                images = [f"data:image/png;base64,{convert_image_to_png(env_step.screenshot_base64)}"]
            elif idx == len(env_steps) - 1:
                message = f"**Action Result**: {json.dumps(result_content)}"
                images = [f"data:image/png;base64,{convert_image_to_png(env_step.screenshot_base64)}"]
            else:
                message = f"**Action Result**: {json.dumps(result_content)}"
                images = []
            action_result["content"] = construct_structured_message(
                message=message,
                images=images,
                model_type=self.reasoning_model.model_type,
                vision_enabled=True,
            )

        # Append action results to history
        action_results_content = []
        for action_result in agent_action.action_results:
            action_results_content.extend(action_result["content"])
        self.messages.append(AgentMessage(role="environment", content=action_results_content))

    async def summarize(self, summarize_prompt: str, env_state: EnvState) -> str:
        conversation_history = self._format_message_for_api(self.messages)
        try:
            summary = await send_message_to_model_wrapper(
                query=summarize_prompt,
                conversation_log=conversation_history,
                agent_chat_model=self.reasoning_model,
                tracer=self.tracer,
            )
            # Set summary to last action message
            if not summary:
                raise ValueError("Summary is empty.")
        except Exception as e:
            logger.error(f"Error calling Reasoning LLM for summary: {e}")
            summary = "\n".join([self._get_message_text(msg) for msg in self.messages])

        # Append summary messages to history
        trigger_summary = AgentMessage(role="user", content=summarize_prompt)
        summary_message = AgentMessage(role="assistant", content=summary)
        self.messages.extend([trigger_summary, summary_message])

        return summary

    def compile_response(self, response_content: Union[str, List, dict]) -> str:
        """Compile response content into a string, handling OpenAI message structures."""
        if isinstance(response_content, str):
            return response_content

        if isinstance(response_content, dict) and response_content.get("role") == "assistant":
            # Grounding LLM response message (might contain tool calls)
            text_content = response_content.get("content")
            tool_calls = response_content.get("tool_calls")
            compiled = []
            if text_content:
                compiled.append(text_content)
            if tool_calls:
                for tc in tool_calls:
                    compiled.append(
                        f"**Action ({tc.get('function', {}).get('name')})**: {tc.get('function', {}).get('arguments')}"
                    )
            return "\n- ".join(filter(None, compiled))

        if isinstance(response_content, list):  # Tool results list
            compiled = ["**Tool Results**:"]
            for item in response_content:
                if isinstance(item, dict) and item.get("role") == "tool":
                    compiled.append(f"  - ID {item.get('tool_call_id')}: {item.get('content')}")
                else:
                    compiled.append(f"  - {str(item)}")  # Fallback
            return "\n".join(compiled)

        # Fallback for unexpected types
        return str(response_content)

    def _render_response(self, response: List, screenshot: Optional[str]) -> Optional[str]:
        """Render response for display. Currently uses compile_response."""
        # TODO: Could potentially enhance rendering, e.g., showing vision thought + grounding actions distinctly.
        # For now, rely on the structure built during the 'act' phase.
        return response  # The rendered_response is already built in act()

    def _get_message_text(self, message: AgentMessage) -> str:
        if isinstance(message.content, list):
            return "\n".join([item["text"] for item in message.content if item["type"] == "text"])
        return message.content

    def _get_message_images(self, message: AgentMessage) -> List[str]:
        images = []
        if isinstance(message.content, list):
            images = [item["image_url"]["url"] for item in message.content if item["type"] == "image_url"]
        return images

    def _format_message_for_api(self, messages: list[AgentMessage]) -> List[dict]:
        """Format operator agent messages into the Khoj conversation history format."""
        formatted_messages = [
            {
                "message": self._get_message_text(message),
                "images": self._get_message_images(message),
                "by": "you" if message.role in ["user", "environment"] else message.role,
            }
            for message in messages
        ]
        return {"chat": formatted_messages}

    def reset(self):
        """Reset the agent state."""
        super().reset()
