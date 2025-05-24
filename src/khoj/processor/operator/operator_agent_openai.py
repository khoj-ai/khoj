import json
import logging
from copy import deepcopy
from datetime import datetime
from typing import List, Optional, cast

from openai.types.responses import Response, ResponseOutputItem

from khoj.processor.operator.operator_actions import *
from khoj.processor.operator.operator_agent_base import (
    AgentActResult,
    AgentMessage,
    OperatorAgent,
)
from khoj.processor.operator.operator_environment_base import EnvState, EnvStepResult
from khoj.utils.helpers import get_openai_async_client, is_none_or_empty

logger = logging.getLogger(__name__)


# --- Anthropic Operator Agent ---
class OpenAIOperatorAgent(OperatorAgent):
    async def act(self, current_state: EnvState) -> AgentActResult:
        client = get_openai_async_client(
            self.vision_model.ai_model_api.api_key, self.vision_model.ai_model_api.api_base_url
        )
        safety_check_prefix = "Say 'continue' after resolving the following safety checks to proceed:"
        safety_check_message = None
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
        self._commit_trace()  # Commit trace before next action
        system_prompt = f"""<SYSTEM_CAPABILITY>
* You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
* You operate a single Chromium browser page using Playwright.
* You cannot access the OS or filesystem.
* You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more using the computer_use_preview tool.
* You can use the additional back() and goto() functions to navigate the browser.
* Always use the goto() function to navigate to a specific URL. If you see nothing, try goto duckduckgo.com
* When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
* When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
* Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
* The current date is {datetime.today().strftime('%A, %B %-d, %Y')}.
* The current URL is {current_state.url}.
</SYSTEM_CAPABILITY>

<IMPORTANT>
* You are allowed upto {self.max_iterations} iterations to complete the task.
* After initialization if the browser is blank, enter a website URL using the goto() function instead of waiting
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

        if is_none_or_empty(self.messages):
            self.messages = [AgentMessage(role="user", content=self.query)]

        messages_for_api = self._format_message_for_api(self.messages)
        response: Response = await client.responses.create(
            model="computer-use-preview",
            input=messages_for_api,
            instructions=system_prompt,
            tools=tools,
            parallel_tool_calls=False,  # Keep sequential for now
            max_output_tokens=4096,  # TODO: Make configurable?
            truncation="auto",
        )

        logger.debug(f"Openai response: {response.model_dump_json()}")
        self.messages += [AgentMessage(role="environment", content=response.output)]
        rendered_response = self._render_response(response.output, current_state.screenshot)

        last_call_id = None
        content = None
        for block in response.output:
            action_to_run: Optional[OperatorAction] = None
            if block.type == "function_call":
                last_call_id = block.call_id
                if block.name == "goto":
                    try:
                        args = json.loads(block.arguments)
                        url = args.get("url")
                        if url:
                            action_to_run = GotoAction(url=url)
                        else:
                            logger.warning("Goto function called without URL argument.")
                    except json.JSONDecodeError:
                        logger.warning(f"Failed to parse arguments for goto: {block.arguments}")
                elif block.name == "back":
                    action_to_run = BackAction()

            elif block.type == "computer_call":
                last_call_id = block.call_id
                if block.pending_safety_checks:
                    safety_check_body = "\n- ".join([check.message for check in block.pending_safety_checks])
                    safety_check_message = f"{safety_check_prefix}\n- {safety_check_body}"
                    action_to_run = RequestUserAction(request=safety_check_message)
                    actions.append(action_to_run)
                    break  # Stop processing actions if safety check needed

                # Convert OpenAI action to standardized BrowserAction
                openai_action = block.action
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
                        raise ValueError(f"Unsupported OpenAI computer action type: {action_type}")
                except ValueError as ve:
                    logger.error(f"Error converting OpenAI action {action_type}: {ve}")
                    content = f"ValueError: {action_type}: {ve}"
                except Exception as e:
                    logger.error(f"Error converting OpenAI action {action_type}: {e}")
                    content = f"Error: {action_type}: {e}"
            elif block.type == "message":
                rendered_response["text"] = response.output_text
            elif block.type == "reasoning":
                actions.append(NoopAction())
                # Add placeholder action result for reasoning
                # This is to prevent run termination.
                # It will be removed later by add_action_results func
                action_results.append(
                    {
                        "type": block.type,
                        "id": block.id,
                        "summary": [],
                    }
                )
            if action_to_run or content:
                actions.append(action_to_run)
            if action_to_run or content:
                # Prepare the action result
                action_results.append(
                    {
                        "type": f"{block.type}_output",
                        "output": content,  # Updated after environment step
                        "call_id": last_call_id,
                    }
                )

        self._update_usage(response.usage.input_tokens, response.usage.output_tokens)

        return AgentActResult(
            actions=actions,
            action_results=action_results,
            rendered_response=rendered_response,
        )

    def add_action_results(self, env_steps: list[EnvStepResult], agent_action: AgentActResult) -> None:
        if not agent_action.action_results:
            return

        # Update action results with results of applying suggested actions on the environment
        items_to_pop = []
        for idx, env_step in enumerate(env_steps):
            action_result = agent_action.action_results[idx]
            result_content = env_step.error or env_step.output or "[Action completed]"
            if env_step.type == "image" and isinstance(result_content, dict):
                # Add screenshot data in openai message format
                action_result["output"] = {
                    "type": "input_image",
                    "image_url": f'data:image/webp;base64,{result_content["image"]}',
                    "current_url": result_content["url"],
                }
            elif action_result["type"] == "computer_call_output" and idx == len(env_steps) - 1:
                # Always add screenshot, current url to last action result, when computer tool used
                action_result["output"] = {
                    "type": "input_image",
                    "image_url": f"data:image/webp;base64,{env_step.screenshot_base64}",
                    "current_url": env_step.current_url,
                }
            elif action_result["type"] == "reasoning":
                items_to_pop.append(idx)  # Mark placeholder reasoning action result for removal
                continue
            else:
                # Add text data
                action_result["output"] = result_content

        for idx in reversed(items_to_pop):
            agent_action.action_results.pop(idx)

        self.messages += [AgentMessage(role="environment", content=agent_action.action_results)]

    def _format_message_for_api(self, messages: list[AgentMessage]) -> list:
        """Format the message for OpenAI API."""
        formatted_messages: list = []
        for message in messages:
            if message.role == "environment":
                if isinstance(message.content, list):
                    # Remove reasoning message if not followed by computer call
                    if (
                        len(message.content) > 1
                        and all(hasattr(item, "type") for item in message.content)
                        and message.content[0].type == "reasoning"
                        and message.content[1].type != "computer_call"
                    ) or (
                        len(message.content) == 1
                        and all(hasattr(item, "type") for item in message.content)
                        and message.content[0].type == "reasoning"
                    ):
                        logger.warning(
                            f"Removing reasoning message not followed by a computer call action: {message.content}"
                        )
                        message.content.pop(0)
                    formatted_messages.extend(message.content)
                else:
                    logger.warning(f"Expected message content list from environment, got {type(message.content)}")
            else:
                formatted_messages.append(
                    {
                        "role": message.role,
                        "content": message.content,
                    }
                )
        return formatted_messages

    def compile_response(self, response_content: str | list[dict | ResponseOutputItem]) -> str:
        """Compile the response from model into a single string."""
        # Handle case where response content is a string.
        # This is the case when response content is a user query
        if isinstance(response_content, str):
            return response_content
        elif is_none_or_empty(response_content):
            return ""
        # Handle case where response_content is a dictionary and not ResponseOutputItem
        # This is the case when response_content contains action results
        if not hasattr(response_content[0], "type"):
            return "**Action**: " + json.dumps(response_content[0].get("output", "Noop"))

        compiled_response = [""]
        for block in deepcopy(response_content):
            block = cast(ResponseOutputItem, block)  # Ensure block is of type ResponseOutputItem
            # Handle different block types
            if block.type == "message":
                # Extract text content if available
                for content in block.content:
                    text_content = ""
                    if hasattr(content, "text"):
                        text_content += content.text
                    elif hasattr(content, "refusal"):
                        text_content += f"Refusal: {content.refusal}"
                    else:
                        text_content += content.model_dump_json()
                compiled_response.append(text_content)
            elif block.type == "function_call":
                block_function_input = {"action": block.name}
                if block.name == "goto":
                    try:
                        args = json.loads(block.arguments)
                        block_function_input["url"] = args.get("url", "[Missing URL]")
                    except json.JSONDecodeError:
                        block_function_input["arguments"] = block.arguments  # Show raw args on error
                compiled_response.append(f"**Action**: {json.dumps(block_function_input)}")
            elif block.type == "computer_call":
                block_computer_input = block.action
                # If it's a screenshot action
                if block_computer_input.type == "screenshot":
                    # Use a placeholder for screenshot data
                    block_input_render = block_computer_input.model_dump()
                    block_input_render["image"] = "[placeholder for screenshot]"
                    compiled_response.append(f"**Action**: {json.dumps(block_input_render)}")
                else:
                    compiled_response.append(f"**Action**: {block_computer_input.model_dump_json()}")
            elif block.type == "reasoning" and block.summary:
                compiled_response.append(f"**Thought**: {block.summary}")
        return "\n- ".join(filter(None, compiled_response))  # Filter out empty strings

    def _render_response(self, response_content: list[ResponseOutputItem], screenshot: str | None) -> dict:
        """Render OpenAI response for display, potentially including screenshots."""
        render_texts = []
        for block in deepcopy(response_content):  # Use deepcopy to avoid modifying original
            if block.type == "message":
                text_content = block.text if hasattr(block, "text") else block.model_dump_json()
                render_texts += [text_content]
            elif block.type == "function_call":
                if block.name == "goto":
                    args = json.loads(block.arguments)
                    render_texts = [f'Open URL: {args.get("url", "[Missing URL]")}']
                else:
                    render_texts += [block.name]
            elif block.type == "computer_call":
                block_input = block.action
                if block_input.type == "screenshot" and not screenshot:
                    render_texts += ["Failed to get screenshot"]
                elif block_input.type == "type":
                    render_texts += [f'Type "{block_input.text}"']
                elif block_input.type == "keypress":
                    render_texts += [f"Press {'+'.join(block_input.keys)}"]
                else:
                    render_texts += [f"{block_input.type.capitalize()}"]
            elif block.type == "reasoning" and block.summary:
                render_texts += [f"**Thought**: {block.summary}"]

        render_payload = {
            # Combine text into a single string and filter out empty strings
            "text": "\n- ".join(filter(None, render_texts)),
            # Add screenshot data if available
            "image": f"data:image/webp;base64,{screenshot}" if screenshot else None,
        }

        return render_payload
