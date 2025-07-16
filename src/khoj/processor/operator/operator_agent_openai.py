import json
import logging
from copy import deepcopy
from datetime import datetime
from textwrap import dedent
from typing import List, Optional, cast

from openai.types.responses import Response, ResponseOutputItem

from khoj.database.models import ChatModel
from khoj.processor.conversation.utils import AgentMessage
from khoj.processor.operator.operator_actions import (
    BackAction,
    ClickAction,
    DoubleClickAction,
    DragAction,
    GotoAction,
    KeypressAction,
    MoveAction,
    NoopAction,
    OperatorAction,
    Point,
    RequestUserAction,
    ScreenshotAction,
    ScrollAction,
    TypeAction,
    WaitAction,
)
from khoj.processor.operator.operator_agent_base import AgentActResult, OperatorAgent
from khoj.processor.operator.operator_environment_base import (
    EnvironmentType,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import get_openai_async_client, is_none_or_empty

logger = logging.getLogger(__name__)


# --- Anthropic Operator Agent ---
class OpenAIOperatorAgent(OperatorAgent):
    async def act(self, current_state: EnvState) -> AgentActResult:
        safety_check_prefix = "Say 'continue' after resolving the following safety checks to proceed:"
        safety_check_message = None
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
        self._commit_trace()  # Commit trace before next action
        system_prompt = self.get_instructions(self.environment_type, current_state)
        tools = self.get_tools(self.environment_type, current_state)
        if is_none_or_empty(self.messages):
            self.messages = [AgentMessage(role="user", content=self.query)]

        response = await self._call_model(self.vision_model, system_prompt, tools)
        self.messages += [AgentMessage(role="assistant", content=response.output)]
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
            else:
                logger.warning(f"Unsupported response block type: {block.type}")
                content = f"Unsupported response block type: {block.type}"
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
                    "image_url": f"data:image/webp;base64,{result_content['image']}",
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
            elif action_result["type"] == "computer_call" and action_result["status"] == "in_progress":
                if isinstance(result_content, dict):
                    result_content["status"] = "completed"  # Mark in-progress actions as completed
                action_result["output"] = result_content
            else:
                # Add text data
                action_result["output"] = result_content

        for idx in reversed(items_to_pop):
            agent_action.action_results.pop(idx)

        self.messages += [AgentMessage(role="environment", content=agent_action.action_results)]

    async def summarize(self, current_state: EnvState, summarize_prompt: str = None) -> str:
        summarize_prompt = summarize_prompt or self.summarize_prompt
        self.messages.append(AgentMessage(role="user", content=summarize_prompt))
        response = await self._call_model(self.vision_model, summarize_prompt, [])
        self.messages += [AgentMessage(role="assistant", content=response.output)]
        if not self.messages:
            return "No actions to summarize."
        return self._compile_response(self.messages[-1].content)

    async def _call_model(self, model: ChatModel, system_prompt, tools) -> Response:
        client = get_openai_async_client(model.ai_model_api.api_key, model.ai_model_api.api_base_url)
        if tools:
            model_name = "computer-use-preview"
        else:
            model_name = model.name

        # Format messages for OpenAI API
        messages_for_api = self._format_message_for_api(self.messages)
        # format messages for summary if model is not computer-use-preview
        if model_name != "computer-use-preview":
            messages_for_api = self._format_messages_for_summary(messages_for_api)

        response: Response = await client.responses.create(
            model=model_name,
            input=messages_for_api,
            instructions=system_prompt,
            tools=tools,
            parallel_tool_calls=False,
            truncation="auto",
        )

        logger.debug(f"Openai response: {response.model_dump_json()}")
        return response

    def _format_message_for_api(self, messages: list[AgentMessage]) -> list:
        """Format the message for OpenAI API."""
        formatted_messages: list = []
        for message in messages:
            if message.role == "assistant":
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
                    logger.warning(f"Expected message content list from assistant, got {type(message.content)}")
            elif message.role == "environment":
                formatted_messages.extend(message.content)
            else:
                if isinstance(message.content, list):
                    message.content = "\n".join([part["text"] for part in message.content if part["type"] == "text"])
                formatted_messages.append(
                    {
                        "role": message.role,
                        "content": message.content,
                    }
                )

        return formatted_messages

    def _compile_response(self, response_content: str | list[dict | ResponseOutputItem]) -> str:
        """Compile the response from model into a single string for prompt tracing."""
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
                    render_texts = [f"Open URL: {args.get('url', '[Missing URL]')}"]
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

    def get_instructions(self, environment_type: EnvironmentType, current_state: EnvState) -> str:
        """Return system instructions for the OpenAI operator."""
        if environment_type == EnvironmentType.BROWSER:
            return dedent(
                f"""
                <SYSTEM_CAPABILITY>
                * You are Khoj, a smart web browser operating assistant. You help the users accomplish tasks using a web browser.
                * You operate a single Chromium browser page using Playwright.
                * You cannot access the OS or filesystem.
                * You can interact with the web browser to perform tasks like clicking, typing, scrolling, and more using the computer_use_preview tool.
                * You can use the additional back() and goto() functions to navigate the browser.
                * Always use the goto() function to navigate to a specific URL. If you see nothing, try goto duckduckgo.com
                * When viewing a webpage it can be helpful to zoom out so that you can see everything on the page. Either that, or make sure you scroll down to see everything before deciding something isn't available.
                * When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
                * Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                * The current date is {datetime.today().strftime("%A, %B %-d, %Y")}.
                * The current URL is {current_state.url}.
                </SYSTEM_CAPABILITY>

                <IMPORTANT>
                * You are allowed upto {self.max_iterations} iterations to complete the task.
                * After initialization if the browser is blank, enter a website URL using the goto() function instead of waiting
                </IMPORTANT>
                """
            ).lstrip()
        elif environment_type == EnvironmentType.COMPUTER:
            return dedent(
                f"""
                <SYSTEM_CAPABILITY>
                * You are Khoj, a smart computer operating assistant. You help the users accomplish their tasks using a computer.
                * You can interact with the computer to perform tasks like clicking, typing, scrolling, and more using the computer_use_preview tool.
                * When viewing a document or webpage it can be helpful to zoom out or scroll down to ensure you see everything before deciding something isn't available.
                * When using your computer function calls, they take a while to run and send back to you. Where possible/feasible, try to chain multiple of these calls all into one function calls request.
                * Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                * You are allowed upto {self.max_iterations} iterations to complete the task.
                </SYSTEM_CAPABILITY>

                <CONTEXT>
                * The current date is {datetime.today().strftime("%A, %B %-d, %Y")}.
                </CONTEXT>
                """
            ).lstrip()
        else:
            raise ValueError(f"Unsupported environment type: {environment_type}")

    def get_tools(self, environment_type: EnvironmentType, current_state: EnvState) -> list[dict]:
        """Return the tools available for the OpenAI operator."""
        if environment_type == EnvironmentType.COMPUTER:
            # TODO: Get OS info from the environment
            # For now, assume Linux as the environment OS
            environment_os = "linux"
            # environment = "mac" if platform.system() == "Darwin" else "windows" if platform.system() == "Windows" else "linux"
        else:
            environment_os = "browser"

        tools = [
            {
                "type": "computer_use_preview",
                "display_width": current_state.width,
                "display_height": current_state.height,
                "environment": environment_os,
            }
        ]
        if environment_type == EnvironmentType.BROWSER:
            tools += [
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
        return tools

    def _format_messages_for_summary(self, formatted_messages: List[dict]) -> List[dict]:
        """Format messages for summary."""
        # Format messages to interact with non computer use AI models
        items_to_drop = []  # Track indices to drop reasoning messages
        for idx, msg in enumerate(formatted_messages):
            if isinstance(msg, dict) and "content" in msg:
                continue
            elif isinstance(msg, dict) and "output" in msg:
                # Drop current_url from output as not supported for non computer operations
                if "current_url" in msg["output"]:
                    del msg["output"]["current_url"]
                formatted_messages[idx] = {"role": "user", "content": [msg["output"]]}
            elif isinstance(msg, str):
                formatted_messages[idx] = {"role": "user", "content": [{"type": "input_text", "text": msg}]}
            else:
                text = self._compile_response([msg])
                if not text:
                    items_to_drop.append(idx)  # Track index to drop reasoning message
                else:
                    formatted_messages[idx] = {
                        "role": "assistant",
                        "content": [{"type": "output_text", "text": text}],
                    }

        # Remove reasoning messages for non-computer use models
        for idx in reversed(items_to_drop):
            formatted_messages.pop(idx)

        return formatted_messages
