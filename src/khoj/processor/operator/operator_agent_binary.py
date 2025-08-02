import json
import logging
from datetime import datetime
from textwrap import dedent
from typing import List, Optional

from khoj.database.models import ChatMessageModel, ChatModel
from khoj.processor.conversation.utils import (
    AgentMessage,
    OperatorRun,
    construct_structured_message,
)
from khoj.processor.operator.grounding_agent import GroundingAgent
from khoj.processor.operator.grounding_agent_uitars import GroundingAgentUitars
from khoj.processor.operator.operator_actions import OperatorAction, WaitAction
from khoj.processor.operator.operator_agent_base import AgentActResult, OperatorAgent
from khoj.processor.operator.operator_environment_base import (
    EnvironmentType,
    EnvState,
    EnvStepResult,
)
from khoj.routers.helpers import send_message_to_model_wrapper
from khoj.utils.helpers import get_openai_async_client, is_none_or_empty

logger = logging.getLogger(__name__)


# --- Binary Operator Agent ---
class BinaryOperatorAgent(OperatorAgent):
    """
    An OperatorAgent that uses two LLMs:
    1. Reasoning LLM: Determines the next high-level action based on the objective and current visual reasoning trajectory.
    2. Grounding LLM: Converts the high-level action into specific, actions executable on the environment.
    """

    def __init__(
        self,
        query: str,
        reasoning_model: ChatModel,
        grounding_model: ChatModel,
        environment_type: EnvironmentType,
        max_iterations: int,
        max_context: int,
        chat_history: List[AgentMessage] = [],
        previous_trajectory: Optional[OperatorRun] = None,
        tracer: dict = {},
    ):
        super().__init__(
            query,
            reasoning_model,
            environment_type,
            max_iterations,
            max_context,
            chat_history,
            previous_trajectory,
            tracer,
        )  # Use reasoning model for primary tracking
        self.reasoning_model = reasoning_model
        self.grounding_model = grounding_model
        # Initialize openai api compatible client for grounding model
        grounding_client = get_openai_async_client(
            grounding_model.ai_model_api.api_key, grounding_model.ai_model_api.api_base_url
        )

        self.grounding_agent: GroundingAgent | GroundingAgentUitars = None
        if "ui-tars-1.5" in grounding_model.name:
            self.grounding_agent = GroundingAgentUitars(
                grounding_model.name, self.environment_type, grounding_client, max_iterations, tracer=tracer
            )
        else:
            self.grounding_agent = GroundingAgent(
                grounding_model.name, self.environment_type, grounding_client, max_iterations, tracer=tracer
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
                rendered_response={"text": natural_language_action, "image": None},
            )
        elif reasoner_response["type"] == "done":
            return AgentActResult(
                actions=[],
                action_results=[],
                rendered_response={"text": natural_language_action, "image": None},
            )

        # --- Step 2: Grounding LLM converts NL action to structured action ---
        return await self.act_ground(natural_language_action, current_state)

    async def act_reason(self, current_state: EnvState) -> dict[str, str]:
        """
        Uses the reasoning LLM to determine the next high-level action based on the operation trajectory.
        """
        reasoning_system_prompt = self.get_instruction(self.environment_type, current_state)
        if is_none_or_empty(self.messages):
            query_text = f"**Main Objective**: {self.query}"
            query_screenshot = [f"data:image/webp;base64,{current_state.screenshot}"]
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
            raw_response = await send_message_to_model_wrapper(
                query=query_text,
                query_images=query_screenshot,
                system_message=reasoning_system_prompt,
                chat_history=visual_reasoner_history,
                agent_chat_model=self.reasoning_model,
                tracer=self.tracer,
            )
            natural_language_action = raw_response.text

            if not isinstance(natural_language_action, str) or not natural_language_action.strip():
                raise ValueError(f"Natural language action is empty or not a string. Got {natural_language_action}")

            self.messages.append(current_message)
            self.messages.append(AgentMessage(role="assistant", content=natural_language_action))

            if natural_language_action.strip().endswith("DONE"):
                return {"type": "done", "message": "Completed task."}

            logger.info(f"Reasoning LLM suggested action: {natural_language_action}")

        except Exception as e:
            logger.error(f"Error calling Reasoning LLM: {e}", exc_info=True)
            return {"type": "error", "message": f"Error calling Reasoning LLM: {e}"}

        return {"type": "action", "message": natural_language_action}

    async def act_ground(self, action_instruction: str, current_state: EnvState) -> AgentActResult:
        """Uses the grounding LLM to convert the high-level action into structured browser actions."""
        actions: List[OperatorAction] = []
        action_results: List[dict] = []
        rendered_parts = [f"**Thought (Vision)**: {action_instruction}"]

        try:
            grounding_response, actions = await self.grounding_agent.act(action_instruction, current_state)

            # Process grounding response
            if grounding_response.strip().endswith("DONE"):
                # Ignore DONE response by the grounding agent. Reasoning agent handles termination.
                actions.append(WaitAction(duration=1.0))
                rendered_parts += ["Nothing to do."]
            elif grounding_response.strip().endswith("FAIL"):
                # Ignore FAIL response by the grounding agent. Reasoning agent handles termination.
                actions.append(WaitAction(duration=1.0))
                rendered_parts += ["Could not process response."]
            else:
                grounding_thoughts = grounding_response.rsplit("\nAction: ", 1)[0]
                rendered_parts += [f"**Thought (Grounding)**: {grounding_thoughts}"]
                for action in actions:
                    if action.type == "type":
                        rendered_parts += [f'**Action**: Type "{action.text}"']
                    elif action.type == "keypress":
                        rendered_parts += [f'**Action**: Press "{action.keys}"']
                    elif action.type == "hold_key":
                        rendered_parts += [f'**Action**: Hold "{action.text}" for {action.duration} seconds']
                    elif action.type == "key_up":
                        rendered_parts += [f'**Action**: Release Key "{action.key}"']
                    elif action.type == "key_down":
                        rendered_parts += [f'**Action**: Press Key "{action.key}"']
                    elif action.type == "screenshot" and not current_state.screenshot:
                        rendered_parts += ["**Error**: Failed to take screenshot"]
                    elif action.type == "goto":
                        rendered_parts += [f"**Action**: Open URL {action.url}"]
                    else:
                        rendered_parts += [f"**Action**: {action.type}"]
            action_results += [{"content": None}]  # content set after environment step
        except Exception as e:
            logger.error(f"Error calling Grounding LLM: {e}", exc_info=True)
            rendered_parts += [f"**Error**: Error contacting Grounding LLM: {e}"]

        rendered_response = self._render_response(rendered_parts, current_state.screenshot)

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
                images = [f"data:image/png;base64,{env_step.screenshot_base64}"]
            elif idx == len(env_steps) - 1:
                message = f"**Action Result**: {json.dumps(result_content)}"
                images = [f"data:image/png;base64,{env_step.screenshot_base64}"]
            else:
                message = f"**Action Result**: {json.dumps(result_content)}"
                images = []
            if not images:
                action_result["content"] = [{"type": "text", "text": message}]
            else:
                action_result["content"] = construct_structured_message(
                    message=message,
                    images=images,
                    model_type=self.reasoning_model.model_type,
                    vision_enabled=True,
                )

        # Append action results to history
        action_results_content = []
        for action_result in agent_action.action_results:
            if not action_result.get("content"):
                logger.error("Action result content is empty or None: {action_result}")
            action_results_content.extend(action_result["content"])
        self.messages.append(AgentMessage(role="environment", content=action_results_content))

    async def summarize(self, env_state: EnvState, summarize_prompt: str = None) -> str:
        summarize_prompt = summarize_prompt or self.summarize_prompt
        conversation_history = self._format_message_for_api(self.messages)
        try:
            summary = await send_message_to_model_wrapper(
                query=summarize_prompt,
                chat_history=conversation_history,
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
        summary_message = AgentMessage(role="assistant", content=summary.text)
        self.messages.extend([trigger_summary, summary_message])

        return summary.text

    def _compile_response(self, response_content: str | List) -> str:
        """Compile response content into a string, handling OpenAI message structures."""
        if isinstance(response_content, str):
            return response_content

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

    def _render_response(self, response: List, screenshot: str | None) -> dict:
        """Render response for display"""
        render_payload = {
            "text": "\n- ".join(response),
            "image": f"data:image/webp;base64,{screenshot}" if screenshot else None,
        }
        return render_payload

    def _get_message_text(self, message: AgentMessage) -> str:
        if isinstance(message.content, list):
            return "\n".join([item["text"] for item in message.content if item["type"] == "text"])
        return message.content

    def _get_message_images(self, message: AgentMessage) -> List[str]:
        images = []
        if isinstance(message.content, list):
            images = [item["image_url"]["url"] for item in message.content if item["type"] == "image_url"]
        return images

    def _format_message_for_api(self, messages: list[AgentMessage]) -> List[ChatMessageModel]:
        """Format operator agent messages into the Khoj conversation history format."""
        formatted_messages = [
            ChatMessageModel(
                message=self._get_message_text(message),
                images=self._get_message_images(message),
                by="you" if message.role in ["user", "environment"] else message.role,
            )
            for message in messages
        ]
        return formatted_messages

    def get_instruction(self, environment_type: EnvironmentType, env_state: EnvState) -> str:
        """Get the system instruction for the reasoning agent."""
        if environment_type == EnvironmentType.BROWSER:
            return dedent(
                f"""
                # Introduction
                * You are Khoj, a smart and resourceful web browsing assistant. You help the user accomplish their task using a web browser.
                * You are given the user's query and screenshots of the browser's state transitions.
                * The current date is {datetime.today().strftime("%A, %B %-d, %Y")}.
                * The current URL is {env_state.url}.

                # Your Task
                * First look at the screenshots carefully to notice all pertinent information.
                * Then instruct a tool AI to perform the next action that will help you progress towards the user's goal.
                * Make sure you scroll down to see everything before deciding something isn't available.
                * Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                * Use your creativity to find alternate ways to make progress if you get stuck at any point.

                # Tool AI Capabilities
                * The tool AI only has access to the current screenshot and your instructions. It uses your instructions to perform the next action on the page.
                * It can interact with the web browser with these actions: click, right click, double click, type, scroll, drag, wait, goto url and go back to previous page.
                * It cannot access the OS, filesystem or application window. It just controls a single Chromium browser tab via Playwright.

                # IMPORTANT
                * You are allowed upto {self.max_iterations} iterations to complete the task.
                * To navigate to a specific URL, put "GOTO <URL>" (without quotes) on the last line of your response.
                * To navigate back to the previous page, end your response with "BACK" (without quotes).
                * Once you've verified that the main objective has been achieved, end your response with "DONE" (without quotes).

                # Examples
                ## Example 1
                GOTO https://example.com
                ## Example 2
                click the blue login button located at the top right corner
                ## Example 3
                scroll down the page
                ## Example 4
                type the username example@email.com into the input field labeled Username
                ## Example 5
                DONE

                # Instructions
                Now describe a single high-level action to take next to progress towards the user's goal in detail.
                Focus on the visual action and provide all necessary context.
                """
            ).strip()

        elif environment_type == EnvironmentType.COMPUTER:
            return dedent(
                f"""
                # Introduction
                * You are Khoj, a smart and resourceful computer assistant. You help the user accomplish their task using a computer.
                * You are given the user's query and screenshots of the computer's state transitions.
                * The current date is {datetime.today().strftime("%A, %B %-d, %Y")}.

                # Your Task
                * First look at the screenshots carefully to notice all pertinent information.
                * Then instruct a tool AI to perform the next action that will help you progress towards the user's goal.
                * Make sure you scroll down to see everything before deciding something isn't available.
                * Perform web searches using DuckDuckGo. Don't use Google even if requested as the query will fail.
                * Use your creativity to find alternate ways to make progress if you get stuck at any point.

                # Tool AI Capabilities
                * The tool AI only has access to the current screenshot and your instructions. It uses your instructions to perform the next action on the page.
                * It can interact with the computer with these actions: click, right click, double click, type, scroll, drag, wait to previous page.

                # IMPORTANT
                * You are allowed upto {self.max_iterations} iterations to complete the task.
                * Once you've verified that the main objective has been achieved, end your response with "DONE" (without quotes).

                # Examples
                ## Example 1
                type https://example.com into the address bar and press Enter
                ## Example 2
                click the blue login button located at the top right corner
                ## Example 3
                scroll down the page
                ## Example 4
                type the username example@email.com into the input field labeled Username
                ## Example 5
                DONE

                # Instructions
                Now describe a single high-level action to take next to progress towards the user's goal in detail.
                Focus on the visual action and provide all necessary context.
                """
            ).strip()
        else:
            raise ValueError(f"Expected environment type: Computer or Browser. Got {environment_type}.")

    def reset(self):
        """Reset the agent state."""
        super().reset()
        self.grounding_agent.reset()  # Reset grounding agent state
