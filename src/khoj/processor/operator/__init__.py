import asyncio
import json
import logging
import os
from typing import Callable, List, Optional

from khoj.database.adapters import AgentAdapters, ConversationAdapters
from khoj.database.models import Agent, ChatMessageModel, ChatModel, KhojUser
from khoj.processor.conversation.utils import (
    AgentMessage,
    OperatorRun,
    construct_chat_history_for_operator,
)
from khoj.processor.operator.operator_actions import RequestUserAction
from khoj.processor.operator.operator_agent_anthropic import AnthropicOperatorAgent
from khoj.processor.operator.operator_agent_base import OperatorAgent
from khoj.processor.operator.operator_agent_binary import BinaryOperatorAgent
from khoj.processor.operator.operator_agent_openai import OpenAIOperatorAgent
from khoj.processor.operator.operator_environment_base import (
    Environment,
    EnvironmentType,
    EnvStepResult,
)
from khoj.processor.operator.operator_environment_browser import BrowserEnvironment
from khoj.processor.operator.operator_environment_computer import ComputerEnvironment
from khoj.routers.helpers import ChatEvent, get_message_from_queue
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


# --- Main Operator Entrypoint ---
async def operate_environment(
    query: str,
    user: KhojUser,
    conversation_log: List[ChatMessageModel],
    location_data: LocationData,
    previous_trajectory: Optional[OperatorRun] = None,
    environment_type: EnvironmentType = EnvironmentType.COMPUTER,
    send_status_func: Optional[Callable] = None,
    query_images: Optional[List[str]] = None,  # TODO: Handle query images
    agent: Agent = None,
    query_files: str = None,  # TODO: Handle query files
    cancellation_event: Optional[asyncio.Event] = None,
    interrupt_queue: Optional[asyncio.Queue] = None,
    abort_message: Optional[str] = ChatEvent.END_EVENT.value,
    tracer: dict = {},
):
    response, user_input_message = None, None

    # Only use partial previous trajectories to continue existing task
    if previous_trajectory and previous_trajectory.response:
        previous_trajectory = None

    # Get the agent chat model
    agent_chat_model = await AgentAdapters.aget_agent_chat_model(agent, user) if agent else None
    reasoning_model: ChatModel = await ConversationAdapters.aget_default_chat_model(user, agent_chat_model)
    if not reasoning_model or not reasoning_model.vision_enabled:
        reasoning_model = await ConversationAdapters.aget_vision_enabled_config()
    if not reasoning_model:
        raise ValueError("No vision enabled chat model found. Configure a vision chat model to operate environment.")

    # Create conversation history from conversation log
    chat_history = construct_chat_history_for_operator(conversation_log)

    # Initialize Agent
    max_context = await ConversationAdapters.aget_max_context_size(reasoning_model, user) or 20000
    max_iterations = int(os.getenv("KHOJ_OPERATOR_ITERATIONS", 100))
    operator_agent: OperatorAgent
    if is_operator_model(reasoning_model.name) == ChatModel.ModelType.ANTHROPIC:
        operator_agent = AnthropicOperatorAgent(
            query,
            reasoning_model,
            environment_type,
            max_iterations,
            max_context,
            chat_history,
            previous_trajectory,
            tracer,
        )
    # TODO: Remove once OpenAI Operator Agent is useful
    elif is_operator_model(reasoning_model.name) == ChatModel.ModelType.OPENAI and False:
        operator_agent = OpenAIOperatorAgent(
            query,
            reasoning_model,
            environment_type,
            max_iterations,
            max_context,
            chat_history,
            previous_trajectory,
            tracer,
        )
    # TODO: Remove once Binary Operator Agent is useful
    elif False:
        grounding_model_name = "ui-tars-1.5"
        grounding_model = await ConversationAdapters.aget_chat_model_by_name(grounding_model_name)
        if (
            not grounding_model
            or not grounding_model.vision_enabled
            or not grounding_model.model_type == ChatModel.ModelType.OPENAI
        ):
            raise ValueError("Binary operator agent needs ui-tars-1.5 served over an OpenAI compatible API.")
        operator_agent = BinaryOperatorAgent(
            query,
            reasoning_model,
            grounding_model,
            environment_type,
            max_iterations,
            max_context,
            chat_history,
            previous_trajectory,
            tracer,
        )
    else:
        raise ValueError(
            f"Unsupported operator model: {reasoning_model.name}. "
            "Please use a supported operator model. Only Anthropic models are currently supported."
        )

    # Initialize Environment
    if send_status_func:
        async for event in send_status_func(f"**Launching {environment_type.value}**"):
            yield {ChatEvent.STATUS: event}
    if environment_type == EnvironmentType.BROWSER:
        environment: Environment = BrowserEnvironment()
    else:
        environment = ComputerEnvironment(provider="docker")
    await environment.start(width=1024, height=768)

    # Start Operator Loop
    try:
        task_completed = False
        iterations = 0
        operator_run = OperatorRun(query=query, trajectory=operator_agent.messages, response=response)
        yield operator_run

        with timer(
            f"Operating {environment_type.value} with {reasoning_model.model_type} {reasoning_model.name}", logger
        ):
            while iterations < max_iterations and not task_completed:
                if cancellation_event and cancellation_event.is_set():
                    logger.debug(f"{environment_type.value} operator cancelled by client disconnect")
                    break

                # Add interrupt query to current operator run
                if interrupt_query := get_message_from_queue(interrupt_queue):
                    if interrupt_query == abort_message:
                        cancellation_event.set()
                        logger.debug(f"Operator run cancelled by user {user} via interrupt queue.")
                        break
                    # Add the interrupt query as a new user message to the research conversation history
                    logger.info(f"Continuing operator run with the new instruction: {interrupt_query}")
                    operator_agent.messages.append(AgentMessage(role="user", content=interrupt_query))
                    async for result in send_status_func(f"**Incorporate New Instruction**: {interrupt_query}"):
                        yield result

                iterations += 1

                # 1. Get current environment state
                env_state = await environment.get_state()

                # 2. Agent decides action(s)
                agent_result = await operator_agent.act(env_state)

                # 3. Execute actions in the environment
                env_steps: List[EnvStepResult] = []
                for action in agent_result.actions:
                    if cancellation_event and cancellation_event.is_set():
                        logger.debug(f"{environment_type.value} operator cancelled by client disconnect")
                        break
                    # Handle request for user action and break the loop
                    if isinstance(action, RequestUserAction):
                        user_input_message = action.request
                        if send_status_func:
                            async for event in send_status_func(f"**Requesting User Input**:\n{action.request}"):
                                yield {ChatEvent.STATUS: event}
                        break
                    env_step = await environment.step(action)
                    env_steps.append(env_step)

                # Render status update
                latest_screenshot = (
                    f"data:image/webp;base64,{env_steps[-1].screenshot_base64 if env_steps else env_state.screenshot}"
                )
                render_payload = agent_result.rendered_response
                render_payload["image"] = latest_screenshot
                render_content = f"**Action**: {json.dumps(render_payload)}"
                if send_status_func:
                    async for event in send_status_func(f"**Operating {environment_type.value}**:\n{render_content}"):
                        yield {ChatEvent.STATUS: event}

                # Check if termination conditions are met
                task_completed = not agent_result.actions  # No actions requested by agent
                trigger_iteration_limit = iterations == max_iterations
                if user_input_message:
                    logger.info(f"User input requested: {user_input_message}")
                    break
                if task_completed or trigger_iteration_limit:
                    # Summarize results of operator run on last iteration
                    operator_agent.add_action_results(env_steps, agent_result)
                    summary_message = await operator_agent.summarize(env_state)
                    logger.info(f"Task completed: {task_completed}, Iteration limit: {trigger_iteration_limit}")
                    break

                # 4. Update agent on the results of its action on the environment
                operator_agent.add_action_results(env_steps, agent_result)
                operator_run.trajectory = operator_agent.messages

            # Determine final response message
            if user_input_message:
                operator_run.response = user_input_message
            elif task_completed:
                operator_run.response = summary_message
            elif cancellation_event and cancellation_event.is_set():
                operator_run.response = None
            else:  # Hit iteration limit
                operator_run.response = f"Operator hit iteration limit ({max_iterations}). If the results seem incomplete try again, assign a smaller task or try a different approach.\nThese were the results till now:\n{summary_message}"
    finally:
        if environment and not user_input_message:  # Don't close environment if user input required
            await environment.close()
        if operator_agent:
            operator_agent.reset()

    if environment_type == EnvironmentType.BROWSER and hasattr(environment, "visited_urls"):
        operator_run.webpages = [{"link": url, "snippet": ""} for url in environment.visited_urls]

    yield operator_run


def is_operator_model(model: str) -> ChatModel.ModelType | None:
    """Check if the model is an operator model."""
    operator_models = {
        "gpt-4o": ChatModel.ModelType.OPENAI,
        "claude-3-7-sonnet": ChatModel.ModelType.ANTHROPIC,
        "claude-sonnet-4": ChatModel.ModelType.ANTHROPIC,
        "claude-opus-4": ChatModel.ModelType.ANTHROPIC,
    }
    for operator_model in operator_models:
        if model.startswith(operator_model):
            return operator_models[operator_model]  # type: ignore[return-value]
    return None
