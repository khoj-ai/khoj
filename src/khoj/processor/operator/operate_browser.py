import asyncio
import json
import logging
import os
from typing import Callable, List, Optional

import requests

from khoj.database.adapters import AgentAdapters, ConversationAdapters
from khoj.database.models import Agent, ChatModel, KhojUser
from khoj.processor.operator.operator_actions import *
from khoj.processor.operator.operator_agent_anthropic import AnthropicOperatorAgent
from khoj.processor.operator.operator_agent_base import OperatorAgent
from khoj.processor.operator.operator_agent_binary import BinaryOperatorAgent
from khoj.processor.operator.operator_agent_openai import OpenAIOperatorAgent
from khoj.processor.operator.operator_environment_base import EnvStepResult
from khoj.processor.operator.operator_environment_browser import BrowserEnvironment
from khoj.routers.helpers import ChatEvent
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


# --- Browser Operator Function ---
async def operate_browser(
    query: str,
    user: KhojUser,
    conversation_log: dict,
    location_data: LocationData,
    send_status_func: Optional[Callable] = None,
    query_images: Optional[List[str]] = None,  # TODO: Handle query images
    agent: Agent = None,
    query_files: str = None,  # TODO: Handle query files
    cancellation_event: Optional[asyncio.Event] = None,
    tracer: dict = {},
):
    response, summary_message, user_input_message = None, None, None
    environment: Optional[BrowserEnvironment] = None

    # Get the agent chat model
    agent_chat_model = await AgentAdapters.aget_agent_chat_model(agent, user) if agent else None
    reasoning_model: ChatModel = await ConversationAdapters.aget_default_chat_model(user, agent_chat_model)
    if not reasoning_model or not reasoning_model.vision_enabled:
        reasoning_model = await ConversationAdapters.aget_vision_enabled_config()
    if not reasoning_model:
        raise ValueError(f"No vision enabled chat model found. Configure a vision chat model to operate browser.")

    # Initialize Agent
    max_iterations = int(os.getenv("KHOJ_OPERATOR_ITERATIONS", 40))
    operator_agent: OperatorAgent
    if reasoning_model.name.startswith("gpt-4o"):
        operator_agent = OpenAIOperatorAgent(query, reasoning_model, max_iterations, tracer)
    elif reasoning_model.name.startswith("claude-3-7-sonnet"):
        operator_agent = AnthropicOperatorAgent(query, reasoning_model, max_iterations, tracer)
    else:
        grounding_model_name = "ui-tars-1.5"
        grounding_model = await ConversationAdapters.aget_chat_model_by_name(grounding_model_name)
        if (
            not grounding_model
            or not grounding_model.vision_enabled
            or not grounding_model.model_type == ChatModel.ModelType.OPENAI
        ):
            raise ValueError("No supported visual grounding model for binary operator agent found.")
        operator_agent = BinaryOperatorAgent(query, reasoning_model, grounding_model, max_iterations, tracer)

    # Initialize Environment
    if send_status_func:
        async for event in send_status_func(f"**Launching Browser**"):
            yield {ChatEvent.STATUS: event}
    environment = BrowserEnvironment()
    await environment.start(width=1024, height=768)

    # Start Operator Loop
    try:
        summarize_prompt = f"Use the results of our research to provide a comprehensive, self-contained answer for the target query:\n{query}."
        task_completed = False
        iterations = 0

        with timer(f"Operating browser with {reasoning_model.model_type} {reasoning_model.name}", logger):
            while iterations < max_iterations and not task_completed:
                if cancellation_event and cancellation_event.is_set():
                    logger.debug(f"Browser operator cancelled by client disconnect")
                    break

                iterations += 1

                # 1. Get current environment state
                browser_state = await environment.get_state()

                # 2. Agent decides action(s)
                agent_result = await operator_agent.act(browser_state)

                # 3. Execute actions in the environment
                env_steps: List[EnvStepResult] = []
                for action in agent_result.actions:
                    if cancellation_event and cancellation_event.is_set():
                        logger.debug(f"Browser operator cancelled by client disconnect")
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
                latest_screenshot = f"data:image/webp;base64,{env_steps[-1].screenshot_base64 if env_steps else browser_state.screenshot}"
                render_payload = agent_result.rendered_response
                render_payload["image"] = latest_screenshot
                render_content = f"**Action**: {json.dumps(render_payload)}"
                if send_status_func:
                    async for event in send_status_func(f"**Operating Browser**:\n{render_content}"):
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
                    summary_message = await operator_agent.summarize(summarize_prompt, browser_state)
                    logger.info(f"Task completed: {task_completed}, Iteration limit: {trigger_iteration_limit}")
                    break

                # 4. Update agent on the results of its action on the environment
                operator_agent.add_action_results(env_steps, agent_result)

            # Determine final response message
            if user_input_message:
                response = user_input_message
            elif task_completed:
                response = summary_message
            else:  # Hit iteration limit
                response = f"Operator hit iteration limit ({max_iterations}). If the results seem incomplete try again, assign a smaller task or try a different approach.\nThese were the results till now:\n{summary_message}"
    finally:
        if environment and not user_input_message:  # Don't close browser if user input required
            await environment.close()
        if operator_agent:
            operator_agent.reset()

    yield {
        "query": query,
        "result": user_input_message or response,
        "webpages": [{"link": url, "snippet": ""} for url in environment.visited_urls],
    }
