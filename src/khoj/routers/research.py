import asyncio
import logging
import os
from copy import deepcopy
from datetime import datetime
from enum import Enum
from typing import Callable, Dict, List, Optional, Type

import yaml
from pydantic import BaseModel, Field

from khoj.database.adapters import AgentAdapters, EntryAdapters
from khoj.database.models import Agent, ChatMessageModel, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import (
    OperatorRun,
    ResearchIteration,
    construct_iteration_history,
    construct_tool_chat_history,
    load_complex_json,
)
from khoj.processor.operator import operate_environment
from khoj.processor.tools.online_search import read_webpages, search_online
from khoj.processor.tools.run_code import run_code
from khoj.routers.helpers import (
    ChatEvent,
    generate_summary_from_files,
    search_documents,
    send_message_to_model_wrapper,
)
from khoj.utils.helpers import (
    ConversationCommand,
    is_none_or_empty,
    is_operator_enabled,
    timer,
    tool_description_for_research_llm,
    truncate_code_context,
)
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


class PlanningResponse(BaseModel):
    """
    Schema for the response from planning agent when deciding the next tool to pick.
    """

    scratchpad: str = Field(..., description="Scratchpad to reason about which tool to use next")

    class Config:
        arbitrary_types_allowed = True

    @classmethod
    def create_model_with_enum(cls: Type["PlanningResponse"], tool_options: dict) -> Type["PlanningResponse"]:
        """
        Factory method that creates a customized PlanningResponse model
        with a properly typed tool field based on available tools.

        The tool field is dynamically generated based on available tools.
        The query field should be filled by the model after the tool field for a more logical reasoning flow.

        Args:
            tool_options: Dictionary mapping tool names to values

        Returns:
            A customized PlanningResponse class
        """
        # Create dynamic enum from tool options
        tool_enum = Enum("ToolEnum", tool_options)  # type: ignore

        # Create and return a customized response model with the enum
        class PlanningResponseWithTool(PlanningResponse):
            """
            Use the scratchpad to reason about which tool to use next and the query to send to the tool.
            Pick tool from provided options and your query to send to the tool.
            """

            tool: tool_enum = Field(..., description="Name of the tool to use")
            query: str = Field(..., description="Detailed query for the selected tool")

        return PlanningResponseWithTool


async def apick_next_tool(
    query: str,
    conversation_history: List[ChatMessageModel],
    user: KhojUser = None,
    location: LocationData = None,
    user_name: str = None,
    agent: Agent = None,
    previous_iterations: List[ResearchIteration] = [],
    max_iterations: int = 5,
    query_images: List[str] = [],
    query_files: str = None,
    max_document_searches: int = 7,
    max_online_searches: int = 3,
    max_webpages_to_read: int = 1,
    send_status_func: Optional[Callable] = None,
    tracer: dict = {},
):
    """Given a query, determine which of the available tools the agent should use in order to answer appropriately."""

    # Continue with previous iteration if a multi-step tool use is in progress
    if (
        previous_iterations
        and previous_iterations[-1].tool == ConversationCommand.Operator
        and not previous_iterations[-1].summarizedResult
    ):
        previous_iteration = previous_iterations[-1]
        yield ResearchIteration(
            tool=previous_iteration.tool,
            query=query,
            context=previous_iteration.context,
            onlineContext=previous_iteration.onlineContext,
            codeContext=previous_iteration.codeContext,
            operatorContext=previous_iteration.operatorContext,
            warning=previous_iteration.warning,
        )
        return

    # Construct tool options for the agent to choose from
    tool_options = dict()
    tool_options_str = ""
    agent_tools = agent.input_tools if agent else []
    user_has_entries = await EntryAdapters.auser_has_entries(user)
    for tool, description in tool_description_for_research_llm.items():
        # Skip showing operator tool as an option if not enabled
        if tool == ConversationCommand.Operator and not is_operator_enabled():
            continue
        # Skip showing Notes tool as an option if user has no entries
        if tool == ConversationCommand.Notes:
            if not user_has_entries:
                continue
            description = description.format(max_search_queries=max_document_searches)
        if tool == ConversationCommand.Webpage:
            description = description.format(max_webpages_to_read=max_webpages_to_read)
        if tool == ConversationCommand.Online:
            description = description.format(max_search_queries=max_online_searches)
        # Add tool if agent does not have any tools defined or the tool is supported by the agent.
        if len(agent_tools) == 0 or tool.value in agent_tools:
            tool_options[tool.name] = tool.value
            tool_options_str += f'- "{tool.value}": "{description}"\n'

    # Create planning reponse model with dynamically populated tool enum class
    planning_response_model = PlanningResponse.create_model_with_enum(tool_options)

    today = datetime.today()
    location_data = f"{location}" if location else "Unknown"
    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    function_planning_prompt = prompts.plan_function_execution.format(
        tools=tool_options_str,
        personality_context=personality_context,
        current_date=today.strftime("%Y-%m-%d"),
        day_of_week=today.strftime("%A"),
        username=user_name or "Unknown",
        location=location_data,
        max_iterations=max_iterations,
    )

    if query_images:
        query = f"[placeholder for user attached images]\n{query}"

    # Construct chat history with user and iteration history with researcher agent for context
    iteration_chat_history = construct_iteration_history(previous_iterations, prompts.previous_iteration, query)
    chat_and_research_history = conversation_history + iteration_chat_history

    # Plan function execution for the next tool
    query = prompts.plan_function_execution_next_tool.format(query=query) if iteration_chat_history else query

    try:
        with timer("Chat actor: Infer information sources to refer", logger):
            response = await send_message_to_model_wrapper(
                query=query,
                system_message=function_planning_prompt,
                chat_history=chat_and_research_history,
                response_type="json_object",
                response_schema=planning_response_model,
                deepthought=True,
                user=user,
                query_images=query_images,
                query_files=query_files,
                agent_chat_model=agent_chat_model,
                tracer=tracer,
            )
    except Exception as e:
        logger.error(f"Failed to infer information sources to refer: {e}", exc_info=True)
        yield ResearchIteration(
            tool=None,
            query=None,
            warning="Failed to infer information sources to refer. Skipping iteration. Try again.",
        )
        return

    try:
        response = load_complex_json(response)
        if not isinstance(response, dict):
            raise ValueError(f"Expected dict response, got {type(response).__name__}: {response}")
        selected_tool = response.get("tool", None)
        generated_query = response.get("query", None)
        scratchpad = response.get("scratchpad", None)
        warning = None
        logger.info(f"Response for determining relevant tools: {response}")

        # Detect selection of previously used query, tool combination.
        previous_tool_query_combinations = {(i.tool, i.query) for i in previous_iterations if i.warning is None}
        if (selected_tool, generated_query) in previous_tool_query_combinations:
            warning = f"Repeated tool, query combination detected. Skipping iteration. Try something different."
        # Only send client status updates if we'll execute this iteration
        elif send_status_func:
            determined_tool_message = "**Determined Tool**: "
            determined_tool_message += (
                f"{selected_tool}({generated_query})." if selected_tool != ConversationCommand.Text else "respond."
            )
            determined_tool_message += f"\nReason: {scratchpad}" if scratchpad else ""
            async for event in send_status_func(f"{scratchpad}"):
                yield {ChatEvent.STATUS: event}

        yield ResearchIteration(
            tool=selected_tool,
            query=generated_query,
            warning=warning,
        )
    except Exception as e:
        logger.error(f"Invalid response for determining relevant tools: {response}. {e}", exc_info=True)
        yield ResearchIteration(
            tool=None,
            query=None,
            warning=f"Invalid response for determining relevant tools: {response}. Skipping iteration. Fix error: {e}",
        )


async def research(
    user: KhojUser,
    query: str,
    conversation_id: str,
    conversation_history: List[ChatMessageModel],
    previous_iterations: List[ResearchIteration],
    query_images: List[str],
    agent: Agent = None,
    send_status_func: Optional[Callable] = None,
    user_name: str = None,
    location: LocationData = None,
    file_filters: List[str] = [],
    tracer: dict = {},
    query_files: str = None,
    cancellation_event: Optional[asyncio.Event] = None,
):
    max_document_searches = 7
    max_online_searches = 3
    max_webpages_to_read = 1
    current_iteration = 0
    MAX_ITERATIONS = int(os.getenv("KHOJ_RESEARCH_ITERATIONS", 5))

    # Incorporate previous partial research into current research chat history
    research_conversation_history = deepcopy(conversation_history)
    if current_iteration := len(previous_iterations) > 0:
        logger.info(f"Continuing research with the previous {len(previous_iterations)} iteration results.")
        previous_iterations_history = construct_iteration_history(previous_iterations, prompts.previous_iteration)
        research_conversation_history += previous_iterations_history

    while current_iteration < MAX_ITERATIONS:
        # Check for cancellation at the start of each iteration
        if cancellation_event and cancellation_event.is_set():
            logger.debug(f"Research cancelled. User {user} disconnected client.")
            break

        online_results: Dict = dict()
        code_results: Dict = dict()
        document_results: List[Dict[str, str]] = []
        operator_results: OperatorRun = None
        this_iteration = ResearchIteration(tool=None, query=query)

        async for result in apick_next_tool(
            query,
            research_conversation_history,
            user,
            location,
            user_name,
            agent,
            previous_iterations,
            MAX_ITERATIONS,
            query_images=query_images,
            query_files=query_files,
            max_document_searches=max_document_searches,
            max_online_searches=max_online_searches,
            max_webpages_to_read=max_webpages_to_read,
            send_status_func=send_status_func,
            tracer=tracer,
        ):
            if isinstance(result, dict) and ChatEvent.STATUS in result:
                yield result[ChatEvent.STATUS]
            elif isinstance(result, ResearchIteration):
                this_iteration = result
                yield this_iteration

        # Skip running iteration if warning present in iteration
        if this_iteration.warning:
            logger.warning(f"Research mode: {this_iteration.warning}.")

        # Terminate research if selected text tool or query, tool not set for next iteration
        elif not this_iteration.query or not this_iteration.tool or this_iteration.tool == ConversationCommand.Text:
            current_iteration = MAX_ITERATIONS

        elif this_iteration.tool == ConversationCommand.Notes:
            this_iteration.context = []
            document_results = []
            previous_inferred_queries = {
                c["query"] for iteration in previous_iterations if iteration.context for c in iteration.context
            }
            async for result in search_documents(
                user,
                construct_tool_chat_history(previous_iterations, ConversationCommand.Notes),
                this_iteration.query,
                max_document_searches,
                None,
                conversation_id,
                [ConversationCommand.Default],
                location,
                send_status_func,
                query_images,
                previous_inferred_queries=previous_inferred_queries,
                agent=agent,
                tracer=tracer,
                query_files=query_files,
            ):
                if isinstance(result, dict) and ChatEvent.STATUS in result:
                    yield result[ChatEvent.STATUS]
                elif isinstance(result, tuple):
                    document_results = result[0]
                    this_iteration.context += document_results

            if not is_none_or_empty(document_results):
                try:
                    distinct_files = {d["file"] for d in document_results}
                    distinct_headings = set([d["compiled"].split("\n")[0] for d in document_results if "compiled" in d])
                    # Strip only leading # from headings
                    headings_str = "\n- " + "\n- ".join(distinct_headings).replace("#", "")
                    async for result in send_status_func(
                        f"**Found {len(distinct_headings)} Notes Across {len(distinct_files)} Files**: {headings_str}"
                    ):
                        yield result
                except Exception as e:
                    this_iteration.warning = f"Error extracting document references: {e}"
                    logger.error(this_iteration.warning, exc_info=True)

        elif this_iteration.tool == ConversationCommand.Online:
            previous_subqueries = {
                subquery
                for iteration in previous_iterations
                if iteration.onlineContext
                for subquery in iteration.onlineContext.keys()
            }
            try:
                async for result in search_online(
                    this_iteration.query,
                    construct_tool_chat_history(previous_iterations, ConversationCommand.Online),
                    location,
                    user,
                    send_status_func,
                    [],
                    max_online_searches=max_online_searches,
                    max_webpages_to_read=0,
                    query_images=query_images,
                    previous_subqueries=previous_subqueries,
                    agent=agent,
                    tracer=tracer,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    elif is_none_or_empty(result):
                        this_iteration.warning = "Detected previously run online search queries. Skipping iteration. Try something different."
                    else:
                        online_results: Dict[str, Dict] = result  # type: ignore
                        this_iteration.onlineContext = online_results
            except Exception as e:
                this_iteration.warning = f"Error searching online: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        elif this_iteration.tool == ConversationCommand.Webpage:
            try:
                async for result in read_webpages(
                    this_iteration.query,
                    construct_tool_chat_history(previous_iterations, ConversationCommand.Webpage),
                    location,
                    user,
                    send_status_func,
                    max_webpages_to_read=max_webpages_to_read,
                    query_images=query_images,
                    agent=agent,
                    tracer=tracer,
                    query_files=query_files,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        direct_web_pages: Dict[str, Dict] = result  # type: ignore

                        webpages = []
                        for web_query in direct_web_pages:
                            if online_results.get(web_query):
                                online_results[web_query]["webpages"] = direct_web_pages[web_query]["webpages"]
                            else:
                                online_results[web_query] = {"webpages": direct_web_pages[web_query]["webpages"]}

                            for webpage in direct_web_pages[web_query]["webpages"]:
                                webpages.append(webpage["link"])
                        this_iteration.onlineContext = online_results
            except Exception as e:
                this_iteration.warning = f"Error reading webpages: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        elif this_iteration.tool == ConversationCommand.Code:
            try:
                async for result in run_code(
                    this_iteration.query,
                    construct_tool_chat_history(previous_iterations, ConversationCommand.Code),
                    "",
                    location,
                    user,
                    send_status_func,
                    query_images=query_images,
                    agent=agent,
                    query_files=query_files,
                    tracer=tracer,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        code_results: Dict[str, Dict] = result  # type: ignore
                        this_iteration.codeContext = code_results
                async for result in send_status_func(f"**Ran code snippets**: {len(this_iteration.codeContext)}"):
                    yield result
            except ValueError as e:
                this_iteration.warning = f"Error running code: {e}"
                logger.warning(this_iteration.warning, exc_info=True)

        elif this_iteration.tool == ConversationCommand.Operator:
            try:
                async for result in operate_environment(
                    this_iteration.query,
                    user,
                    construct_tool_chat_history(previous_iterations, ConversationCommand.Operator),
                    location,
                    previous_iterations[-1].operatorContext if previous_iterations else None,
                    send_status_func=send_status_func,
                    query_images=query_images,
                    agent=agent,
                    query_files=query_files,
                    cancellation_event=cancellation_event,
                    tracer=tracer,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    elif isinstance(result, OperatorRun):
                        operator_results = result
                        this_iteration.operatorContext = operator_results
                        # Add webpages visited while operating browser to references
                        if result.webpages:
                            if not online_results.get(this_iteration.query):
                                online_results[this_iteration.query] = {"webpages": result.webpages}
                            elif not online_results[this_iteration.query].get("webpages"):
                                online_results[this_iteration.query]["webpages"] = result.webpages
                            else:
                                online_results[this_iteration.query]["webpages"] += result.webpages
                            this_iteration.onlineContext = online_results
            except Exception as e:
                this_iteration.warning = f"Error operating browser: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        else:
            # No valid tools. This is our exit condition.
            current_iteration = MAX_ITERATIONS

        current_iteration += 1

        if document_results or online_results or code_results or operator_results or this_iteration.warning:
            results_data = f"\n<iteration>{current_iteration}\n<tool>{this_iteration.tool}</tool>\n<query>{this_iteration.query}</query>\n<results>"
            if document_results:
                results_data += f"\n<document_references>\n{yaml.dump(document_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</document_references>"
            if online_results:
                results_data += f"\n<online_results>\n{yaml.dump(online_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</online_results>"
            if code_results:
                results_data += f"\n<code_results>\n{yaml.dump(truncate_code_context(code_results), allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</code_results>"
            if operator_results:
                results_data += (
                    f"\n<browser_operator_results>\n{operator_results.response}\n</browser_operator_results>"
                )
            if this_iteration.warning:
                results_data += f"\n<warning>\n{this_iteration.warning}\n</warning>"
            results_data += "\n</results>\n</iteration>"

            # intermediate_result = await extract_relevant_info(this_iteration.query, results_data, agent)
            this_iteration.summarizedResult = results_data

        previous_iterations.append(this_iteration)
        yield this_iteration
