import asyncio
import logging
import os
from copy import deepcopy
from datetime import datetime
from typing import Callable, Dict, List, Optional

import yaml

from khoj.database.adapters import AgentAdapters, EntryAdapters, McpServerAdapters
from khoj.database.models import Agent, ChatMessageModel, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import (
    OperatorRun,
    ResearchIteration,
    ToolCall,
    construct_iteration_history,
    construct_tool_chat_history,
    load_complex_json,
)
from khoj.processor.operator import operate_environment
from khoj.processor.tools.mcp import MCPClient
from khoj.processor.tools.online_search import read_webpages_content, search_online
from khoj.processor.tools.run_code import run_code
from khoj.routers.helpers import (
    ChatEvent,
    get_message_from_queue,
    grep_files,
    list_files,
    search_documents,
    send_message_to_model_wrapper,
    view_file_content,
)
from khoj.utils.helpers import (
    ConversationCommand,
    ToolDefinition,
    dict_to_tuple,
    is_code_sandbox_enabled,
    is_none_or_empty,
    is_operator_enabled,
    is_web_search_enabled,
    timer,
    tools_for_research_llm,
    truncate_code_context,
)
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


class ToolExecutionResult:
    """Result of executing a single tool call"""

    def __init__(self):
        self.status_messages: List = []
        self.document_results: List[Dict[str, str]] = []
        self.online_results: Dict = {}
        self.code_results: Dict = {}
        self.operator_results: OperatorRun = None
        self.mcp_results: List = []
        self.should_terminate: bool = False


async def execute_tool(
    iteration: ResearchIteration,
    user: KhojUser,
    conversation_id: str,
    previous_iterations: List[ResearchIteration],
    location: LocationData,
    query_images: List[str],
    query_files: str,
    max_document_searches: int,
    max_online_searches: int,
    mcp_clients: List[MCPClient],
    cancellation_event: Optional[asyncio.Event],
    interrupt_queue: Optional[asyncio.Queue],
    agent: Agent,
    tracer: dict,
) -> ToolExecutionResult:
    """Execute a single tool call and return results. Designed for parallel execution."""
    result = ToolExecutionResult()

    # Skip if warning present
    if iteration.warning:
        logger.warning(f"Research mode: {iteration.warning}.")
        return result

    # Check for termination conditions
    if not iteration.query or isinstance(iteration.query, str) or iteration.query.name == ConversationCommand.Text:
        result.should_terminate = True
        return result

    # Create a status collector that captures messages for later batch yield
    async def status_collector(message: str):
        """Async generator that collects status messages instead of streaming them."""
        # Just collect the message - we'll process it later in the main loop
        result.status_messages.append(message)
        yield message  # Yield to satisfy async generator protocol expected by tool functions

    try:
        if iteration.query.name == ConversationCommand.SemanticSearchFiles:
            iteration.context = []
            previous_inferred_queries = {
                c["query"] for iter in previous_iterations if iter.context for c in iter.context
            }
            async for res in search_documents(
                **iteration.query.args,
                n=max_document_searches,
                d=None,
                user=user,
                chat_history=construct_tool_chat_history(previous_iterations, ConversationCommand.SemanticSearchFiles),
                conversation_id=conversation_id,
                conversation_commands=[ConversationCommand.Notes],
                location_data=location,
                send_status_func=status_collector,
                query_images=query_images,
                query_files=query_files,
                previous_inferred_queries=previous_inferred_queries,
                agent=agent,
                tracer=tracer,
            ):
                # Status messages are collected by status_collector, skip ChatEvent.STATUS here
                if isinstance(res, tuple):
                    result.document_results = res[0]
                    iteration.context += result.document_results

            if not is_none_or_empty(result.document_results):
                try:
                    distinct_files = {d["file"] for d in result.document_results}
                    distinct_headings = set(
                        [d["compiled"].split("\n")[0] for d in result.document_results if "compiled" in d]
                    )
                    headings_str = "\n- " + "\n- ".join(distinct_headings).replace("#", "")
                    async for _ in status_collector(
                        f"**Found {len(distinct_headings)} Notes Across {len(distinct_files)} Files**: {headings_str}"
                    ):
                        pass
                except Exception as e:
                    iteration.warning = f"Error extracting document references: {e}"
                    logger.error(iteration.warning, exc_info=True)
            else:
                iteration.warning = "No matching document references found"

        elif iteration.query.name == ConversationCommand.SearchWeb:
            previous_subqueries = {
                subquery for iter in previous_iterations if iter.onlineContext for subquery in iter.onlineContext.keys()
            }
            async for res in search_online(
                **iteration.query.args,
                conversation_history=construct_tool_chat_history(previous_iterations, ConversationCommand.SearchWeb),
                location=location,
                user=user,
                send_status_func=status_collector,
                custom_filters=[],
                max_online_searches=max_online_searches,
                max_webpages_to_read=0,
                query_images=query_images,
                previous_subqueries=previous_subqueries,
                agent=agent,
                tracer=tracer,
            ):
                # Status messages are collected by status_collector, skip ChatEvent.STATUS here
                if is_none_or_empty(res):
                    iteration.warning = (
                        "Detected previously run online search queries. Skipping iteration. Try something different."
                    )
                elif not (isinstance(res, dict) and ChatEvent.STATUS in res):
                    result.online_results = res
                    iteration.onlineContext = result.online_results

        elif iteration.query.name == ConversationCommand.ReadWebpage:
            async for res in read_webpages_content(
                **iteration.query.args,
                user=user,
                send_status_func=status_collector,
                agent=agent,
                tracer=tracer,
            ):
                # Status messages are collected by status_collector, skip ChatEvent.STATUS here
                if not (isinstance(res, dict) and ChatEvent.STATUS in res):
                    direct_web_pages: Dict[str, Dict] = res
                    for web_query in direct_web_pages:
                        if result.online_results.get(web_query):
                            result.online_results[web_query]["webpages"] = direct_web_pages[web_query]["webpages"]
                        else:
                            result.online_results[web_query] = {"webpages": direct_web_pages[web_query]["webpages"]}
                    iteration.onlineContext = result.online_results

        elif iteration.query.name == ConversationCommand.PythonCoder:
            async for res in run_code(
                **iteration.query.args,
                conversation_history=construct_tool_chat_history(previous_iterations, ConversationCommand.PythonCoder),
                context="",
                location_data=location,
                user=user,
                send_status_func=status_collector,
                query_images=query_images,
                query_files=query_files,
                agent=agent,
                tracer=tracer,
            ):
                # Status messages are collected by status_collector, skip ChatEvent.STATUS here
                if not (isinstance(res, dict) and ChatEvent.STATUS in res):
                    result.code_results = res
                    iteration.codeContext = result.code_results
            if iteration.codeContext:
                async for _ in status_collector(f"**Ran code snippets**: {len(iteration.codeContext)}"):
                    pass

        elif iteration.query.name == ConversationCommand.ViewFile:
            async for res in view_file_content(
                **iteration.query.args,
                user=user,
            ):
                if res and isinstance(res, list):
                    if iteration.context is None:
                        iteration.context = []
                    result.document_results = res
                    iteration.context += result.document_results
            async for _ in status_collector(f"**Viewed file**: {iteration.query.args['path']}"):
                pass

        elif iteration.query.name == ConversationCommand.ListFiles:
            async for res in list_files(
                **iteration.query.args,
                user=user,
            ):
                # Status messages are collected by status_collector, skip ChatEvent.STATUS here
                if not (isinstance(res, dict) and ChatEvent.STATUS in res):
                    if iteration.context is None:
                        iteration.context = []
                    result.document_results = [res]
                    iteration.context += result.document_results
            if result.document_results:
                async for _ in status_collector(result.document_results[-1].get("query", "Listed files")):
                    pass

        elif iteration.query.name == ConversationCommand.RegexSearchFiles:
            async for res in grep_files(
                **iteration.query.args,
                user=user,
            ):
                # Status messages are collected by status_collector, skip ChatEvent.STATUS here
                if not (isinstance(res, dict) and ChatEvent.STATUS in res):
                    if iteration.context is None:
                        iteration.context = []
                    result.document_results = [res]
                    iteration.context += result.document_results
            if result.document_results:
                async for _ in status_collector(result.document_results[-1].get("query", "Searched files")):
                    pass

        elif "/" in iteration.query.name:
            server_name, tool_name = iteration.query.name.split("/", 1)
            mcp_client = next((client for client in mcp_clients if client.name == server_name), None)
            if not mcp_client:
                raise ValueError(f"Could not find MCP server with name {server_name}")

            result.mcp_results = await mcp_client.run_tool(tool_name, iteration.query.args)
            if iteration.context is None:
                iteration.context = []
            iteration.context += result.mcp_results
            async for _ in status_collector(f"**Used MCP Tool**: {tool_name} on {mcp_client.name}"):
                pass

        else:
            result.should_terminate = True

    except Exception as e:
        tool_name = iteration.query.name if iteration.query else "unknown"
        iteration.warning = f"Error executing {tool_name}: {e}"
        logger.error(iteration.warning, exc_info=True)

    return result


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
    max_webpages_to_read: int = 3,
    mcp_clients: List[MCPClient] = [],
    send_status_func: Optional[Callable] = None,
    tracer: dict = {},
):
    """Given a query, determine which of the available tools the agent should use in order to answer appropriately."""

    # Continue with previous iteration if a multi-step tool use is in progress
    if (
        previous_iterations
        and previous_iterations[-1].query
        and isinstance(previous_iterations[-1].query, ToolCall)
        and previous_iterations[-1].query.name == ConversationCommand.Operator
        and not previous_iterations[-1].summarizedResult
    ):
        previous_iteration = previous_iterations[-1]
        yield ResearchIteration(
            query=ToolCall(name=previous_iteration.query.name, args={"query": query}, id=previous_iteration.query.id),  # type: ignore
            context=previous_iteration.context,
            onlineContext=previous_iteration.onlineContext,
            codeContext=previous_iteration.codeContext,
            operatorContext=previous_iteration.operatorContext,
            warning=previous_iteration.warning,
        )
        return

    # Construct tool options for the agent to choose from
    tools = []
    tool_options_str = ""
    agent_input_tools = agent.input_tools if agent and agent.input_tools else []
    agent_tools = []

    # Map agent user facing tools to research tools to include in agents toolbox
    document_research_tools = [
        ConversationCommand.SemanticSearchFiles,
        ConversationCommand.RegexSearchFiles,
        ConversationCommand.ViewFile,
        ConversationCommand.ListFiles,
    ]
    web_research_tools = [ConversationCommand.SearchWeb, ConversationCommand.ReadWebpage]
    input_tools_to_research_tools = {
        ConversationCommand.Notes.value: [tool.value for tool in document_research_tools],
        ConversationCommand.Webpage.value: [ConversationCommand.ReadWebpage.value],
        ConversationCommand.Online.value: [ConversationCommand.SearchWeb.value],
        ConversationCommand.Code.value: [ConversationCommand.PythonCoder.value],
        ConversationCommand.Operator.value: [ConversationCommand.OperateComputer.value],
    }
    for input_tool, research_tools in input_tools_to_research_tools.items():
        if input_tool in agent_input_tools:
            agent_tools += research_tools

    user_has_entries = await EntryAdapters.auser_has_entries(user)
    for tool, tool_data in tools_for_research_llm.items():
        # Skip showing operator tool as an option if not enabled
        if tool == ConversationCommand.OperateComputer and not is_operator_enabled():
            continue
        # Skip showing document related tools if user has no documents
        if tool in document_research_tools and not user_has_entries:
            continue
        # Skip showing web search tool if agent has no access to internet
        if tool in web_research_tools and not is_web_search_enabled():
            continue
        # Skip showing code tool if agent has no access to code execution sandbox
        if tool == ConversationCommand.PythonCoder and not is_code_sandbox_enabled():
            continue
        # Format description with relevant usage limits
        if tool == ConversationCommand.SemanticSearchFiles:
            description = tool_data.description.format(max_search_queries=max_document_searches)
        elif tool == ConversationCommand.ReadWebpage:
            description = tool_data.description.format(max_webpages_to_read=max_webpages_to_read)
        elif tool == ConversationCommand.SearchWeb:
            description = tool_data.description.format(max_search_queries=max_online_searches)
        else:
            description = tool_data.description
        # Add tool if agent does not have any tools defined or the tool is supported by the agent.
        if len(agent_tools) == 0 or tool.value in agent_tools:
            tool_options_str += f'- "{tool.value}": "{description}"\n'
            tools.append(
                ToolDefinition(
                    name=tool.value,
                    description=description,
                    schema=tool_data.schema,
                )
            )

    # Get MCP tools
    for mcp_client in mcp_clients:
        try:
            mcp_tools = await mcp_client.get_tools()
            for mcp_tool in mcp_tools:
                qualified_tool_name = f"{mcp_client.name}/{mcp_tool.name}"
                tool_options_str += f'- "{qualified_tool_name}": "{mcp_tool.description}"\n'
                tools.append(
                    ToolDefinition(
                        name=qualified_tool_name,
                        description=mcp_tool.description,
                        schema=mcp_tool.inputSchema,
                    )
                )
        except Exception as e:
            logger.warning(f'Failed to get tools from MCP server "{mcp_client.name}", so skipping: {e}.')

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

    # Construct chat history with user and iteration history with researcher agent for context
    iteration_chat_history = construct_iteration_history(previous_iterations, query, query_images, query_files)
    chat_and_research_history = conversation_history + iteration_chat_history

    try:
        with timer("Chat actor: Infer information sources to refer", logger):
            response = await send_message_to_model_wrapper(
                query="",
                query_files=query_files,
                query_images=query_images,
                system_message=function_planning_prompt,
                chat_history=chat_and_research_history,
                tools=tools,
                deepthought=True,
                fast_model=False,
                agent_chat_model=agent_chat_model,
                user=user,
                tracer=tracer,
            )
    except Exception as e:
        logger.error(f"Failed to infer information sources to refer: {e}", exc_info=True)
        yield ResearchIteration(
            query=None,
            warning="Failed to infer information sources to refer. Skipping iteration. Try again.",
        )
        return

    try:
        # Try parse the response as function call response to infer next tools to use.
        response_text = response.text
        parsed_responses = [ToolCall(**item) for item in load_complex_json(response_text)]
    except Exception:
        # Otherwise assume the model has decided to end the research run and respond to the user.
        parsed_responses = [ToolCall(name=ConversationCommand.Text, args={"response": response_text}, id=None)]

    # Detect selection of previously used query, tool combinations.
    previous_tool_query_combinations = {
        (i.query.name, dict_to_tuple(i.query.args))
        for i in previous_iterations
        if i.warning is None and isinstance(i.query, ToolCall)
    }

    # Send status update with model's thoughts if available
    if send_status_func and not is_none_or_empty(response.thought):
        async for event in send_status_func(response.thought):
            yield {ChatEvent.STATUS: event}

    # Yield a ResearchIteration for each tool call to enable parallel execution
    for idx, parsed_response in enumerate(parsed_responses):
        warning = None
        logger.info(
            f"Response for determining relevant tools ({idx + 1}/{len(parsed_responses)}): {parsed_response.name}({parsed_response.args})"
        )

        if (parsed_response.name, dict_to_tuple(parsed_response.args)) in previous_tool_query_combinations:
            warning = f"Repeated tool, query combination detected. You've already called {parsed_response.name} with args: {parsed_response.args}. Try something different."

        # Include raw_response only for the first tool call to avoid duplication in history
        yield ResearchIteration(
            query=parsed_response,
            warning=warning,
            raw_response=response.raw_content if idx == 0 else None,
        )


async def research(
    user: KhojUser,
    query: str,
    conversation_id: str,
    conversation_history: List[ChatMessageModel],
    previous_iterations: List[ResearchIteration],
    query_images: List[str],
    query_files: str = None,
    user_name: str = None,
    location: LocationData = None,
    send_status_func: Optional[Callable] = None,
    cancellation_event: Optional[asyncio.Event] = None,
    interrupt_queue: Optional[asyncio.Queue] = None,
    abort_message: str = ChatEvent.END_EVENT.value,
    agent: Agent = None,
    tracer: dict = {},
):
    max_document_searches = 7
    max_online_searches = 3
    max_webpages_to_read = 1
    current_iteration = 0
    MAX_ITERATIONS = int(os.getenv("KHOJ_RESEARCH_ITERATIONS", 5))

    # Construct MCP clients
    mcp_servers = await McpServerAdapters.aget_all_mcp_servers()
    mcp_clients = [MCPClient(server.name, server.path, server.api_key) for server in mcp_servers]

    # Incorporate previous partial research into current research chat history
    research_conversation_history = [chat for chat in deepcopy(conversation_history) if chat.message]
    if current_iteration := len(previous_iterations) > 0:
        logger.info(f"Continuing research with the previous {len(previous_iterations)} iteration results.")
        previous_iterations_history = construct_iteration_history(previous_iterations)
        research_conversation_history += previous_iterations_history

    while current_iteration < MAX_ITERATIONS:
        # Check for cancellation at the start of each iteration
        if cancellation_event and cancellation_event.is_set():
            logger.debug(f"Research cancelled. User {user} disconnected client.")
            break

        # Update the query for the current research iteration
        if interrupt_query := get_message_from_queue(interrupt_queue):
            if interrupt_query == abort_message:
                cancellation_event.set()
                logger.debug(f"Research cancelled by user {user} via interrupt queue.")
                break
            # Add the interrupt query as a new user message to the research conversation history
            logger.info(
                f"Continuing research for user {user} with the previous {len(previous_iterations)} iterations and new instruction: {interrupt_query}"
            )
            previous_iterations_history = construct_iteration_history(
                previous_iterations, query, query_images, query_files
            )
            research_conversation_history += previous_iterations_history
            query = interrupt_query
            previous_iterations = []

            async for result in send_status_func(f"**Incorporate New Instruction**: {interrupt_query}"):
                yield result

        # Collect all tool calls from apick_next_tool
        iterations_to_process: List[ResearchIteration] = []
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
            mcp_clients=mcp_clients,
            send_status_func=send_status_func,
            tracer=tracer,
        ):
            if isinstance(result, dict) and ChatEvent.STATUS in result:
                yield result[ChatEvent.STATUS]
            elif isinstance(result, ResearchIteration):
                iterations_to_process.append(result)
                yield result

        # Multi-turn tools that stream their execution
        streaming_tools = {ConversationCommand.OperateComputer}
        if iterations_to_process:
            # Separate streaming tools that need real-time status updates
            # from parallelizable tools that can batch their status messages
            streaming_iterations: list[ResearchIteration] = []
            parallel_iterations: list[ResearchIteration] = []
            for iteration in iterations_to_process:
                if isinstance(iteration.query, ToolCall) and iteration.query.name in streaming_tools:
                    streaming_iterations.append(iteration)
                else:
                    parallel_iterations.append(iteration)

            # Execute streaming tools sequentially for real-time status updates
            streaming_results: list[tuple[ResearchIteration, ToolExecutionResult]] = []
            for iteration in streaming_iterations:
                result = ToolExecutionResult()
                if (
                    isinstance(iteration.query, ToolCall)
                    and iteration.query.name == ConversationCommand.OperateComputer
                ):
                    try:
                        # Execute OperateComputer
                        async for res in operate_environment(
                            **iteration.query.args,
                            user=user,
                            conversation_log=construct_tool_chat_history(
                                previous_iterations, ConversationCommand.Operator
                            ),
                            location_data=location,
                            previous_trajectory=previous_iterations[-1].operatorContext
                            if previous_iterations
                            else None,
                            send_status_func=send_status_func,
                            query_images=query_images,
                            agent=agent,
                            query_files=query_files,
                            cancellation_event=cancellation_event,
                            interrupt_queue=interrupt_queue,
                            tracer=tracer,
                        ):
                            if isinstance(res, dict) and ChatEvent.STATUS in res:
                                yield res[ChatEvent.STATUS]
                            elif isinstance(res, OperatorRun):
                                result.operator_results = res
                                iteration.operatorContext = result.operator_results
                                if res.webpages:
                                    if not result.online_results.get(iteration.query):
                                        result.online_results[iteration.query] = {"webpages": res.webpages}
                                    elif not result.online_results[iteration.query].get("webpages"):
                                        result.online_results[iteration.query]["webpages"] = res.webpages
                                    else:
                                        result.online_results[iteration.query]["webpages"] += res.webpages
                                    iteration.onlineContext = result.online_results
                    except Exception as e:
                        iteration.warning = f"Error operating browser: {e}"
                        logger.error(iteration.warning, exc_info=True)
                streaming_results.append((iteration, result))

            # Execute parallelizable tools in parallel
            parallel_results = []
            if parallel_iterations:
                tasks = [
                    execute_tool(
                        iteration=iteration,
                        user=user,
                        conversation_id=conversation_id,
                        previous_iterations=previous_iterations,
                        location=location,
                        query_images=query_images,
                        query_files=query_files,
                        max_document_searches=max_document_searches,
                        max_online_searches=max_online_searches,
                        mcp_clients=mcp_clients,
                        cancellation_event=cancellation_event,
                        interrupt_queue=interrupt_queue,
                        agent=agent,
                        tracer=tracer,
                    )
                    for iteration in parallel_iterations
                ]
                tool_results = await asyncio.gather(*tasks, return_exceptions=True)
                parallel_results = list(zip(parallel_iterations, tool_results))

            # Combine results (streaming first, then parallel)
            all_results = streaming_results + parallel_results

            # Process results and yield status messages
            for this_iteration, tool_result in all_results:
                # Handle exceptions from asyncio.gather
                if isinstance(tool_result, Exception):
                    this_iteration.warning = f"Error executing tool: {tool_result}"
                    logger.error(this_iteration.warning, exc_info=True)
                    tool_result = ToolExecutionResult()

                # Check for termination
                if tool_result.should_terminate:
                    current_iteration = MAX_ITERATIONS

                # Yield all collected status messages through the real send_status_func
                for status_msg in tool_result.status_messages:
                    if send_status_func:
                        async for status_event in send_status_func(status_msg):
                            yield status_event

                current_iteration += 1

                # Build summarized results
                if (
                    tool_result.document_results
                    or tool_result.online_results
                    or tool_result.code_results
                    or tool_result.operator_results
                    or tool_result.mcp_results
                    or this_iteration.warning
                ):
                    results_data = f"\n<iteration_{current_iteration}_results>"
                    if tool_result.document_results:
                        results_data += f"\n<document_references>\n{yaml.dump(tool_result.document_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</document_references>"
                    if tool_result.online_results:
                        results_data += f"\n<online_results>\n{yaml.dump(tool_result.online_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</online_results>"
                    if tool_result.code_results:
                        results_data += f"\n<code_results>\n{yaml.dump(truncate_code_context(tool_result.code_results), allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</code_results>"
                    if tool_result.operator_results:
                        results_data += f"\n<browser_operator_results>\n{tool_result.operator_results.response}\n</browser_operator_results>"
                    if tool_result.mcp_results:
                        results_data += f"\n<mcp_tool_results>\n{yaml.dump(tool_result.mcp_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</mcp_tool_results>"
                    if this_iteration.warning:
                        results_data += f"\n<warning>\n{this_iteration.warning}\n</warning>"
                    results_data += f"\n</iteration_{current_iteration}_results>"

                    this_iteration.summarizedResult = results_data

                this_iteration.summarizedResult = (
                    this_iteration.summarizedResult
                    or f"<iteration_{current_iteration}_results>Failed to get results.</iteration_{current_iteration}_results>"
                )
                previous_iterations.append(this_iteration)
                yield this_iteration

    # Close MCP client connections
    for mcp_client in mcp_clients:
        await mcp_client.close()
