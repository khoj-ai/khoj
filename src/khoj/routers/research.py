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
        # Try parse the response as function call response to infer next tool to use.
        # TODO: Handle multiple tool calls.
        response_text = response.text
        parsed_response = [ToolCall(**item) for item in load_complex_json(response_text)][0]
    except Exception:
        # Otherwise assume the model has decided to end the research run and respond to the user.
        parsed_response = ToolCall(name=ConversationCommand.Text, args={"response": response_text}, id=None)

    # If we have a valid response, extract the tool and query.
    warning = None
    logger.info(f"Response for determining relevant tools: {parsed_response.name}({parsed_response.args})")

    # Detect selection of previously used query, tool combination.
    previous_tool_query_combinations = {
        (i.query.name, dict_to_tuple(i.query.args))
        for i in previous_iterations
        if i.warning is None and isinstance(i.query, ToolCall)
    }
    if (parsed_response.name, dict_to_tuple(parsed_response.args)) in previous_tool_query_combinations:
        warning = f"Repeated tool, query combination detected. You've already called {parsed_response.name} with args: {parsed_response.args}. Try something different."
    # Only send client status updates if we'll execute this iteration and model has thoughts to share.
    elif send_status_func and not is_none_or_empty(response.thought):
        async for event in send_status_func(response.thought):
            yield {ChatEvent.STATUS: event}

    yield ResearchIteration(query=parsed_response, warning=warning, raw_response=response.raw_content)


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
    query_files: str = None,
    cancellation_event: Optional[asyncio.Event] = None,
    interrupt_queue: Optional[asyncio.Queue] = None,
    abort_message: str = ChatEvent.END_EVENT.value,
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

        online_results: Dict = dict()
        code_results: Dict = dict()
        document_results: List[Dict[str, str]] = []
        operator_results: OperatorRun = None
        mcp_results: List = []
        this_iteration = ResearchIteration(query=query)

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
                this_iteration = result
                yield this_iteration

        # Skip running iteration if warning present in iteration
        if this_iteration.warning:
            logger.warning(f"Research mode: {this_iteration.warning}.")

        # Terminate research if selected text tool or query, tool not set for next iteration
        elif (
            not this_iteration.query
            or isinstance(this_iteration.query, str)
            or this_iteration.query.name == ConversationCommand.Text
        ):
            current_iteration = MAX_ITERATIONS

        elif this_iteration.query.name == ConversationCommand.SemanticSearchFiles:
            this_iteration.context = []
            document_results = []
            previous_inferred_queries = {
                c["query"] for iteration in previous_iterations if iteration.context for c in iteration.context
            }
            async for result in search_documents(
                **this_iteration.query.args,
                n=max_document_searches,
                d=None,
                user=user,
                chat_history=construct_tool_chat_history(previous_iterations, ConversationCommand.SemanticSearchFiles),
                conversation_id=conversation_id,
                conversation_commands=[ConversationCommand.Notes],
                location_data=location,
                send_status_func=send_status_func,
                query_images=query_images,
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
            else:
                this_iteration.warning = "No matching document references found"

        elif this_iteration.query.name == ConversationCommand.SearchWeb:
            previous_subqueries = {
                subquery
                for iteration in previous_iterations
                if iteration.onlineContext
                for subquery in iteration.onlineContext.keys()
            }
            try:
                async for result in search_online(
                    **this_iteration.query.args,
                    conversation_history=construct_tool_chat_history(
                        previous_iterations, ConversationCommand.SearchWeb
                    ),
                    location=location,
                    user=user,
                    send_status_func=send_status_func,
                    custom_filters=[],
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

        elif this_iteration.query.name == ConversationCommand.ReadWebpage:
            try:
                async for result in read_webpages_content(
                    **this_iteration.query.args,
                    user=user,
                    send_status_func=send_status_func,
                    # max_webpages_to_read=max_webpages_to_read,
                    agent=agent,
                    tracer=tracer,
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

        elif this_iteration.query.name == ConversationCommand.PythonCoder:
            try:
                async for result in run_code(
                    **this_iteration.query.args,
                    conversation_history=construct_tool_chat_history(
                        previous_iterations, ConversationCommand.PythonCoder
                    ),
                    context="",
                    location_data=location,
                    user=user,
                    send_status_func=send_status_func,
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
            except (ValueError, TypeError) as e:
                this_iteration.warning = f"Error running code: {e}"
                logger.warning(this_iteration.warning, exc_info=True)

        elif this_iteration.query.name == ConversationCommand.OperateComputer:
            try:
                async for result in operate_environment(
                    **this_iteration.query.args,
                    user=user,
                    conversation_log=construct_tool_chat_history(previous_iterations, ConversationCommand.Operator),
                    location_data=location,
                    previous_trajectory=previous_iterations[-1].operatorContext if previous_iterations else None,
                    send_status_func=send_status_func,
                    query_images=query_images,
                    agent=agent,
                    query_files=query_files,
                    cancellation_event=cancellation_event,
                    interrupt_queue=interrupt_queue,
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

        elif this_iteration.query.name == ConversationCommand.ViewFile:
            try:
                async for result in view_file_content(
                    **this_iteration.query.args,
                    user=user,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        if this_iteration.context is None:
                            this_iteration.context = []
                        document_results: List[Dict[str, str]] = result  # type: ignore
                        this_iteration.context += document_results
                async for result in send_status_func(f"**Viewed file**: {this_iteration.query.args['path']}"):
                    yield result
            except Exception as e:
                this_iteration.warning = f"Error viewing file: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        elif this_iteration.query.name == ConversationCommand.ListFiles:
            try:
                async for result in list_files(
                    **this_iteration.query.args,
                    user=user,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        if this_iteration.context is None:
                            this_iteration.context = []
                        document_results: List[Dict[str, str]] = [result]  # type: ignore
                        this_iteration.context += document_results
                async for result in send_status_func(result["query"]):
                    yield result
            except Exception as e:
                this_iteration.warning = f"Error listing files: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        elif this_iteration.query.name == ConversationCommand.RegexSearchFiles:
            try:
                async for result in grep_files(
                    **this_iteration.query.args,
                    user=user,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        if this_iteration.context is None:
                            this_iteration.context = []
                        document_results: List[Dict[str, str]] = [result]  # type: ignore
                        this_iteration.context += document_results
                async for result in send_status_func(result["query"]):
                    yield result
            except Exception as e:
                this_iteration.warning = f"Error searching with regex: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        elif "/" in this_iteration.query.name:
            try:
                # Identify MCP client to use
                server_name, tool_name = this_iteration.query.name.split("/", 1)
                mcp_client = next((client for client in mcp_clients if client.name == server_name), None)
                if not mcp_client:
                    raise ValueError(f"Could not find MCP server with name {server_name}")

                # Invoke tool on the identified MCP server
                mcp_results = await mcp_client.run_tool(tool_name, this_iteration.query.args)

                # Record tool result in context
                if this_iteration.context is None:
                    this_iteration.context = []
                this_iteration.context += mcp_results
                async for result in send_status_func(f"**Used MCP Tool**: {tool_name} on {mcp_client.name}"):
                    yield result
            except Exception as e:
                this_iteration.warning = f"Error using MCP tool: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        else:
            # No valid tools. This is our exit condition.
            current_iteration = MAX_ITERATIONS

        current_iteration += 1

        if (
            document_results
            or online_results
            or code_results
            or operator_results
            or mcp_results
            or this_iteration.warning
        ):
            results_data = f"\n<iteration_{current_iteration}_results>"
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
            if mcp_results:
                results_data += f"\n<mcp_tool_results>\n{yaml.dump(mcp_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</mcp_tool_results>"
            if this_iteration.warning:
                results_data += f"\n<warning>\n{this_iteration.warning}\n</warning>"
            results_data += f"\n</iteration_{current_iteration}_results>"

            # intermediate_result = await extract_relevant_info(this_iteration.query, results_data, agent)
            this_iteration.summarizedResult = results_data

        this_iteration.summarizedResult = this_iteration.summarizedResult or "Failed to get results."
        previous_iterations.append(this_iteration)
        yield this_iteration

    # Close MCP client connections
    for mcp_client in mcp_clients:
        await mcp_client.close()
