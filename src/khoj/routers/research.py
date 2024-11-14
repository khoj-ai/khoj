import json
import logging
from datetime import datetime
from typing import Callable, Dict, List, Optional

import yaml
from fastapi import Request

from khoj.database.models import Agent, KhojUser
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import (
    InformationCollectionIteration,
    clean_json,
    construct_chat_history,
    construct_iteration_history,
    construct_tool_chat_history,
)
from khoj.processor.tools.online_search import read_webpages, search_online
from khoj.processor.tools.run_code import run_code, truncate_code_context
from khoj.routers.api import extract_references_and_questions
from khoj.routers.helpers import (
    ChatEvent,
    generate_summary_from_files,
    send_message_to_model_wrapper,
)
from khoj.utils.helpers import (
    ConversationCommand,
    function_calling_description_for_llm,
    is_none_or_empty,
    timer,
)
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


async def apick_next_tool(
    query: str,
    conversation_history: dict,
    user: KhojUser = None,
    query_images: List[str] = [],
    location: LocationData = None,
    user_name: str = None,
    agent: Agent = None,
    previous_iterations: List[InformationCollectionIteration] = [],
    max_iterations: int = 5,
    send_status_func: Optional[Callable] = None,
    tracer: dict = {},
    query_files: str = None,
):
    """Given a query, determine which of the available tools the agent should use in order to answer appropriately."""

    # Construct tool options for the agent to choose from
    tool_options = dict()
    tool_options_str = ""
    agent_tools = agent.input_tools if agent else []
    for tool, description in function_calling_description_for_llm.items():
        tool_options[tool.value] = description
        if len(agent_tools) == 0 or tool.value in agent_tools:
            tool_options_str += f'- "{tool.value}": "{description}"\n'

    # Construct chat history with user and iteration history with researcher agent for context
    chat_history = construct_chat_history(conversation_history, agent_name=agent.name if agent else "Khoj")
    previous_iterations_history = construct_iteration_history(previous_iterations, prompts.previous_iteration)

    if query_images:
        query = f"[placeholder for user attached images]\n{query}"

    today = datetime.today()
    location_data = f"{location}" if location else "Unknown"
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    function_planning_prompt = prompts.plan_function_execution.format(
        tools=tool_options_str,
        chat_history=chat_history,
        personality_context=personality_context,
        current_date=today.strftime("%Y-%m-%d"),
        day_of_week=today.strftime("%A"),
        username=user_name or "Unknown",
        location=location_data,
        previous_iterations=previous_iterations_history,
        max_iterations=max_iterations,
    )

    try:
        with timer("Chat actor: Infer information sources to refer", logger):
            response = await send_message_to_model_wrapper(
                query=query,
                context=function_planning_prompt,
                response_type="json_object",
                user=user,
                query_images=query_images,
                query_files=query_files,
                tracer=tracer,
            )
    except Exception as e:
        logger.error(f"Failed to infer information sources to refer: {e}", exc_info=True)
        yield InformationCollectionIteration(
            tool=None,
            query=None,
            warning="Failed to infer information sources to refer. Skipping iteration. Try again.",
        )
        return

    try:
        response = clean_json(response)
        response = json.loads(response)
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
            determined_tool_message += f"{selected_tool}({generated_query})." if selected_tool else "respond."
            determined_tool_message += f"\nReason: {scratchpad}" if scratchpad else ""
            async for event in send_status_func(f"{scratchpad}"):
                yield {ChatEvent.STATUS: event}

        yield InformationCollectionIteration(
            tool=selected_tool,
            query=generated_query,
            warning=warning,
        )
    except Exception as e:
        logger.error(f"Invalid response for determining relevant tools: {response}. {e}", exc_info=True)
        yield InformationCollectionIteration(
            tool=None,
            query=None,
            warning=f"Invalid response for determining relevant tools: {response}. Skipping iteration. Fix error: {e}",
        )


async def execute_information_collection(
    request: Request,
    user: KhojUser,
    query: str,
    conversation_id: str,
    conversation_history: dict,
    query_images: List[str],
    agent: Agent = None,
    send_status_func: Optional[Callable] = None,
    user_name: str = None,
    location: LocationData = None,
    file_filters: List[str] = [],
    tracer: dict = {},
    query_files: str = None,
):
    current_iteration = 0
    MAX_ITERATIONS = 5
    previous_iterations: List[InformationCollectionIteration] = []
    while current_iteration < MAX_ITERATIONS:
        online_results: Dict = dict()
        code_results: Dict = dict()
        document_results: List[Dict[str, str]] = []
        summarize_files: str = ""
        this_iteration = InformationCollectionIteration(tool=None, query=query)

        async for result in apick_next_tool(
            query,
            conversation_history,
            user,
            query_images,
            location,
            user_name,
            agent,
            previous_iterations,
            MAX_ITERATIONS,
            send_status_func,
            tracer=tracer,
            query_files=query_files,
        ):
            if isinstance(result, dict) and ChatEvent.STATUS in result:
                yield result[ChatEvent.STATUS]
            elif isinstance(result, InformationCollectionIteration):
                this_iteration = result

        # Skip running iteration if warning present in iteration
        if this_iteration.warning:
            logger.warning(f"Research mode: {this_iteration.warning}.")

        elif this_iteration.tool == ConversationCommand.Notes:
            this_iteration.context = []
            document_results = []
            previous_inferred_queries = {
                c["query"] for iteration in previous_iterations if iteration.context for c in iteration.context
            }
            async for result in extract_references_and_questions(
                request,
                construct_tool_chat_history(previous_iterations, ConversationCommand.Notes),
                this_iteration.query,
                7,
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
                    construct_tool_chat_history(previous_iterations, ConversationCommand.Webpage),
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

        elif this_iteration.tool == ConversationCommand.Summarize:
            try:
                async for result in generate_summary_from_files(
                    this_iteration.query,
                    user,
                    file_filters,
                    construct_tool_chat_history(previous_iterations),
                    query_images=query_images,
                    agent=agent,
                    send_status_func=send_status_func,
                    query_files=query_files,
                ):
                    if isinstance(result, dict) and ChatEvent.STATUS in result:
                        yield result[ChatEvent.STATUS]
                    else:
                        summarize_files = result  # type: ignore
            except Exception as e:
                this_iteration.warning = f"Error summarizing files: {e}"
                logger.error(this_iteration.warning, exc_info=True)

        else:
            # No valid tools. This is our exit condition.
            current_iteration = MAX_ITERATIONS

        current_iteration += 1

        if document_results or online_results or code_results or summarize_files or this_iteration.warning:
            results_data = f"\n<iteration>{current_iteration}\n<tool>{this_iteration.tool}</tool>\n<query>{this_iteration.query}</query>\n<results>"
            if document_results:
                results_data += f"\n<document_references>\n{yaml.dump(document_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</document_references>"
            if online_results:
                results_data += f"\n<online_results>\n{yaml.dump(online_results, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</online_results>"
            if code_results:
                results_data += f"\n<code_results>\n{yaml.dump(truncate_code_context(code_results), allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</code_results>"
            if summarize_files:
                results_data += f"\n<summarized_files>\n{yaml.dump(summarize_files, allow_unicode=True, sort_keys=False, default_flow_style=False)}\n</summarized_files>"
            if this_iteration.warning:
                results_data += f"\n<warning>\n{this_iteration.warning}\n</warning>"
            results_data += "\n</results>\n</iteration>"

            # intermediate_result = await extract_relevant_info(this_iteration.query, results_data, agent)
            this_iteration.summarizedResult = results_data

        previous_iterations.append(this_iteration)
        yield this_iteration
