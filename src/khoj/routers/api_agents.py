import json
import logging
import random
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Optional

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Request
from fastapi.requests import Request
from fastapi.responses import Response
from pydantic import BaseModel
from starlette.authentication import has_required_scope, requires

from khoj.database.adapters import AgentAdapters, ConversationAdapters, EntryAdapters
from khoj.database.models import Agent, Conversation, KhojUser
from khoj.routers.helpers import CommonQueryParams, acheck_if_safe_prompt
from khoj.utils.helpers import (
    ConversationCommand,
    command_descriptions_for_agent,
    mode_descriptions_for_agent,
)

# Initialize Router
logger = logging.getLogger(__name__)


api_agents = APIRouter()


class ModifyAgentBody(BaseModel):
    name: str
    persona: str
    privacy_level: str
    icon: str
    color: str
    chat_model: str
    files: Optional[List[str]] = []
    input_tools: Optional[List[str]] = []
    output_modes: Optional[List[str]] = []
    slug: Optional[str] = None
    is_hidden: Optional[bool] = False


class ModifyHiddenAgentBody(BaseModel):
    slug: Optional[str] = None
    persona: Optional[str] = None
    chat_model: Optional[str] = None
    input_tools: Optional[List[str]] = []
    output_modes: Optional[List[str]] = []


@api_agents.get("", response_class=Response)
async def all_agents(
    request: Request,
    common: CommonQueryParams,
) -> Response:
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    agents = await AgentAdapters.aget_all_accessible_agents(user)
    default_agent = await AgentAdapters.aget_default_agent()
    default_agent_packet = None
    agents_packet = list()
    for agent in agents:
        files = agent.fileobject_set.all()
        file_names = [file.file_name for file in files]
        agent_packet = {
            "slug": agent.slug,
            "name": agent.name,
            "persona": agent.personality,
            "creator": agent.creator.username if agent.creator else None,
            "managed_by_admin": agent.managed_by_admin,
            "color": agent.style_color,
            "icon": agent.style_icon,
            "privacy_level": agent.privacy_level,
            "chat_model": agent.chat_model.name,
            "files": file_names,
            "input_tools": agent.input_tools,
            "output_modes": agent.output_modes,
        }
        if agent.slug == default_agent.slug:
            default_agent_packet = agent_packet
        else:
            agents_packet.append(agent_packet)

    # Load recent conversation sessions
    min_date = datetime.min.replace(tzinfo=timezone.utc)
    two_weeks_ago = datetime.today() - timedelta(weeks=2)
    conversations = []
    if user:
        conversations = await sync_to_async(list[Conversation])(
            ConversationAdapters.get_conversation_sessions(user, request.user.client_app)
            .filter(updated_at__gte=two_weeks_ago)
            .order_by("-updated_at")[:50]
        )
    conversation_times = {conv.agent.slug: conv.updated_at for conv in conversations if conv.agent}

    # Put default agent first, then sort by mru and finally shuffle unused randomly
    random.shuffle(agents_packet)
    agents_packet.sort(key=lambda x: conversation_times.get(x["slug"]) or min_date, reverse=True)
    if default_agent_packet:
        agents_packet.insert(0, default_agent_packet)

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.get("/conversation", response_class=Response)
@requires(["authenticated"])
async def get_agent_by_conversation(
    request: Request,
    common: CommonQueryParams,
    conversation_id: str,
) -> Response:
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    is_subscribed = has_required_scope(request, ["premium"])
    conversation = await ConversationAdapters.aget_conversation_by_user(user=user, conversation_id=conversation_id)

    if not conversation:
        return Response(
            content=json.dumps({"error": f"Conversation with id {conversation_id} not found for user {user}."}),
            media_type="application/json",
            status_code=404,
        )
    if not conversation.agent:
        agent = await AgentAdapters.aget_default_agent()

    agent = await AgentAdapters.aget_agent_by_slug(conversation.agent.slug, user)

    has_files = agent.fileobject_set.exists()

    agents_packet = {
        "slug": agent.slug,
        "name": agent.name,
        "persona": agent.personality,
        "creator": agent.creator.username if agent.creator else None,
        "managed_by_admin": agent.managed_by_admin,
        "color": agent.style_color,
        "icon": agent.style_icon,
        "privacy_level": agent.privacy_level,
        "chat_model": agent.chat_model.name if is_subscribed else None,
        "has_files": has_files,
        "input_tools": agent.input_tools,
        "output_modes": agent.output_modes,
        "is_creator": agent.creator == user,
        "is_hidden": agent.is_hidden,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.get("/options", response_class=Response)
async def get_agent_configuration_options(
    request: Request,
    common: CommonQueryParams,
) -> Response:
    agent_input_tools = [key for key, _ in Agent.InputToolOptions.choices]
    agent_output_modes = [key for key, _ in Agent.OutputModeOptions.choices]

    agent_input_tool_with_descriptions: Dict[str, str] = {}
    for key in agent_input_tools:
        conversation_command = ConversationCommand(key)
        agent_input_tool_with_descriptions[key] = command_descriptions_for_agent[conversation_command]

    agent_output_modes_with_descriptions: Dict[str, str] = {}
    for key in agent_output_modes:
        conversation_command = ConversationCommand(key)
        agent_output_modes_with_descriptions[key] = mode_descriptions_for_agent[conversation_command]

    return Response(
        content=json.dumps(
            {
                "input_tools": agent_input_tool_with_descriptions,
                "output_modes": agent_output_modes_with_descriptions,
            }
        ),
        media_type="application/json",
        status_code=200,
    )


@api_agents.get("/{agent_slug}", response_class=Response)
async def get_agent(
    request: Request,
    common: CommonQueryParams,
    agent_slug: str,
) -> Response:
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    agent = await AgentAdapters.aget_readonly_agent_by_slug(agent_slug, user)

    if not agent:
        return Response(
            content=json.dumps({"error": f"Agent with name {agent_slug} not found."}),
            media_type="application/json",
            status_code=404,
        )

    files = agent.fileobject_set.all()
    file_names = [file.file_name for file in files]
    agents_packet = {
        "slug": agent.slug,
        "name": agent.name,
        "persona": agent.personality,
        "creator": agent.creator.username if agent.creator else None,
        "managed_by_admin": agent.managed_by_admin,
        "color": agent.style_color,
        "icon": agent.style_icon,
        "privacy_level": agent.privacy_level,
        "chat_model": agent.chat_model.name,
        "files": file_names,
        "input_tools": agent.input_tools,
        "output_modes": agent.output_modes,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.delete("/{agent_slug}", response_class=Response)
@requires(["authenticated"])
async def delete_agent(
    request: Request,
    common: CommonQueryParams,
    agent_slug: str,
) -> Response:
    user: KhojUser = request.user.object

    agent = await AgentAdapters.aget_agent_by_slug(agent_slug, user)

    if not agent:
        return Response(
            content=json.dumps({"error": f"Agent with name {agent_slug} not found."}),
            media_type="application/json",
            status_code=404,
        )

    await AgentAdapters.adelete_agent_by_slug(agent_slug, user)

    return Response(content=json.dumps({"message": "Agent deleted."}), media_type="application/json", status_code=200)


@api_agents.patch("/hidden", response_class=Response)
@requires(["authenticated"])
async def update_hidden_agent(
    request: Request,
    common: CommonQueryParams,
    body: ModifyHiddenAgentBody,
) -> Response:
    user: KhojUser = request.user.object

    subscribed = has_required_scope(request, ["premium"])
    chat_model = body.chat_model if subscribed else None

    selected_agent = await AgentAdapters.aget_agent_by_slug(body.slug, user)

    if not selected_agent:
        return Response(
            content=json.dumps({"error": f"Agent with name {body.slug} not found."}),
            media_type="application/json",
            status_code=404,
        )

    agent = await AgentAdapters.aupdate_hidden_agent(
        user=user,
        slug=body.slug,
        persona=body.persona,
        chat_model=chat_model,
        input_tools=body.input_tools,
        output_modes=body.output_modes,
        existing_agent=selected_agent,
    )

    agents_packet = {
        "slug": agent.slug,
        "name": agent.name,
        "persona": agent.personality,
        "creator": agent.creator.username if agent.creator else None,
        "chat_model": agent.chat_model.name,
        "input_tools": agent.input_tools,
        "output_modes": agent.output_modes,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.post("/hidden", response_class=Response)
@requires(["authenticated"])
async def create_hidden_agent(
    request: Request,
    common: CommonQueryParams,
    conversation_id: str,
    body: ModifyHiddenAgentBody,
) -> Response:
    user: KhojUser = request.user.object

    subscribed = has_required_scope(request, ["premium"])
    chat_model = body.chat_model if subscribed else None

    conversation = await ConversationAdapters.aget_conversation_by_user(user=user, conversation_id=conversation_id)
    if not conversation:
        return Response(
            content=json.dumps({"error": f"Conversation with id {conversation_id} not found for user {user}."}),
            media_type="application/json",
            status_code=404,
        )

    if conversation.agent:
        # If the conversation is not already associated with an agent (i.e., it's using the default agent ), we can create a new one
        if conversation.agent.slug != AgentAdapters.DEFAULT_AGENT_SLUG:
            return Response(
                content=json.dumps(
                    {"error": f"Conversation with id {conversation_id} already has an agent. Use the PATCH method."}
                ),
                media_type="application/json",
                status_code=400,
            )

    agent = await AgentAdapters.aupdate_hidden_agent(
        user=user,
        slug=body.slug,
        persona=body.persona,
        chat_model=chat_model,
        input_tools=body.input_tools,
        output_modes=body.output_modes,
        existing_agent=None,
    )

    conversation.agent = agent
    await conversation.asave()

    agents_packet = {
        "slug": agent.slug,
        "name": agent.name,
        "persona": agent.personality,
        "creator": agent.creator.username if agent.creator else None,
        "chat_model": agent.chat_model.name,
        "input_tools": agent.input_tools,
        "output_modes": agent.output_modes,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.post("", response_class=Response)
@requires(["authenticated"])
async def create_agent(
    request: Request,
    common: CommonQueryParams,
    body: ModifyAgentBody,
) -> Response:
    user: KhojUser = request.user.object

    is_safe_prompt, reason = await acheck_if_safe_prompt(
        body.persona, user, lax=body.privacy_level == Agent.PrivacyLevel.PRIVATE
    )

    if not is_safe_prompt:
        return Response(
            content=json.dumps({"error": f"{reason}"}),
            media_type="application/json",
            status_code=400,
        )

    subscribed = has_required_scope(request, ["premium"])
    chat_model = body.chat_model if subscribed else None

    agent = await AgentAdapters.aupdate_agent(
        user,
        body.name,
        body.persona,
        body.privacy_level,
        body.icon,
        body.color,
        chat_model,
        body.files,
        body.input_tools,
        body.output_modes,
        body.slug,
        body.is_hidden,
    )

    agents_packet = {
        "slug": agent.slug,
        "name": agent.name,
        "persona": agent.personality,
        "creator": agent.creator.username if agent.creator else None,
        "managed_by_admin": agent.managed_by_admin,
        "color": agent.style_color,
        "icon": agent.style_icon,
        "privacy_level": agent.privacy_level,
        "chat_model": agent.chat_model.name,
        "files": body.files,
        "input_tools": agent.input_tools,
        "output_modes": agent.output_modes,
        "is_hidden": agent.is_hidden,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.patch("", response_class=Response)
@requires(["authenticated"])
async def update_agent(
    request: Request,
    common: CommonQueryParams,
    body: ModifyAgentBody,
) -> Response:
    user: KhojUser = request.user.object

    is_safe_prompt, reason = await acheck_if_safe_prompt(
        body.persona, user, lax=body.privacy_level == Agent.PrivacyLevel.PRIVATE
    )

    if not is_safe_prompt:
        return Response(
            content=json.dumps({"error": f"{reason}"}),
            media_type="application/json",
            status_code=400,
        )

    selected_agent = await AgentAdapters.aget_agent_by_slug(body.slug, user)

    if not selected_agent:
        return Response(
            content=json.dumps({"error": f"Agent with name {body.name} not found."}),
            media_type="application/json",
            status_code=404,
        )

    subscribed = has_required_scope(request, ["premium"])
    chat_model = body.chat_model if subscribed else None

    agent = await AgentAdapters.aupdate_agent(
        user,
        body.name,
        body.persona,
        body.privacy_level,
        body.icon,
        body.color,
        chat_model,
        body.files,
        body.input_tools,
        body.output_modes,
        body.slug,
    )

    agents_packet = {
        "slug": agent.slug,
        "name": agent.name,
        "persona": agent.personality,
        "creator": agent.creator.username if agent.creator else None,
        "managed_by_admin": agent.managed_by_admin,
        "color": agent.style_color,
        "icon": agent.style_icon,
        "privacy_level": agent.privacy_level,
        "chat_model": agent.chat_model.name,
        "files": body.files,
        "input_tools": agent.input_tools,
        "output_modes": agent.output_modes,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)
