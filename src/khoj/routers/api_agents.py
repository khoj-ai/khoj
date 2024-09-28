import json
import logging
from typing import List, Optional

from asgiref.sync import sync_to_async
from fastapi import APIRouter, Request
from fastapi.requests import Request
from fastapi.responses import Response
from pydantic import BaseModel
from starlette.authentication import requires

from khoj.database.adapters import AgentAdapters
from khoj.database.models import KhojUser
from khoj.routers.helpers import CommonQueryParams, acheck_if_safe_prompt

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


@api_agents.get("", response_class=Response)
async def all_agents(
    request: Request,
    common: CommonQueryParams,
) -> Response:
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    agents = await AgentAdapters.aget_all_accessible_agents(user)
    agents_packet = list()
    for agent in agents:
        files = agent.fileobject_set.all()
        file_names = [file.file_name for file in files]
        agents_packet.append(
            {
                "slug": agent.slug,
                "name": agent.name,
                "persona": agent.personality,
                "creator": agent.creator.username if agent.creator else None,
                "managed_by_admin": agent.managed_by_admin,
                "color": agent.style_color,
                "icon": agent.style_icon,
                "privacy_level": agent.privacy_level,
                "chat_model": agent.chat_model.chat_model,
                "files": file_names,
            }
        )

    # Make sure that the agent named 'khoj' is first in the list. Everything else is sorted by name.
    agents_packet.sort(key=lambda x: x["name"])
    agents_packet.sort(key=lambda x: x["slug"] == "khoj", reverse=True)
    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


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
        "chat_model": agent.chat_model.chat_model,
        "files": file_names,
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

    is_safe_prompt, reason = await acheck_if_safe_prompt(body.persona)
    if not is_safe_prompt:
        return Response(
            content=json.dumps({"error": f"{reason}"}),
            media_type="application/json",
            status_code=400,
        )

    agent = await AgentAdapters.aupdate_agent(
        user,
        body.name,
        body.persona,
        body.privacy_level,
        body.icon,
        body.color,
        body.chat_model,
        body.files,
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
        "chat_model": agent.chat_model.chat_model,
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

    is_safe_prompt, reason = await acheck_if_safe_prompt(body.persona)
    if not is_safe_prompt:
        return Response(
            content=json.dumps({"error": f"{reason}"}),
            media_type="application/json",
            status_code=400,
        )

    selected_agent = await AgentAdapters.aget_agent_by_name(body.name, user)

    if not selected_agent:
        return Response(
            content=json.dumps({"error": f"Agent with name {body.name} not found."}),
            media_type="application/json",
            status_code=404,
        )

    agent = await AgentAdapters.aupdate_agent(
        user,
        body.name,
        body.persona,
        body.privacy_level,
        body.icon,
        body.color,
        body.chat_model,
        body.files,
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
        "chat_model": agent.chat_model.chat_model,
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)
