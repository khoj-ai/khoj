import json
import logging
from typing import List

from fastapi import APIRouter, Request
from fastapi.requests import Request
from fastapi.responses import Response
from starlette.authentication import requires

from khoj.database.adapters import AgentAdapters
from khoj.database.models import KhojUser
from khoj.routers.helpers import CommonQueryParams, acheck_if_safe_prompt

# Initialize Router
logger = logging.getLogger(__name__)


api_agents = APIRouter()


@api_agents.get("", response_class=Response)
async def all_agents(
    request: Request,
    common: CommonQueryParams,
) -> Response:
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    agents = await AgentAdapters.aget_all_accessible_agents(user)
    agents_packet = list()
    for agent in agents:
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
            }
        )

    # Make sure that the agent named 'khoj' is first in the list. Everything else is sorted by name.
    agents_packet.sort(key=lambda x: x["name"])
    agents_packet.sort(key=lambda x: x["slug"] == "khoj", reverse=True)
    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.post("", response_class=Response)
@requires(["authenticated"])
async def create_agent(
    request: Request,
    common: CommonQueryParams,
    name: str,
    persona: str,
    privacy_level: str,
    icon: str,
    color: str,
    chat_model: str,
    files: List[str],
) -> Response:
    user: KhojUser = request.user.object

    is_safe_prompt, reason = await acheck_if_safe_prompt(persona)
    if not is_safe_prompt:
        return Response(
            content=json.dumps({"error": f"{reason}"}),
            media_type="application/json",
            status_code=400,
        )

    agent = await AgentAdapters.acreate_agent(
        user,
        name,
        persona,
        privacy_level,
        icon,
        color,
        chat_model,
        files,
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
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.patch("", response_class=Response)
@requires(["authenticated"])
async def update_agent(
    request: Request,
    common: CommonQueryParams,
    slug: str,
    name: str,
    persona: str,
    privacy_level: str,
    icon: str,
    color: str,
    chat_model: str,
    files: List[str],
) -> Response:
    user: KhojUser = request.user.object

    is_safe_prompt, reason = await acheck_if_safe_prompt(persona)
    if not is_safe_prompt:
        return Response(
            content=json.dumps({"error": f"{reason}"}),
            media_type="application/json",
            status_code=400,
        )

    selected_agent = await AgentAdapters.aget_agent_by_slug(slug, user)

    agent = await AgentAdapters.aupdate_agent(
        selected_agent,
        slug,
        name,
        persona,
        privacy_level,
        icon,
        color,
        chat_model,
        files,
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
    }

    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)
