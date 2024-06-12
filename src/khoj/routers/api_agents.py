import json
import logging

from fastapi import APIRouter, Request
from fastapi.requests import Request
from fastapi.responses import Response

from khoj.database.adapters import AgentAdapters
from khoj.database.models import KhojUser
from khoj.routers.helpers import CommonQueryParams

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
                "avatar": agent.avatar,
                "name": agent.name,
                "personality": agent.personality,
                "public": agent.public,
                "creator": agent.creator.username if agent.creator else None,
                "managed_by_admin": agent.managed_by_admin,
            }
        )

    # Make sure that the agent named 'khoj' is first in the list. Everything else is sorted by name.
    agents_packet.sort(key=lambda x: x["name"])
    agents_packet.sort(key=lambda x: x["slug"] == "khoj", reverse=True)
    return Response(content=json.dumps(agents_packet), media_type="application/json", status_code=200)


@api_agents.get("/{slug}", response_class=Response)
async def get_agent(
    request: Request,
    slug: str,
    common: CommonQueryParams,
) -> Response:
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    agent = await AgentAdapters.get_agent_by_slug(slug, user)
    if agent is None:
        return Response(
            content=json.dumps({"error": "Agent not found"}), media_type="application/json", status_code=404
        )

    agent_packet = {
        "slug": agent.slug,
        "avatar": agent.avatar,
        "name": agent.name,
        "personality": agent.personality,
        "public": agent.public,
        "creator": agent.creator.username if agent.creator else None,
        "managed_by_admin": agent.managed_by_admin,
    }
    return Response(content=json.dumps(agent_packet), media_type="application/json", status_code=200)
