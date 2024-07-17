# System Packages
import json
import os
from typing import Optional

from fastapi import APIRouter, Request
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse
from fastapi.templating import Jinja2Templates
from starlette.authentication import requires

from khoj.database.adapters import (
    AgentAdapters,
    PublicConversationAdapters,
    get_user_github_config,
    get_user_notion_config,
)
from khoj.database.models import KhojUser
from khoj.routers.helpers import get_next_url, get_user_config
from khoj.utils import constants, state
from khoj.utils.rawconfig import (
    GithubContentConfig,
    GithubRepoConfig,
    NotionContentConfig,
)

# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates([constants.web_directory, constants.next_js_directory])


# Create Routes
@web_client.get("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index(request: Request):
    user = request.user.object
    user_config = get_user_config(user, request)

    return templates.TemplateResponse("chat.html", context=user_config)


@web_client.post("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index_post(request: Request):
    user = request.user.object
    user_config = get_user_config(user, request)

    return templates.TemplateResponse("chat.html", context=user_config)


@web_client.get("/search", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def search_page(request: Request):
    user = request.user.object
    user_config = get_user_config(user, request)

    return templates.TemplateResponse("search.html", context=user_config)


@web_client.get("/chat", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def chat_page(request: Request):
    user = request.user.object
    user_config = get_user_config(user, request)

    return templates.TemplateResponse("chat.html", context=user_config)


@web_client.get("/experimental", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def experimental_page(request: Request):
    return templates.TemplateResponse(
        "index.html",
        context={
            "request": request,
        },
    )


@web_client.get("/factchecker", response_class=FileResponse)
def fact_checker_page(request: Request):
    return templates.TemplateResponse(
        "factchecker/index.html",
        context={
            "request": request,
        },
    )


@web_client.get("/login", response_class=FileResponse)
def login_page(request: Request):
    next_url = get_next_url(request)
    if request.user.is_authenticated:
        return RedirectResponse(url=next_url)
    google_client_id = os.environ.get("GOOGLE_CLIENT_ID")
    redirect_uri = str(request.app.url_path_for("auth"))
    return templates.TemplateResponse(
        "login.html",
        context={
            "request": request,
            "google_client_id": google_client_id,
            "redirect_uri": f"{redirect_uri}?next={next_url}",
        },
    )


@web_client.get("/agents", response_class=HTMLResponse)
def agents_page(request: Request):
    return templates.TemplateResponse(
        "agents/index.html",
        context={
            "request": request,
        },
    )


@web_client.get("/agent/{agent_slug}", response_class=HTMLResponse)
def agent_page(request: Request, agent_slug: str):
    user: KhojUser = request.user.object if request.user.is_authenticated else None
    user_config = get_user_config(user, request)
    agent = AgentAdapters.get_agent_by_slug(agent_slug)

    if agent == None:
        user_config["has_documents"] = False
        return templates.TemplateResponse("404.html", context=user_config)

    user_config["agent"] = {
        "slug": agent.slug,
        "avatar": agent.avatar,
        "name": agent.name,
        "personality": agent.personality,
        "public": agent.public,
        "creator": agent.creator.username if agent.creator else None,
        "managed_by_admin": agent.managed_by_admin,
        "chat_model": agent.chat_model.chat_model,
        "creator_not_self": agent.creator != user,
    }

    return templates.TemplateResponse("agent.html", context=user_config)


@web_client.get("/settings", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def config_page(request: Request):
    user: KhojUser = request.user.object
    user_config = get_user_config(user, request, is_detailed=True)

    return templates.TemplateResponse("settings.html", context=user_config)


@web_client.get("/settings/content/github", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def github_config_page(request: Request):
    user = request.user.object
    user_config = get_user_config(user, request)
    current_github_config = get_user_github_config(user)

    if current_github_config:
        raw_repos = current_github_config.githubrepoconfig.all()
        repos = []
        for repo in raw_repos:
            repos.append(
                GithubRepoConfig(
                    name=repo.name,
                    owner=repo.owner,
                    branch=repo.branch,
                )
            )
        current_config = GithubContentConfig(
            pat_token=current_github_config.pat_token,
            repos=repos,
        )
        current_config = json.loads(current_config.json())
    else:
        current_config = {}  # type: ignore

    user_config["current_config"] = current_config
    return templates.TemplateResponse("content_source_github_input.html", context=user_config)


@web_client.get("/settings/content/notion", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def notion_config_page(request: Request):
    user = request.user.object
    user_config = get_user_config(user, request)

    current_notion_config = get_user_notion_config(user)
    token = current_notion_config.token if current_notion_config else ""
    current_config = NotionContentConfig(token=token)
    current_config = json.loads(current_config.model_dump_json())

    user_config["current_config"] = current_config
    return templates.TemplateResponse("content_source_notion_input.html", context=user_config)


@web_client.get("/settings/content/computer", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def computer_config_page(request: Request):
    user = request.user.object if request.user.is_authenticated else None
    user_config = get_user_config(user, request)

    return templates.TemplateResponse("content_source_computer_input.html", context=user_config)


@web_client.get("/share/chat/{public_conversation_slug}", response_class=HTMLResponse)
def view_public_conversation(request: Request):
    public_conversation_slug = request.path_params.get("public_conversation_slug")
    public_conversation = PublicConversationAdapters.get_public_conversation_by_slug(public_conversation_slug)
    if not public_conversation:
        return templates.TemplateResponse(
            "404.html",
            context={
                "request": request,
                "khoj_version": state.khoj_version,
            },
        )
    user = request.user.object if request.user.is_authenticated else None
    user_config = get_user_config(user, request)
    user_config["public_conversation_slug"] = public_conversation_slug
    user_config["google_client_id"] = os.environ.get("GOOGLE_CLIENT_ID")

    all_agents = AgentAdapters.get_all_accessible_agents(request.user.object if request.user.is_authenticated else None)

    # Filter out the current agent
    all_agents = [agent for agent in all_agents if agent != public_conversation.agent]
    agents_packet = []
    for agent in all_agents:
        agents_packet.append(
            {
                "slug": agent.slug,
                "avatar": agent.avatar,
                "name": agent.name,
            }
        )
    user_config["agents"] = agents_packet

    redirect_uri = str(request.app.url_path_for("auth"))
    next_url = str(
        request.app.url_path_for("view_public_conversation", public_conversation_slug=public_conversation_slug)
    )
    user_config["redirect_uri"] = f"{redirect_uri}?next={next_url}"

    return templates.TemplateResponse("public_conversation.html", context=user_config)


@web_client.get("/automations", response_class=HTMLResponse)
def automations_config_page(
    request: Request,
    subject: Optional[str] = None,
    crontime: Optional[str] = None,
    queryToRun: Optional[str] = None,
):
    user = request.user.object if request.user.is_authenticated else None
    user_config = get_user_config(user, request)
    user_config["subject"] = subject if subject else ""
    user_config["crontime"] = crontime if crontime else ""
    user_config["queryToRun"] = queryToRun if queryToRun else ""

    return templates.TemplateResponse("config_automation.html", context=user_config)
