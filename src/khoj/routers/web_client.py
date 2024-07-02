# System Packages
import json
import os
from datetime import timedelta
from typing import Optional

from fastapi import APIRouter, Request
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse
from fastapi.templating import Jinja2Templates
from starlette.authentication import has_required_scope, requires

from khoj.database import adapters
from khoj.database.adapters import (
    AgentAdapters,
    ConversationAdapters,
    EntryAdapters,
    PublicConversationAdapters,
    get_user_github_config,
    get_user_name,
    get_user_notion_config,
    get_user_subscription_state,
)
from khoj.database.models import KhojUser
from khoj.processor.speech.text_to_speech import is_eleven_labs_enabled
from khoj.routers.helpers import get_next_url
from khoj.routers.notion import get_notion_auth_url
from khoj.routers.twilio import is_twilio_enabled
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
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)

    return templates.TemplateResponse(
        "chat.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


@web_client.post("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index_post(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)

    return templates.TemplateResponse(
        "chat.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


@web_client.get("/search", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def search_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)

    return templates.TemplateResponse(
        "search.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


@web_client.get("/chat", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def chat_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)

    return templates.TemplateResponse(
        "chat.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


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
    user_picture = request.session.get("user", {}).get("picture") if user else None

    agent = AgentAdapters.get_agent_by_slug(agent_slug)
    has_documents = EntryAdapters.user_has_entries(user=user)

    if agent == None:
        return templates.TemplateResponse(
            "404.html",
            context={
                "request": request,
                "khoj_version": state.khoj_version,
                "username": user.username if user else None,
                "has_documents": False,
                "is_active": has_required_scope(request, ["premium"]),
                "user_photo": user_picture,
            },
        )

    agent_metadata = {
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

    return templates.TemplateResponse(
        "agent.html",
        context={
            "request": request,
            "agent": agent_metadata,
            "khoj_version": state.khoj_version,
            "username": user.username if user else None,
            "has_documents": has_documents,
            "is_active": has_required_scope(request, ["premium"]),
            "user_photo": user_picture,
        },
    )


@web_client.get("/config", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def config_page(request: Request):
    user: KhojUser = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)

    user_subscription_state = get_user_subscription_state(user.email)
    user_subscription = adapters.get_user_subscription(user.email)
    subscription_renewal_date = (
        user_subscription.renewal_date.strftime("%d %b %Y")
        if user_subscription and user_subscription.renewal_date
        else (user_subscription.created_at + timedelta(days=7)).strftime("%d %b %Y")
    )
    given_name = get_user_name(user)

    enabled_content_source = set(EntryAdapters.get_unique_file_sources(user))
    successfully_configured = {
        "computer": ("computer" in enabled_content_source),
        "github": ("github" in enabled_content_source),
        "notion": ("notion" in enabled_content_source),
    }

    selected_conversation_config = ConversationAdapters.get_conversation_config(user)
    conversation_options = ConversationAdapters.get_conversation_processor_options().all()
    all_conversation_options = list()
    for conversation_option in conversation_options:
        all_conversation_options.append({"chat_model": conversation_option.chat_model, "id": conversation_option.id})

    search_model_options = adapters.get_or_create_search_models().all()
    all_search_model_options = list()
    for search_model_option in search_model_options:
        all_search_model_options.append({"name": search_model_option.name, "id": search_model_option.id})

    current_search_model_option = adapters.get_user_search_model_or_default(user)

    selected_paint_model_config = ConversationAdapters.get_user_text_to_image_model_config(user)
    paint_model_options = ConversationAdapters.get_text_to_image_model_options().all()
    all_paint_model_options = list()
    for paint_model in paint_model_options:
        all_paint_model_options.append({"model_name": paint_model.model_name, "id": paint_model.id})

    notion_oauth_url = get_notion_auth_url(user)

    eleven_labs_enabled = is_eleven_labs_enabled()

    voice_models = ConversationAdapters.get_voice_model_options()
    voice_model_options = list()
    for voice_model in voice_models:
        voice_model_options.append({"name": voice_model.name, "id": voice_model.model_id})

    if len(voice_model_options) == 0:
        eleven_labs_enabled = False

    selected_voice_config = ConversationAdapters.get_voice_model_config(user)

    return templates.TemplateResponse(
        "config.html",
        context={
            "request": request,
            "current_model_state": successfully_configured,
            "anonymous_mode": state.anonymous_mode,
            "username": user.username,
            "given_name": given_name,
            "search_model_options": all_search_model_options,
            "selected_search_model_config": current_search_model_option.id,
            "conversation_options": all_conversation_options,
            "selected_conversation_config": selected_conversation_config.id if selected_conversation_config else None,
            "paint_model_options": all_paint_model_options,
            "selected_paint_model_config": selected_paint_model_config.id if selected_paint_model_config else None,
            "user_photo": user_picture,
            "billing_enabled": state.billing_enabled,
            "subscription_state": user_subscription_state,
            "subscription_renewal_date": subscription_renewal_date,
            "khoj_cloud_subscription_url": os.getenv("KHOJ_CLOUD_SUBSCRIPTION_URL"),
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "is_twilio_enabled": is_twilio_enabled(),
            "is_eleven_labs_enabled": eleven_labs_enabled,
            "voice_model_options": voice_model_options,
            "selected_voice_config": selected_voice_config.model_id if selected_voice_config else None,
            "phone_number": user.phone_number,
            "is_phone_number_verified": user.verified_phone_number,
            "khoj_version": state.khoj_version,
            "notion_oauth_url": notion_oauth_url,
        },
    )


@web_client.get("/config/content-source/github", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def github_config_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)
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

    return templates.TemplateResponse(
        "content_source_github_input.html",
        context={
            "request": request,
            "current_config": current_config,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


@web_client.get("/config/content-source/notion", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def notion_config_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user)
    current_notion_config = get_user_notion_config(user)

    current_config = NotionContentConfig(
        token=current_notion_config.token if current_notion_config else "",
    )

    current_config = json.loads(current_config.model_dump_json())

    return templates.TemplateResponse(
        "content_source_notion_input.html",
        context={
            "request": request,
            "current_config": current_config,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


@web_client.get("/config/content-source/computer", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def computer_config_page(request: Request):
    user = request.user.object if request.user.is_authenticated else None
    user_picture = request.session.get("user", {}).get("picture") if user else None
    has_documents = EntryAdapters.user_has_entries(user=user) if user else False

    return templates.TemplateResponse(
        "content_source_computer_input.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
        },
    )


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
    user_picture = request.session.get("user", {}).get("picture") if user else None
    has_documents = EntryAdapters.user_has_entries(user=user) if user else False

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

    google_client_id = os.environ.get("GOOGLE_CLIENT_ID")
    redirect_uri = str(request.app.url_path_for("auth"))
    next_url = str(
        request.app.url_path_for("view_public_conversation", public_conversation_slug=public_conversation_slug)
    )

    return templates.TemplateResponse(
        "public_conversation.html",
        context={
            "request": request,
            "username": user.username if user else None,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
            "public_conversation_slug": public_conversation_slug,
            "agents": agents_packet,
            "google_client_id": google_client_id,
            "redirect_uri": f"{redirect_uri}?next={next_url}",
        },
    )


@web_client.get("/automations", response_class=HTMLResponse)
def automations_config_page(
    request: Request,
    subject: Optional[str] = None,
    crontime: Optional[str] = None,
    queryToRun: Optional[str] = None,
):
    user = request.user.object if request.user.is_authenticated else None
    user_picture = request.session.get("user", {}).get("picture")
    has_documents = EntryAdapters.user_has_entries(user=user) if user else False

    return templates.TemplateResponse(
        "config_automation.html",
        context={
            "request": request,
            "username": user.username if user else None,
            "user_photo": user_picture,
            "is_active": has_required_scope(request, ["premium"]),
            "has_documents": has_documents,
            "khoj_version": state.khoj_version,
            "subject": subject if subject else "",
            "crontime": crontime if crontime else "",
            "queryToRun": queryToRun if queryToRun else "",
        },
    )
