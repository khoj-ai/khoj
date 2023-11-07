# System Packages
import json
import os

# External Packages
from fastapi import APIRouter
from fastapi import Request
from fastapi.responses import HTMLResponse, FileResponse, RedirectResponse
from fastapi.templating import Jinja2Templates
from starlette.authentication import requires
from khoj.utils.rawconfig import (
    GithubContentConfig,
    GithubRepoConfig,
    NotionContentConfig,
)

# Internal Packages
from khoj.utils import constants, state
from database.adapters import EntryAdapters, get_user_github_config, get_user_notion_config, ConversationAdapters

# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)


# Create Routes
@web_client.get("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")

    return templates.TemplateResponse(
        "chat.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
        },
    )


@web_client.post("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index_post(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")

    return templates.TemplateResponse(
        "chat.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
        },
    )


@web_client.get("/search", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def search_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")

    return templates.TemplateResponse(
        "search.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
        },
    )


@web_client.get("/chat", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def chat_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")

    return templates.TemplateResponse(
        "chat.html",
        context={
            "request": request,
            "username": user.username,
            "user_photo": user_picture,
        },
    )


@web_client.get("/login", response_class=FileResponse)
def login_page(request: Request):
    if request.user.is_authenticated:
        next_url = request.query_params.get("next", "/")
        return RedirectResponse(url=next_url)
    google_client_id = os.environ.get("GOOGLE_CLIENT_ID")
    redirect_uri = str(request.app.url_path_for("auth"))
    return templates.TemplateResponse(
        "login.html",
        context={
            "request": request,
            "google_client_id": google_client_id,
            "redirect_uri": redirect_uri,
        },
    )


@web_client.get("/config", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def config_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    enabled_content = set(EntryAdapters.get_unique_file_types(user).all())

    successfully_configured = {
        "pdf": ("pdf" in enabled_content),
        "markdown": ("markdown" in enabled_content),
        "org": ("org" in enabled_content),
        "image": False,
        "github": ("github" in enabled_content),
        "notion": ("notion" in enabled_content),
        "plaintext": ("plaintext" in enabled_content),
    }

    if state.content_index:
        successfully_configured.update(
            {
                "image": state.content_index.image is not None,
            }
        )

    conversation_options = ConversationAdapters.get_conversation_processor_options().all()
    all_conversation_options = list()
    for conversation_option in conversation_options:
        all_conversation_options.append({"chat_model": conversation_option.chat_model, "id": conversation_option.id})

    selected_conversation_config = ConversationAdapters.get_conversation_config(user)

    return templates.TemplateResponse(
        "config.html",
        context={
            "request": request,
            "current_model_state": successfully_configured,
            "anonymous_mode": state.anonymous_mode,
            "username": user.username if user else None,
            "conversation_options": all_conversation_options,
            "selected_conversation_config": selected_conversation_config.id if selected_conversation_config else None,
            "user_photo": user_picture,
        },
    )


@web_client.get("/config/content_type/github", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def github_config_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
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
        "content_type_github_input.html",
        context={
            "request": request,
            "current_config": current_config,
            "username": user.username,
            "user_photo": user_picture,
        },
    )


@web_client.get("/config/content_type/notion", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def notion_config_page(request: Request):
    user = request.user.object
    user_picture = request.session.get("user", {}).get("picture")
    current_notion_config = get_user_notion_config(user)

    current_config = NotionContentConfig(
        token=current_notion_config.token if current_notion_config else "",
    )

    current_config = json.loads(current_config.json())

    return templates.TemplateResponse(
        "content_type_notion_input.html",
        context={
            "request": request,
            "current_config": current_config,
            "username": user.username,
            "user_photo": user_picture,
        },
    )
