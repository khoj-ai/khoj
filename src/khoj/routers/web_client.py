# System Packages
import json
import os

from fastapi import APIRouter, Request
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse
from fastapi.templating import Jinja2Templates
from starlette.authentication import requires

from khoj.database.adapters import get_user_github_config
from khoj.routers.helpers import get_next_url, get_user_config
from khoj.utils import constants
from khoj.utils.rawconfig import GithubContentConfig, GithubRepoConfig

# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates([constants.web_directory, constants.next_js_directory, constants.pypi_static_directory])


# Create Routes
@web_client.get("/", response_class=FileResponse)
def index(request: Request):
    return templates.TemplateResponse("index.html", context={"request": request})


@web_client.post("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index_post(request: Request):
    return templates.TemplateResponse("index.html", context={"request": request})


@web_client.get("/search", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def search_page(request: Request):
    return templates.TemplateResponse(
        "search/index.html",
        context={
            "request": request,
        },
    )


@web_client.get("/chat", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def chat_page(request: Request):
    return templates.TemplateResponse(
        "chat/index.html",
        context={
            "request": request,
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


@web_client.get("/settings", response_class=HTMLResponse)
@requires(["authenticated"], redirect="login_page")
def config_page(request: Request):
    return templates.TemplateResponse("settings/index.html", context={"request": request})


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


@web_client.get("/share/chat/{public_conversation_slug}", response_class=HTMLResponse)
def view_public_conversation(request: Request):
    return templates.TemplateResponse(
        "share/chat/index.html",
        context={
            "request": request,
        },
    )


@web_client.get("/automations", response_class=HTMLResponse)
def automations_config_page(
    request: Request,
):
    return templates.TemplateResponse(
        "automations/index.html",
        context={
            "request": request,
        },
    )
