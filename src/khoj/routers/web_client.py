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
    TextContentConfig,
    OpenAIProcessorConfig,
    FullConfig,
    GithubContentConfig,
    GithubRepoConfig,
    NotionContentConfig,
)

# Internal Packages
from khoj.utils import constants, state
from database.adapters import EmbeddingsAdapters, get_user_github_config, get_user_notion_config
from database.models import LocalOrgConfig, LocalMarkdownConfig, LocalPdfConfig, LocalPlaintextConfig


# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)

VALID_TEXT_CONTENT_TYPES = ["org", "markdown", "pdf", "plaintext"]


# Create Routes
@web_client.get("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index(request: Request):
    return templates.TemplateResponse("index.html", context={"request": request, "demo": state.demo})


@web_client.post("/", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def index_post(request: Request):
    return templates.TemplateResponse("index.html", context={"request": request, "demo": state.demo})


@web_client.get("/chat", response_class=FileResponse)
@requires(["authenticated"], redirect="login_page")
def chat_page(request: Request):
    return templates.TemplateResponse("chat.html", context={"request": request, "demo": state.demo})


@web_client.get("/login", response_class=FileResponse)
def login_page(request: Request):
    if request.user.is_authenticated:
        next_url = request.query_params.get("next", "/")
        return RedirectResponse(url=next_url)
    google_client_id = os.environ.get("GOOGLE_CLIENT_ID")
    redirect_uri = request.url_for("auth")
    return templates.TemplateResponse(
        "login.html",
        context={
            "request": request,
            "demo": state.demo,
            "google_client_id": google_client_id,
            "redirect_uri": redirect_uri,
        },
    )


def map_config_to_object(content_type: str):
    if content_type == "org":
        return LocalOrgConfig
    if content_type == "markdown":
        return LocalMarkdownConfig
    if content_type == "pdf":
        return LocalPdfConfig
    if content_type == "plaintext":
        return LocalPlaintextConfig


if not state.demo:

    @web_client.get("/config", response_class=HTMLResponse)
    @requires(["authenticated"], redirect="login_page")
    def config_page(request: Request):
        user = request.user.object if request.user.is_authenticated else None
        enabled_content = set(EmbeddingsAdapters.get_unique_file_types(user).all())
        default_full_config = FullConfig(
            content_type=None,
            search_type=None,
            processor=None,
        )
        current_config = state.config or json.loads(default_full_config.json())

        successfully_configured = {
            "pdf": ("pdf" in enabled_content),
            "markdown": ("markdown" in enabled_content),
            "org": ("org" in enabled_content),
            "image": False,
            "github": ("github" in enabled_content),
            "notion": ("notion" in enabled_content),
            "plaintext": ("plaintext" in enabled_content),
            "enable_offline_model": False,
            "conversation_openai": False,
            "conversation_gpt4all": False,
        }

        if state.content_index:
            successfully_configured.update(
                {
                    "image": state.content_index.image is not None,
                }
            )

        if state.processor_config and state.processor_config.conversation:
            successfully_configured.update(
                {
                    "conversation_openai": state.processor_config.conversation.openai_model is not None,
                    "conversation_gpt4all": state.processor_config.conversation.gpt4all_model.loaded_model is not None,
                }
            )

        return templates.TemplateResponse(
            "config.html",
            context={
                "request": request,
                "current_config": current_config,
                "current_model_state": successfully_configured,
                "anonymous_mode": state.anonymous_mode,
            },
        )

    @web_client.get("/config/content_type/github", response_class=HTMLResponse)
    @requires(["authenticated"], redirect="login_page")
    def github_config_page(request: Request):
        user = request.user.object if request.user.is_authenticated else None
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
            "content_type_github_input.html", context={"request": request, "current_config": current_config}
        )

    @web_client.get("/config/content_type/notion", response_class=HTMLResponse)
    @requires(["authenticated"], redirect="login_page")
    def notion_config_page(request: Request):
        user = request.user.object if request.user.is_authenticated else None
        current_notion_config = get_user_notion_config(user)

        current_config = NotionContentConfig(
            token=current_notion_config.token if current_notion_config else "",
        )

        current_config = json.loads(current_config.json())

        return templates.TemplateResponse(
            "content_type_notion_input.html", context={"request": request, "current_config": current_config}
        )

    @web_client.get("/config/content_type/{content_type}", response_class=HTMLResponse)
    @requires(["authenticated"], redirect="login_page")
    def content_config_page(request: Request, content_type: str):
        if content_type not in VALID_TEXT_CONTENT_TYPES:
            return templates.TemplateResponse("config.html", context={"request": request})

        object = map_config_to_object(content_type)
        user = request.user.object if request.user.is_authenticated else None
        config = object.objects.filter(user=user).first()
        if config == None:
            config = object.objects.create(user=user)

        current_config = TextContentConfig(
            input_files=config.input_files,
            input_filter=config.input_filter,
            index_heading_entries=config.index_heading_entries,
        )
        current_config = json.loads(current_config.json())

        return templates.TemplateResponse(
            "content_type_input.html",
            context={
                "request": request,
                "current_config": current_config,
                "content_type": content_type,
            },
        )

    @web_client.get("/config/processor/conversation/openai", response_class=HTMLResponse)
    def conversation_processor_config_page(request: Request):
        default_copy = constants.default_config.copy()
        default_processor_config = default_copy["processor"]["conversation"]["openai"]  # type: ignore
        default_openai_config = OpenAIProcessorConfig(
            api_key="",
            chat_model=default_processor_config["chat-model"],
        )

        current_processor_openai_config = (
            state.config.processor.conversation.openai
            if state.config
            and state.config.processor
            and state.config.processor.conversation
            and state.config.processor.conversation.openai
            else default_openai_config
        )
        current_processor_openai_config = json.loads(current_processor_openai_config.json())

        return templates.TemplateResponse(
            "processor_conversation_input.html",
            context={
                "request": request,
                "current_config": current_processor_openai_config,
            },
        )
