# External Packages
from fastapi import APIRouter
from fastapi import Request
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.templating import Jinja2Templates
from starlette.authentication import requires
from khoj.utils.rawconfig import (
    TextContentConfig,
    OpenAIProcessorConfig,
    FullConfig,
    GithubContentConfig,
    GithubRepoConfig,
)

# Internal Packages
from khoj.utils import constants, state
from database.adapters import EmbeddingsAdapters, get_user_github_config

import json


# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)

VALID_TEXT_CONTENT_TYPES = ["org", "markdown", "pdf", "plaintext"]


# Create Routes
@web_client.get("/", response_class=FileResponse)
def index(request: Request):
    return templates.TemplateResponse("index.html", context={"request": request, "demo": state.demo})


@web_client.get("/chat", response_class=FileResponse)
def chat_page(request: Request):
    return templates.TemplateResponse("chat.html", context={"request": request, "demo": state.demo})


if not state.demo:

    @web_client.get("/config", response_class=HTMLResponse)
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
            },
        )

    @web_client.get("/config/content_type/github", response_class=HTMLResponse)
    @requires(["authenticated"])
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
            current_config = {}

        return templates.TemplateResponse(
            "content_type_github_input.html", context={"request": request, "current_config": current_config}
        )

    @web_client.get("/config/content_type/notion", response_class=HTMLResponse)
    def notion_config_page(request: Request):
        default_copy = constants.default_config.copy()
        default_notion = default_copy["content-type"]["notion"]  # type: ignore

        default_config = TextContentConfig(
            compressed_jsonl=default_notion["compressed-jsonl"],
            embeddings_file=default_notion["embeddings-file"],
        )

        current_config = (
            state.config.content_type.notion
            if state.config and state.config.content_type and state.config.content_type.notion
            else default_config
        )

        current_config = json.loads(current_config.json())

        return templates.TemplateResponse(
            "content_type_notion_input.html", context={"request": request, "current_config": current_config}
        )

    @web_client.get("/config/content_type/{content_type}", response_class=HTMLResponse)
    def content_config_page(request: Request, content_type: str):
        if content_type not in VALID_TEXT_CONTENT_TYPES:
            return templates.TemplateResponse("config.html", context={"request": request})

        default_copy = constants.default_config.copy()
        default_content_type = default_copy["content-type"][content_type]  # type: ignore

        default_config = TextContentConfig(
            compressed_jsonl=default_content_type["compressed-jsonl"],
            embeddings_file=default_content_type["embeddings-file"],
        )

        current_config = (
            state.config.content_type[content_type]
            if state.config and state.config.content_type and state.config.content_type[content_type]  # type: ignore
            else default_config
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
