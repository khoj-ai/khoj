# External Packages
from fastapi import APIRouter
from fastapi import Request
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.templating import Jinja2Templates
from khoj.utils.rawconfig import TextContentConfig, OpenAIProcessorConfig, FullConfig

# Internal Packages
from khoj.utils import constants, state

import json


# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)

VALID_TEXT_CONTENT_TYPES = ["org", "markdown", "pdf"]


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
        default_full_config = FullConfig(
            content_type=None,
            search_type=None,
            processor=None,
        )
        current_config = state.config or json.loads(default_full_config.json())

        successfully_configured = {
            "pdf": False,
            "markdown": False,
            "org": False,
            "image": False,
            "github": False,
            "notion": False,
            "conversation": False,
        }

        if state.content_index:
            successfully_configured.update(
                {
                    "pdf": state.content_index.pdf is not None,
                    "markdown": state.content_index.markdown is not None,
                    "org": state.content_index.org is not None,
                    "image": state.content_index.image is not None,
                    "github": state.content_index.github is not None,
                    "notion": state.content_index.notion is not None,
                }
            )

        if state.processor_config:
            successfully_configured.update(
                {
                    "conversation": state.processor_config.conversation is not None,
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
    def github_config_page(request: Request):
        default_copy = constants.default_config.copy()
        default_github = default_copy["content-type"]["github"]  # type: ignore

        default_config = TextContentConfig(
            compressed_jsonl=default_github["compressed-jsonl"],
            embeddings_file=default_github["embeddings-file"],
        )

        current_config = (
            state.config.content_type.github
            if state.config and state.config.content_type and state.config.content_type.github
            else default_config
        )

        current_config = json.loads(current_config.json())

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
