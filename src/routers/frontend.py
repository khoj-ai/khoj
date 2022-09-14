# Standard Packages
import logging

# External Packages
from fastapi import APIRouter
from fastapi import Request
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.templating import Jinja2Templates

# Internal Packages
from src.utils import constants


frontend_router = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)
logger = logging.getLogger(__name__)


@frontend_router.get("/", response_class=FileResponse)
def index():
    return FileResponse(constants.web_directory / "index.html")

@frontend_router.get('/config', response_class=HTMLResponse)
def config_page(request: Request):
    return templates.TemplateResponse("config.html", context={'request': request})
