# External Packages
from fastapi import APIRouter
from fastapi import Request
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.templating import Jinja2Templates

# Internal Packages
from src.utils import constants


# Initialize Router
web_client = APIRouter()
templates = Jinja2Templates(directory=constants.web_directory)


# Create Routes
@web_client.get("/", response_class=FileResponse)
def index():
    return FileResponse(constants.web_directory / "index.html")

@web_client.get('/config', response_class=HTMLResponse)
def config_page(request: Request):
    return templates.TemplateResponse("config.html", context={'request': request})
