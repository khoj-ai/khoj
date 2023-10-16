import logging
import json
import os
from fastapi import APIRouter
from starlette.config import Config
from starlette.requests import Request
from starlette.responses import HTMLResponse, RedirectResponse
from authlib.integrations.starlette_client import OAuth, OAuthError

from google.oauth2 import id_token
from google.auth.transport import requests as google_requests

from database.adapters import get_or_create_user

logger = logging.getLogger(__name__)

auth_router = APIRouter()

if not os.environ.get("GOOGLE_CLIENT_ID") or not os.environ.get("GOOGLE_CLIENT_SECRET"):
    logger.info("Please set GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET environment variables to use Google OAuth")
else:
    config = Config(environ=os.environ)

    oauth = OAuth(config)

    CONF_URL = "https://accounts.google.com/.well-known/openid-configuration"
    oauth.register(name="google", server_metadata_url=CONF_URL, client_kwargs={"scope": "openid email profile"})


@auth_router.get("/")
async def homepage(request: Request):
    user = request.session.get("user")
    if user:
        data = json.dumps(user)
        html = f"<pre>{data}</pre>" '<a href="/logout">logout</a>'
        return HTMLResponse(html)
    return HTMLResponse('<a href="/login">login</a>')


@auth_router.get("/login")
async def login(request: Request):
    redirect_uri = request.url_for("auth")
    return await oauth.google.authorize_redirect(request, redirect_uri)


@auth_router.post("/login")
async def login(request: Request):
    redirect_uri = request.url_for("auth")
    return await oauth.google.authorize_redirect(request, redirect_uri)


@auth_router.get("/redirect")
async def auth(request: Request):
    try:
        token = await oauth.google.authorize_access_token(request)
    except OAuthError as error:
        return HTMLResponse(f"<h1>{error.error}</h1>")
    khoj_user = await get_or_create_user(token)
    user = token.get("userinfo")
    if user:
        request.session["user"] = dict(user)
    return RedirectResponse(url="/")


@auth_router.post("/redirect")
async def auth(request: Request):
    form = await request.form()
    credential = form.get("credential")
    try:
        idinfo = id_token.verify_oauth2_token(credential, google_requests.Request(), os.environ["GOOGLE_CLIENT_ID"])
    except OAuthError as error:
        return HTMLResponse(f"<h1>{error.error}</h1>")
    khoj_user = await get_or_create_user(idinfo)
    user = idinfo.get("userinfo")
    if user:
        request.session["user"] = dict(user)
    return RedirectResponse(url="/")


@auth_router.get("/logout")
async def logout(request: Request):
    request.session.pop("user", None)
    return RedirectResponse(url="/")
