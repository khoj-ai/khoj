# Standard Packages
import logging
import os
from typing import Optional

# External Packages
from fastapi import APIRouter
from starlette.config import Config
from starlette.requests import Request
from starlette.responses import HTMLResponse, RedirectResponse, Response
from starlette.authentication import requires
from authlib.integrations.starlette_client import OAuth, OAuthError

from google.oauth2 import id_token
from google.auth.transport import requests as google_requests

# Internal Packages
from khoj.database.adapters import get_khoj_tokens, get_or_create_user, create_khoj_token, delete_khoj_token
from khoj.database.models import KhojApiUser
from khoj.routers.helpers import update_telemetry_state
from khoj.utils import state


logger = logging.getLogger(__name__)

auth_router = APIRouter()

if not state.anonymous_mode and not (os.environ.get("GOOGLE_CLIENT_ID") and os.environ.get("GOOGLE_CLIENT_SECRET")):
    logger.warn(
        "🚨 Use --anonymous-mode flag to disable Google OAuth or set GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET environment variables to enable it"
    )
else:
    config = Config(environ=os.environ)

    oauth = OAuth(config)

    CONF_URL = "https://accounts.google.com/.well-known/openid-configuration"
    oauth.register(name="google", server_metadata_url=CONF_URL, client_kwargs={"scope": "openid email profile"})


@auth_router.get("/login")
async def login_get(request: Request):
    redirect_uri = str(request.app.url_path_for("auth"))
    return await oauth.google.authorize_redirect(request, redirect_uri)


@auth_router.post("/login")
async def login(request: Request):
    redirect_uri = str(request.app.url_path_for("auth"))
    return await oauth.google.authorize_redirect(request, redirect_uri)


@auth_router.post("/token")
@requires(["authenticated"], redirect="login_page")
async def generate_token(request: Request, token_name: Optional[str] = None):
    "Generate API token for given user"
    if token_name:
        token = await create_khoj_token(user=request.user.object, name=token_name)
    else:
        token = await create_khoj_token(user=request.user.object)
    return {
        "token": token.token,
        "name": token.name,
    }


@auth_router.get("/token")
@requires(["authenticated"], redirect="login_page")
def get_tokens(request: Request):
    "Get API tokens enabled for given user"
    tokens = get_khoj_tokens(user=request.user.object)
    return tokens


@auth_router.delete("/token")
@requires(["authenticated"], redirect="login_page")
async def delete_token(request: Request, token: str) -> str:
    "Delete API token for given user"
    return await delete_khoj_token(user=request.user.object, token=token)


@auth_router.post("/redirect")
async def auth(request: Request):
    form = await request.form()
    credential = form.get("credential")

    csrf_token_cookie = request.cookies.get("g_csrf_token")
    if not csrf_token_cookie:
        return Response("Missing CSRF token", status_code=400)
    csrf_token_body = form.get("g_csrf_token")
    if not csrf_token_body:
        return Response("Missing CSRF token", status_code=400)
    if csrf_token_cookie != csrf_token_body:
        return Response("Invalid CSRF token", status_code=400)

    try:
        idinfo = id_token.verify_oauth2_token(credential, google_requests.Request(), os.environ["GOOGLE_CLIENT_ID"])
    except OAuthError as error:
        return HTMLResponse(f"<h1>{error.error}</h1>")
    khoj_user = await get_or_create_user(idinfo)
    if khoj_user:
        request.session["user"] = dict(idinfo)

        if not khoj_user.last_login:
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="create_user",
                metadata={"user_id": str(khoj_user.uuid)},
            )
            logger.log(logging.INFO, f"New User Created: {khoj_user.uuid}")
            RedirectResponse(url="/?status=welcome")

    return RedirectResponse(url="/")


@auth_router.get("/logout")
async def logout(request: Request):
    request.session.pop("user", None)
    return RedirectResponse(url="/")
