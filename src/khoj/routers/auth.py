import asyncio
import datetime
import logging
import os
from typing import Optional
from urllib.parse import urlencode, urlparse, urlunparse

import requests
from fastapi import APIRouter, Depends, HTTPException
from starlette.authentication import requires
from starlette.config import Config
from starlette.requests import Request
from starlette.responses import HTMLResponse, RedirectResponse, Response
from starlette.status import HTTP_302_FOUND

from khoj.app.settings import DISABLE_HTTPS
from khoj.database.adapters import (
    acreate_khoj_token,
    aget_or_create_user_by_email,
    aget_user_validated_by_email_verification_code,
    delete_khoj_token,
    get_khoj_tokens,
    get_or_create_user,
)
from khoj.routers.email import send_magic_link_email, send_welcome_email
from khoj.routers.helpers import (
    EmailAttemptRateLimiter,
    EmailVerificationApiRateLimiter,
    MagicLinkForm,
    get_next_url,
    update_telemetry_state,
)
from khoj.utils import state
from khoj.utils.helpers import in_debug_mode

logger = logging.getLogger(__name__)

auth_router = APIRouter()


if not state.anonymous_mode:
    missing_requirements = []
    from authlib.integrations.starlette_client import OAuth, OAuthError
    from google.auth.transport import requests as google_requests
    from google.oauth2 import id_token

    if not os.environ.get("RESEND_API_KEY") and (
        not os.environ.get("GOOGLE_CLIENT_ID") or not os.environ.get("GOOGLE_CLIENT_SECRET")
    ):
        missing_requirements += [
            "Set your RESEND_API_KEY or GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET as environment variables"
        ]
    if missing_requirements:
        requirements_string = "\n   - " + "\n   - ".join(missing_requirements)
        error_msg = f"🚨 Start Khoj with --anonymous-mode flag or to enable authentication:{requirements_string}"
        logger.error(error_msg)

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


@auth_router.post("/magic")
async def login_magic_link(
    request: Request,
    form: MagicLinkForm,
    email_limiter=Depends(EmailAttemptRateLimiter(requests=20, window=60 * 60 * 24, slug="magic_link_login_by_email")),
):
    if request.user.is_authenticated:
        # Clear the session if user is already authenticated
        request.session.pop("user", None)

    # Get/create user if valid email address
    check_deliverability = state.billing_enabled and not in_debug_mode()
    user, is_new = await aget_or_create_user_by_email(form.email, check_deliverability=check_deliverability)
    if not user:
        raise HTTPException(status_code=404, detail="Invalid email address. Please fix before trying again.")

    # Rate limit email login by user
    user_limiter = EmailVerificationApiRateLimiter(requests=10, window=60 * 60 * 24, slug="magic_link_login_by_user")
    await user_limiter(email=user.email)

    # Send email with magic link
    unique_id = user.email_verification_code
    await send_magic_link_email(user.email, unique_id, request.base_url)
    if is_new:
        update_telemetry_state(
            request=request,
            telemetry_type="api",
            api="create_user__email",
            metadata={"server_id": str(user.uuid)},
        )
        logger.log(logging.INFO, f"🥳 New User Created: {user.uuid}")

    return Response(status_code=200)


@auth_router.get("/magic")
async def sign_in_with_magic_link(
    request: Request,
    code: str,
    email: str,
    rate_limiter=Depends(
        EmailVerificationApiRateLimiter(requests=10, window=60 * 60 * 24, slug="magic_link_verification")
    ),
):
    user, code_is_expired = await aget_user_validated_by_email_verification_code(code, email)

    if user:
        if code_is_expired:
            request.session["user"] = {}
            return Response(status_code=403)

        id_info = {
            "email": user.email,
        }

        request.session["user"] = dict(id_info)
        return RedirectResponse(url="/")
    return Response(status_code=401)


@auth_router.post("/token")
@requires(["authenticated"], redirect="login_page")
async def generate_token(request: Request, token_name: Optional[str] = None):
    "Generate API token for given user"
    if token_name:
        token = await acreate_khoj_token(user=request.user.object, name=token_name)
    else:
        token = await acreate_khoj_token(user=request.user.object)
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
async def delete_token(request: Request, token: str):
    "Delete API token for given user"
    return await delete_khoj_token(user=request.user.object, token=token)


@auth_router.post("/redirect")
async def auth_post(request: Request):
    # This is maintained for compatibility with the /login endpoint
    form = await request.form()
    next_url = get_next_url(request)
    for q in request.query_params:
        if q != "next":
            next_url += f"&{q}={request.query_params[q]}"

    credential = form.get("credential")

    csrf_token_cookie = request.cookies.get("g_csrf_token")
    if not csrf_token_cookie:
        logger.info("Missing CSRF token. Redirecting user to login page")
        return RedirectResponse(url=next_url)
    csrf_token_body = form.get("g_csrf_token")
    if not csrf_token_body:
        logger.info("Missing CSRF token body. Redirecting user to login page")
        return RedirectResponse(url=next_url)
    if csrf_token_cookie != csrf_token_body:
        return Response("Invalid CSRF token", status_code=400)

    try:
        idinfo = id_token.verify_oauth2_token(credential, google_requests.Request(), os.environ["GOOGLE_CLIENT_ID"])
    except OAuthError as error:
        return HTMLResponse(f"<h1>{error.error}</h1>")
    khoj_user = await get_or_create_user(idinfo)

    if khoj_user:
        request.session["user"] = dict(idinfo)

        if datetime.timedelta(minutes=3) > (datetime.datetime.now(datetime.timezone.utc) - khoj_user.date_joined):
            asyncio.create_task(send_welcome_email(idinfo["name"], idinfo["email"]))
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="create_user__google",
                metadata={"server_id": str(khoj_user.uuid)},
            )
            logger.log(logging.INFO, f"🥳 New User Created: {khoj_user.uuid}")
            return RedirectResponse(url=next_url, status_code=HTTP_302_FOUND)

    return RedirectResponse(url=next_url, status_code=HTTP_302_FOUND)


@auth_router.get("/redirect")
async def auth(request: Request):
    next_url_path = get_next_url(request)

    # Add query params from request, excluding OAuth params to next URL
    oauth_params = {"code", "state", "scope", "authuser", "prompt", "session_state", "access_type", "next"}
    query_params = {param: value for param, value in request.query_params.items() if param not in oauth_params}

    # Rebuild next URL with updated query params
    parsed_next_url_path = urlparse(next_url_path)
    next_url = urlunparse(
        (
            parsed_next_url_path.scheme,
            parsed_next_url_path.netloc,
            parsed_next_url_path.path,
            parsed_next_url_path.params,
            urlencode(query_params, doseq=True),
            parsed_next_url_path.fragment,
        )
    )

    # Construct the full redirect URI including domain
    base_url = str(request.base_url).rstrip("/")
    if not DISABLE_HTTPS:
        base_url = base_url.replace("http://", "https://")
    redirect_uri = f"{base_url}{request.app.url_path_for('auth')}"

    # Build the payload for the token request
    code = request.query_params.get("code")
    payload = {
        "code": code,
        "client_id": os.environ["GOOGLE_CLIENT_ID"],
        "client_secret": os.environ["GOOGLE_CLIENT_SECRET"],
        "redirect_uri": redirect_uri,
        "grant_type": "authorization_code",
    }

    # Request the token from Google
    verified_data = requests.post(
        "https://oauth2.googleapis.com/token",
        headers={"Content-Type": "application/x-www-form-urlencoded"},
        data=payload,
    )

    # Validate the OAuth response
    if verified_data.status_code != 200:
        logger.error(f"Token request failed: {verified_data.text}")
        try:
            error_json = verified_data.json()
            logger.error(f"Error response JSON for Google verification: {error_json}")
        except ValueError:
            logger.error("Response content is not valid JSON")
        verified_data.raise_for_status()

    credential = verified_data.json().get("id_token")
    if not credential:
        logger.error("Missing id_token in OAuth response")
        return RedirectResponse(url="/login?error=invalid_token", status_code=HTTP_302_FOUND)

    # Validate the OAuth token
    try:
        idinfo = id_token.verify_oauth2_token(credential, google_requests.Request(), os.environ["GOOGLE_CLIENT_ID"])
    except OAuthError as error:
        return HTMLResponse(f"<h1>{error.error}</h1>")

    # Get or create the authenticated user in the database
    khoj_user = await get_or_create_user(idinfo)

    # Set the user session if the user is authenticated
    if khoj_user:
        request.session["user"] = dict(idinfo)

        # Send a welcome email to new users
        if datetime.timedelta(minutes=3) > (datetime.datetime.now(datetime.timezone.utc) - khoj_user.date_joined):
            asyncio.create_task(send_welcome_email(idinfo["name"], idinfo["email"]))
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api="create_user__google",
                metadata={"server_id": str(khoj_user.uuid)},
            )
            logger.log(logging.INFO, f"🥳 New User Created: {khoj_user.uuid}")

    # Redirect the user to the next URL
    return RedirectResponse(url=next_url, status_code=HTTP_302_FOUND)


@auth_router.get("/logout")
async def logout(request: Request):
    request.session.pop("user", None)
    return RedirectResponse(url="/")


@auth_router.get("/oauth/metadata")
async def oauth_metadata(request: Request):
    redirect_uri = str(request.app.url_path_for("auth"))

    return {
        "google": {
            "client_id": os.environ.get("GOOGLE_CLIENT_ID"),
            "redirect_uri": f"{redirect_uri}",
        }
    }
