import asyncio
import datetime
import logging
import os
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException
from starlette.authentication import requires
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
    get_or_create_user_oauth,
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
from khoj.utils.helpers import in_debug_mode, is_env_var_true
from khoj.utils.oauth_config import get_all_oauth_configs

logger = logging.getLogger(__name__)

auth_router = APIRouter()


if not state.anonymous_mode:
    from authlib.integrations.starlette_client import OAuth, OAuthError
    from authlib.jose.errors import JoseError

    # Check for authentication configuration
    has_resend = bool(os.environ.get("RESEND_API_KEY"))

    # Register all configured OAuth providers
    oauth_configs = get_all_oauth_configs()

    if not has_resend and not oauth_configs:
        logger.error(
            "🚨 Start Khoj with --anonymous-mode flag or configure authentication:"
            "\n   - Set RESEND_API_KEY for magic link authentication, OR"
            "\n   - Configure OAuth (GENERIC_OAUTH_ENABLED, GENERIC_OAUTH_CLIENT_ID, GENERIC_OAUTH_CLIENT_SECRET)"
        )

    oauth = OAuth()

    for provider_name, provider_config in oauth_configs.items():
        # Only pass endpoint params that are actually set to avoid passing None as URL
        register_kwargs = {
            "name": provider_config["name"],
            "client_id": provider_config["client_id"],
            "client_secret": provider_config["client_secret"],
            "client_kwargs": {"scope": provider_config.get("scope", "openid profile email")},
        }
        for key in ("authorize_url", "access_token_url"):
            if provider_config.get(key):
                register_kwargs[key] = provider_config[key]

        # Pass pre-fetched OIDC metadata so Authlib can verify JWT tokens
        # without making HTTP requests at runtime
        if provider_config.get("server_metadata"):
            register_kwargs.update(provider_config["server_metadata"])

        logger.info(f"Registering OAuth provider '{provider_name}'")
        oauth.register(**register_kwargs)


def _patch_parse_id_token(oauth_client):
    """Patch parse_id_token to gracefully handle JWT verification failures.

    Authlib's authorize_access_token exchanges the auth code (consuming it),
    then tries to parse the ID token. If JWT parsing fails (e.g. unsupported_algorithm),
    the auth code is already consumed and we can't retry. This patch catches the error
    so authorize_access_token can still return the token, and we fall back to the
    userinfo endpoint for user details.
    """
    if getattr(oauth_client, "_parse_patched", False):
        return

    _orig_parse = oauth_client.parse_id_token

    async def _resilient_parse(token, nonce, **kw):
        try:
            return await _orig_parse(token, nonce, **kw)
        except (JoseError, ValueError) as e:
            logger.warning(f"ID token parse failed ({e}), falling back to userinfo endpoint")
            return None

    oauth_client.parse_id_token = _resilient_parse
    oauth_client._parse_patched = True


async def get_oauth_userinfo(oauth_client, token_data: dict) -> dict:
    """Get user info from OAuth token. Uses Authlib's built-in OIDC parsing and userinfo fetching."""
    # Authlib auto-parses the ID token for OIDC providers
    if token_data.get("userinfo"):
        return token_data["userinfo"]

    # Fetch from userinfo endpoint via Authlib
    try:
        user_info = await oauth_client.userinfo(token=token_data)
        if user_info:
            return dict(user_info)
    except Exception as e:
        logger.warning(f"Failed to fetch userinfo via Authlib: {e}")

    # Last resort: return raw token data
    return token_data


# Unified login endpoint - supports all providers
@auth_router.get("/login/{provider}")
async def login_provider(request: Request, provider: str):
    """Redirect to OAuth provider for authentication."""
    oauth_client = oauth.create_client(provider)
    if not oauth_client:
        logger.error(f"Unknown OAuth provider: {provider}")
        return RedirectResponse(url="/login?error=unknown_provider", status_code=HTTP_302_FOUND)

    base_url = str(request.base_url).rstrip("/")
    # Force HTTPS in redirect_uri when behind a TLS-terminating reverse proxy.
    # KHOJ_DOMAIN being set indicates a production deployment behind a proxy.
    if not DISABLE_HTTPS or os.environ.get("KHOJ_DOMAIN"):
        base_url = base_url.replace("http://", "https://")
    redirect_uri = f"{base_url}{request.app.url_path_for('oauth_callback', provider=provider)}"
    return await oauth_client.authorize_redirect(request, redirect_uri)


@auth_router.post("/login/{provider}")
async def login_provider_post(request: Request, provider: str):
    """POST variant for OAuth login."""
    return await login_provider(request, provider)


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


@auth_router.get("/logout")
async def logout(request: Request):
    request.session.pop("user", None)
    return RedirectResponse(url="/")


@auth_router.get("/oauth/metadata")
async def oauth_metadata(request: Request):
    """Return OAuth provider configuration for the frontend."""
    metadata = {}

    for provider_key, provider_config in oauth_configs.items():
        redirect_uri = str(request.app.url_path_for("oauth_callback", provider=provider_key))
        metadata[provider_key] = {
            "client_id": provider_config["client_id"],
            "redirect_uri": redirect_uri,
            "provider_name": provider_config.get("provider_name", "OAuth"),
            "button_label": provider_config.get("button_label"),
        }

    return metadata


# Unified OAuth callback handler
@auth_router.get("/oauth/{provider}")
async def oauth_callback(request: Request, provider: str):
    """Unified callback handler for all OAuth providers."""
    oauth_client = oauth.create_client(provider)
    if not oauth_client:
        logger.error(f"Unknown OAuth provider: {provider}")
        return RedirectResponse(url="/login?error=unknown_provider", status_code=HTTP_302_FOUND)

    next_url_path = get_next_url(request)

    # Make ID token parsing resilient — if JWT verification fails,
    # we fall back to the userinfo endpoint instead of losing the auth code.
    _patch_parse_id_token(oauth_client)

    try:
        token_data = await oauth_client.authorize_access_token(request)

        # Get user info from userinfo endpoint (OIDC standard)
        user_info = await get_oauth_userinfo(oauth_client, token_data)

        if not user_info or not user_info.get("email"):
            logger.error(f"Failed to get user info from {provider}: {user_info}")
            return RedirectResponse(url="/login?error=auth_failed", status_code=HTTP_302_FOUND)

        # Get or create user via unified adapter
        khoj_user = await get_or_create_user_oauth(provider, user_info)

        if not khoj_user:
            logger.error(f"Failed to create user from {provider} OAuth: {user_info.get('email')}")
            return RedirectResponse(url="/login?error=auth_failed", status_code=HTTP_302_FOUND)

        # Set session
        request.session["user"] = {
            "email": user_info["email"],
            "name": user_info.get("name"),
        }

        # Send welcome email for new users
        if datetime.timedelta(minutes=3) > (datetime.datetime.now(datetime.timezone.utc) - khoj_user.date_joined):
            asyncio.create_task(send_welcome_email(user_info.get("name", ""), user_info["email"]))
            update_telemetry_state(
                request=request,
                telemetry_type="api",
                api=f"create_user__oauth_{provider}",
                metadata={"server_id": str(khoj_user.uuid)},
            )
            logger.log(logging.INFO, f"🥳 New User Created via {provider}: {khoj_user.uuid}")

        return RedirectResponse(url=next_url_path, status_code=HTTP_302_FOUND)

    except OAuthError as error:
        logger.error(f"OAuth error for {provider}: {error}")
        return HTMLResponse(f"<h1>Authentication Error</h1><p>{error.error}</p>")
    except Exception as error:
        logger.error(f"Unexpected error in {provider} OAuth: {error}")
        return RedirectResponse(url="/login?error=auth_failed", status_code=HTTP_302_FOUND)
