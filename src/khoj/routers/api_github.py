import logging
import os
import secrets
from typing import Optional

import httpx
from fastapi import APIRouter, BackgroundTasks, Request
from fastapi.responses import JSONResponse, RedirectResponse
from starlette.authentication import requires

from khoj.database import adapters
from khoj.database.models import GithubConfig, GithubRepoConfig, KhojUser
from khoj.processor.content.github.github_to_entries import GithubToEntries

github_router = APIRouter()
logger = logging.getLogger(__name__)

# Replace these with your GitHub OAuth app credentials
GITHUB_CLIENT_ID = os.getenv("GITHUB_CLIENT_ID")
GITHUB_CLIENT_SECRET = os.getenv("GITHUB_CLIENT_SECRET")
GITHUB_REDIRECT_URI = os.getenv("GITHUB_REDIRECT_URI")

# In-memory store for testing (use a database in production)
oauth_state_store = {}


def save_oauth_state(state: str, user: KhojUser) -> None:
    oauth_state_store[state] = user  # Store the state and user mapping


def get_user_id_by_oauth_state(state: str) -> Optional[KhojUser]:
    return oauth_state_store.pop(state, None)  # Remove the state after use


def index_github(user: KhojUser):
    config = GithubConfig.objects.filter(user=user).first()
    if config:
        GithubToEntries(config).process(files={}, user=user, regenerate=False)
        logger.info(f"Github entries indexed for user {user.id}")


@github_router.get("/connect")
@requires(["authenticated"])
async def connect_github(request: Request):
    """
    Redirect the user to GitHub's OAuth authorization page.
    """
    user = request.user
    if not user.is_authenticated:
        return JSONResponse(content={"error": "User not authenticated"}, status_code=401)

    # Generate a unique state value
    state = secrets.token_urlsafe(16)

    # Save the state and user ID mapping (e.g., in a database or in-memory store)
    save_oauth_state(state, user)  # Implement this function to store the mapping

    github_oauth_url = (
        f"https://github.com/login/oauth/authorize"
        f"?client_id={GITHUB_CLIENT_ID}&redirect_uri={GITHUB_REDIRECT_URI}&scope=repo,user"
        f"&state={state}"
    )
    return RedirectResponse(url=github_oauth_url)


@github_router.get("/callback")
async def github_callback(request: Request):
    code = request.query_params.get("code")
    state = request.query_params.get("state")

    if not code or not state:
        logger.error("Missing code or state in GitHub callback")
        return RedirectResponse(url="/settings")

    user = get_user_id_by_oauth_state(state)
    if not user:
        logger.error("Invalid or expired OAuth state")
        return RedirectResponse(url="/settings")

    if not user or not hasattr(user, "object"):
        logger.error("OAuth state returned invalid user")
        return RedirectResponse(url="/settings")

    try:
        async with httpx.AsyncClient() as client:
            response = await client.post(
                "https://github.com/login/oauth/access_token",
                headers={"Accept": "application/json"},
                data={
                    "client_id": GITHUB_CLIENT_ID,
                    "client_secret": GITHUB_CLIENT_SECRET,
                    "code": code,
                    "redirect_uri": GITHUB_REDIRECT_URI,
                    "state": state,
                },
            )

            if response.status_code != 200:
                logger.error(f"GitHub token exchange failed: {response.text}")
                return RedirectResponse(url="/settings")

            token_data = response.json()
            access_token = token_data.get("access_token")
            if not access_token:
                logger.error("No access token returned from GitHub")
                return RedirectResponse(url="/settings")

    except Exception as e:
        logger.exception("Exception during GitHub token exchange")
        return RedirectResponse(url="/settings")

    try:
        # Save the GitHub access token
        config = await adapters.GithubConfig.objects.filter(user=user.object).afirst()
        if not config:
            config = await adapters.GithubConfig.objects.acreate(pat_token=access_token, user=user.object)
        else:
            config.pat_token = access_token
            await config.asave()
            await config.githubrepoconfig.all().adelete()

        logger.info(f"GitHub integration successfully set up for user {user.object.id}")
        settings_redirect = str(request.app.url_path_for("config_page"))

        logger.info(f"Redirecting to Settings config page: {settings_redirect}")

        return RedirectResponse(settings_redirect + "?github_connected=true")

    except Exception as e:
        logger.exception("Failed to save GitHub configuration")
        return RedirectResponse(url="/settings")


@github_router.get("/repos")
@requires(["authenticated"])
async def list_user_repos(request: Request):
    user = request.user
    if not user.is_authenticated:
        return JSONResponse({"error": "Not authenticated"}, status_code=401)

    config = await GithubConfig.objects.filter(user=user.object).prefetch_related("githubrepoconfig").afirst()
    if not config:
        return JSONResponse({"error": "GitHub not connected"}, status_code=400)

    logger.debug(f"GitHub config for user {user.object.id}: config: {config.id}")

    raw_repos = config.githubrepoconfig.all()
    selected_repos = []
    for repo in raw_repos:
        selected_repos.append(repo.owner + "/" + repo.name)
    logger.debug(f"Repos from DB: {selected_repos}")

    headers = {"Authorization": f"token {config.pat_token}"}
    async with httpx.AsyncClient() as client:
        response = await client.get("https://api.github.com/user/repos", headers=headers)

    if response.status_code != 200:
        return JSONResponse(
            {"error": "Failed to fetch repos", "detail": response.text}, status_code=response.status_code
        )

    repos = response.json()
    return [
        {
            "name": r["name"],
            "owner": r["owner"]["login"],
            "branch": r["default_branch"],
            "full_name": r["full_name"],
            "description": r.get("description"),
            "private": r.get("private", False),
            "selected": r["full_name"] in selected_repos,  # ✅ new flag
        }
        for r in repos
    ]


@github_router.post("/repos/select")
@requires(["authenticated"])
async def select_user_repos(request: Request, background_tasks: BackgroundTasks):
    user = request.user
    if not user.is_authenticated:
        return JSONResponse({"error": "Not authenticated"}, status_code=401)

    body = await request.json()
    repos = body.get("repos", [])
    if not repos:
        return JSONResponse({"error": "No repositories provided"}, status_code=400)

    config = await GithubConfig.objects.filter(user=user.object).afirst()
    if not config:
        return JSONResponse({"error": "GitHub not connected"}, status_code=400)

    await config.githubrepoconfig.all().adelete()  # clear old selections

    for repo in repos:
        await GithubRepoConfig.objects.acreate(
            name=repo["name"], owner=repo["owner"], branch=repo["branch"], github_config=config
        )

    # Trigger an async job to index_github. Let it run without blocking the response.
    background_tasks.add_task(index_github, user.object)

    return {"status": "success", "count": len(repos)}


@github_router.delete("/disconnect")
@requires(["authenticated"])
async def disconnect_github(request: Request):
    """
    Disconnect the GitHub integration for the authenticated user.
    """
    user = request.user
    if not user.is_authenticated:
        return JSONResponse(content={"error": "User not authenticated"}, status_code=401)

    # Delete the GitHub configuration for the user
    await GithubConfig.objects.filter(user=user.object).adelete()

    logger.info(f"GitHub integration successfully set up for user {user.object.id}")
    settings_redirect = str(request.app.url_path_for("config_page"))

    return RedirectResponse(settings_redirect + "?github_connected=false")
