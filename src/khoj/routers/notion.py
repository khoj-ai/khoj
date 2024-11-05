import asyncio
import base64
import json
import logging
import os
from concurrent.futures import ThreadPoolExecutor

import requests
from fastapi import APIRouter, BackgroundTasks, Request, Response
from starlette.responses import RedirectResponse

from khoj.database.adapters import aget_user_by_uuid
from khoj.database.models import KhojUser, NotionConfig
from khoj.routers.helpers import configure_content
from khoj.utils.state import SearchType

NOTION_OAUTH_CLIENT_ID = os.getenv("NOTION_OAUTH_CLIENT_ID")
NOTION_OAUTH_CLIENT_SECRET = os.getenv("NOTION_OAUTH_CLIENT_SECRET")
NOTION_REDIRECT_URI = os.getenv("NOTION_REDIRECT_URI")

notion_router = APIRouter()

executor = ThreadPoolExecutor()

logger = logging.getLogger(__name__)


async def run_in_executor(func, *args):
    loop = asyncio.get_event_loop()
    return await loop.run_in_executor(executor, func, *args)


@notion_router.get("/auth/callback")
async def notion_auth_callback(request: Request, background_tasks: BackgroundTasks):
    code = request.query_params.get("code")
    state = request.query_params.get("state")
    if not code or not state:
        return Response("Missing code or state", status_code=400)

    user: KhojUser = await aget_user_by_uuid(state)

    await NotionConfig.objects.filter(user=user).adelete()

    if not user:
        raise Exception("User not found")

    bearer_token = f"{NOTION_OAUTH_CLIENT_ID}:{NOTION_OAUTH_CLIENT_SECRET}"
    base64_encoded_token = base64.b64encode(bearer_token.encode()).decode()

    headers = {
        "Accept": "application/json",
        "Content-Type": "application/json",
        "Authorization": f"Basic {base64_encoded_token}",
    }

    data = {
        "grant_type": "authorization_code",
        "code": code,
        "redirect_uri": NOTION_REDIRECT_URI,
    }

    response = requests.post("https://api.notion.com/v1/oauth/token", data=json.dumps(data), headers=headers)

    final_response = response.json()

    logger.info(f"Notion auth callback response: {final_response}")

    access_token = final_response.get("access_token")
    await NotionConfig.objects.acreate(token=access_token, user=user)

    owner = final_response.get("owner")
    workspace_id = final_response.get("workspace_id")
    workspace_name = final_response.get("workspace_name")
    bot_id = final_response.get("bot_id")

    logger.info(
        f"Notion integration. Owner: {owner}, Workspace ID: {workspace_id}, Workspace Name: {workspace_name}, Bot ID: {bot_id}"
    )

    notion_redirect = str(request.app.url_path_for("config_page"))

    # Trigger an async job to configure_content. Let it run without blocking the response.
    background_tasks.add_task(run_in_executor, configure_content, user, {}, False, SearchType.Notion)

    return RedirectResponse(notion_redirect)
