import asyncio
import base64
import json
import os

import requests
from fastapi import APIRouter, Request, Response
from starlette.responses import RedirectResponse

from khoj.database.adapters import get_user_by_uuid
from khoj.database.models import KhojUser, NotionConfig
from khoj.routers.indexer import configure_content

NOTION_OAUTH_CLIENT_ID = os.getenv("NOTION_OAUTH_CLIENT_ID")
NOTION_OAUTH_CLIENT_SECRET = os.getenv("NOTION_OAUTH_CLIENT_SECRET")
NOTION_REDIRECT_URI = os.getenv("NOTION_REDIRECT_URI")

notion_router = APIRouter()


def get_notion_auth_url(user: KhojUser):
    if not NOTION_OAUTH_CLIENT_ID or not NOTION_OAUTH_CLIENT_SECRET or not NOTION_REDIRECT_URI:
        return None
    return f"https://api.notion.com/v1/oauth/authorize?client_id={NOTION_OAUTH_CLIENT_ID}&redirect_uri={NOTION_REDIRECT_URI}&response_type=code&state={user.uuid}"


@notion_router.get("/auth/callback")
def notion_auth_callback(request: Request):
    code = request.query_params.get("code")
    state = request.query_params.get("state")
    if not code or not state:
        return Response("Missing code or state", status_code=400)

    user: KhojUser = get_user_by_uuid(state)

    NotionConfig.objects.filter(user=user).delete()

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

    access_token = final_response.get("access_token")
    NotionConfig.objects.create(token=access_token, user=user)

    owner = final_response.get("owner")
    workspace_id = final_response.get("workspace_id")
    workspace_name = final_response.get("workspace_name")
    bot_id = final_response.get("bot_id")

    notion_redirect = str(request.app.url_path_for("notion_config_page"))

    # Trigger an async job to configure_content. Let it run without blocking the response.
    asyncio.create_task(configure_content(user, "notion"))

    return RedirectResponse(notion_redirect)
