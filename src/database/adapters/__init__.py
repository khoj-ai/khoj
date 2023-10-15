from typing import Type, TypeVar
import uuid

from django.db import models
from django.contrib.sessions.backends.db import SessionStore

# Import sync_to_async from Django Channels
from asgiref.sync import sync_to_async

from fastapi import HTTPException

from database.models import KhojUser, GoogleUser, NotionConfig

ModelType = TypeVar("ModelType", bound=models.Model)


async def retrieve_object(model_class: Type[ModelType], id: int) -> ModelType:
    instance = await model_class.objects.filter(id=id).afirst()
    if not instance:
        raise HTTPException(status_code=404, detail=f"{model_class.__name__} not found")
    return instance


async def set_notion_config(token: str, user: KhojUser):
    notion_config = await NotionConfig.objects.filter(user=user).afirst()
    if not notion_config:
        notion_config = await NotionConfig.objects.acreate(token=token, user=user)
    else:
        notion_config.token = token
        await notion_config.asave()
    return notion_config


async def get_or_create_user(token: dict) -> KhojUser:
    user = await get_user_by_token(token)
    if not user:
        user = await create_google_user(token)
    return user


async def create_google_user(token: dict) -> KhojUser:
    user_info = token.get("userinfo")
    user = await KhojUser.objects.acreate(
        username=user_info.get("email"), email=user_info.get("email"), uuid=uuid.uuid4()
    )
    await user.asave()
    await GoogleUser.objects.acreate(
        sub=user_info.get("sub"),
        azp=user_info.get("azp"),
        email=user_info.get("email"),
        name=user_info.get("name"),
        given_name=user_info.get("given_name"),
        family_name=user_info.get("family_name"),
        picture=user_info.get("picture"),
        locale=user_info.get("locale"),
        user=user,
    )

    return user


async def get_user_by_token(token: dict) -> KhojUser:
    user_info = token.get("userinfo")
    google_user = await GoogleUser.objects.filter(sub=user_info.get("sub")).select_related("user").afirst()
    if not google_user:
        return None
    return google_user.user


async def retrieve_user(session_id: str) -> KhojUser:
    session = SessionStore(session_key=session_id)
    if not await sync_to_async(session.exists)(session_key=session_id):
        raise HTTPException(status_code=401, detail="Invalid session")
    session_data = await sync_to_async(session.load)()
    user = await KhojUser.objects.filter(id=session_data.get("_auth_user_id")).afirst()
    if not user:
        raise HTTPException(status_code=401, detail="Invalid user")
    return user
