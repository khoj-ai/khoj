from typing import Type, TypeVar, List
import uuid

from django.db import models
from django.contrib.sessions.backends.db import SessionStore
from pgvector.django import CosineDistance
from torch import Tensor

# Import sync_to_async from Django Channels
from asgiref.sync import sync_to_async

from fastapi import HTTPException

from database.models import KhojUser, GoogleUser, NotionConfig, GithubConfig, Embeddings, GithubRepoConfig

from khoj.utils.constants import content_directory

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
    user = await KhojUser.objects.acreate(username=user_info.get("email"), email=user_info.get("email"))
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


def get_user_github_config(user: KhojUser):
    config = GithubConfig.objects.filter(user=user).prefetch_related("githubrepoconfig").first()
    if not config:
        return None
    return config


def get_user_notion_config(user: KhojUser):
    config = NotionConfig.objects.filter(user=user).first()
    if not config:
        return None
    return config


async def set_text_content_config(user: KhojUser, object: Type[models.Model], updated_config):
    deduped_files = list(set(updated_config.input_files)) if updated_config.input_files else None
    deduped_filters = list(set(updated_config.input_filter)) if updated_config.input_filter else None
    await object.objects.filter(user=user).adelete()
    await object.objects.acreate(
        input_files=deduped_files,
        input_filter=deduped_filters,
        index_heading_entries=updated_config.index_heading_entries,
        user=user,
    )


async def set_user_github_config(user: KhojUser, pat_token: str, repos: list):
    config = await GithubConfig.objects.filter(user=user).afirst()

    if not config:
        config = await GithubConfig.objects.acreate(pat_token=pat_token, user=user)
    else:
        config.pat_token = pat_token
        await config.asave()
        await config.githubrepoconfig.all().adelete()

    for repo in repos:
        await GithubRepoConfig.objects.acreate(
            name=repo["name"], owner=repo["owner"], branch=repo["branch"], github_config=config
        )
    return config


class EmbeddingsAdapters:
    @staticmethod
    def does_embedding_exist(user: KhojUser, hashed_value: str) -> bool:
        return Embeddings.objects.filter(user=user, hashed_value=hashed_value).exists()

    @staticmethod
    def delete_embedding_by_file(user: KhojUser, file_path: str):
        deleted_count, _ = Embeddings.objects.filter(user=user, file_path=file_path).delete()
        return deleted_count

    @staticmethod
    def delete_all_embeddings(user: KhojUser, file_type: str):
        deleted_count, _ = Embeddings.objects.filter(user=user, file_type=file_type).delete()
        return deleted_count

    @staticmethod
    def get_existing_entry_hashes_by_file(user: KhojUser, file_path: str):
        return Embeddings.objects.filter(user=user, file_path=file_path).values_list("hashed_value", flat=True)

    @staticmethod
    def delete_embedding_by_hash(user: KhojUser, hashed_values: List[str]):
        Embeddings.objects.filter(user=user, hashed_value__in=hashed_values).delete()

    @staticmethod
    def search_with_embeddings(user: KhojUser, embeddings: Tensor, max_results: int = 10, file_filter: str = None):
        relevant_embeddings = Embeddings.objects.filter(user=user).annotate(
            distance=CosineDistance("embeddings", embeddings)
        )
        if file_filter:
            relevant_embeddings = relevant_embeddings.filter(file_type=file_filter)
        return relevant_embeddings.order_by("distance")[:max_results]

    @staticmethod
    def get_unique_file_types(user: KhojUser):
        return Embeddings.objects.filter(user=user).values_list("file_type", flat=True).distinct()
