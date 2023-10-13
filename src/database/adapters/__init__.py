from typing import Type, TypeVar, List
import uuid
from datetime import date

from django.db import models
from django.contrib.sessions.backends.db import SessionStore
from pgvector.django import CosineDistance
from django.db.models.manager import BaseManager
from django.db.models import Q
from torch import Tensor

# Import sync_to_async from Django Channels
from asgiref.sync import sync_to_async

from fastapi import HTTPException

from database.models import (
    KhojUser,
    GoogleUser,
    NotionConfig,
    GithubConfig,
    Embeddings,
    GithubRepoConfig,
    EmbeddingsDates,
)
from khoj.search_filter.word_filter import WordFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.date_filter import DateFilter

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
    word_filer = WordFilter()
    file_filter = FileFilter()
    date_filter = DateFilter()

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
    def get_embeddings_by_date_filter(embeddings: BaseManager[Embeddings], start_date: date, end_date: date):
        return embeddings.filter(
            embeddingsdates__date__gte=start_date,
            embeddingsdates__date__lte=end_date,
        )

    @staticmethod
    def apply_filters(user: KhojUser, query: str, file_type_filter: str = None):
        q_filter_terms = Q()

        explicit_word_terms = EmbeddingsAdapters.word_filer.get_filter_terms(query)
        file_filters = EmbeddingsAdapters.file_filter.get_filter_terms(query)
        date_filters = EmbeddingsAdapters.date_filter.get_query_date_range(query)

        if len(explicit_word_terms) == 0 and len(file_filters) == 0 and len(date_filters) == 0:
            return Embeddings.objects.filter(user=user)

        for term in explicit_word_terms:
            if term.startswith("+"):
                q_filter_terms &= Q(raw__icontains=term[1:])
            elif term.startswith("-"):
                q_filter_terms &= ~Q(raw__icontains=term[1:])

        # if len(file_filters) > 0:
        #     q_filter_terms &= Q(file_name__regex=f"({'|'.join(file_filters)})")

        q_file_filter_terms = Q()

        if len(file_filters) > 0:
            for term in file_filters:
                # Escape the * character in the file filter term
                # term = term.replace("*", r"\*")
                q_file_filter_terms |= Q(file_path__regex=term)

            q_filter_terms &= q_file_filter_terms

        if len(date_filters) > 0:
            min_date, max_date = date_filters
            if min_date is not None:
                # Convert the min_date timestamp to yyyy-mm-dd format
                formatted_min_date = date.fromtimestamp(min_date).strftime("%Y-%m-%d")
                q_filter_terms &= Q(embeddings_dates__date__gte=formatted_min_date)
            if max_date is not None:
                # Convert the max_date timestamp to yyyy-mm-dd format
                formatted_max_date = date.fromtimestamp(max_date).strftime("%Y-%m-%d")
                q_filter_terms &= Q(embeddings_dates__date__lte=formatted_max_date)

        relevant_embeddings = Embeddings.objects.filter(user=user).filter(
            q_filter_terms,
        )
        if file_type_filter:
            relevant_embeddings = relevant_embeddings.filter(file_type=file_type_filter)
        return relevant_embeddings

    @staticmethod
    def search_with_embeddings(
        user: KhojUser, embeddings: Tensor, max_results: int = 10, file_type_filter: str = None, raw_query: str = None
    ):
        relevant_embeddings = EmbeddingsAdapters.apply_filters(user, raw_query, file_type_filter)
        relevant_embeddings = relevant_embeddings.filter(user=user).annotate(
            distance=CosineDistance("embeddings", embeddings)
        )
        if file_type_filter:
            relevant_embeddings = relevant_embeddings.filter(file_type=file_type_filter)

        # results = relevant_embeddings.values("corpus_id").annotate(distance=models.Min("distance")).order_by("distance").values("corpus_id", "raw", "file_path", "compiled", "heading", "id")[:max_results]
        # results = (
        #     relevant_embeddings.values("corpus_id")
        #     .annotate(distance=models.Min("distance"))
        #     .order_by("distance")[:max_results]
        # )
        # target_embeddings = relevant_embeddings.order_by("distance")[:50]
        # results = Embeddings.objects.filter(id__in=target_embeddings).distinct("corpus_id")
        # # relevant_embeddings = relevant_embeddings.order_by("distance", "corpus_id").distinct("distance", "corpus_id")
        relevant_embeddings = relevant_embeddings.order_by("distance")
        # return results
        return relevant_embeddings[:max_results]

    @staticmethod
    def get_unique_file_types(user: KhojUser):
        return Embeddings.objects.filter(user=user).values_list("file_type", flat=True).distinct()
