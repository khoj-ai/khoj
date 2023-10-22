import secrets
from typing import Type, TypeVar, List
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
    KhojApiUser,
    NotionConfig,
    GithubConfig,
    Embeddings,
    GithubRepoConfig,
    Conversation,
    ConversationProcessorConfig,
    OpenAIProcessorConversationConfig,
    OfflineChatProcessorConversationConfig,
)
from khoj.utils.rawconfig import (
    ConversationProcessorConfig as UserConversationProcessorConfig,
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


async def create_khoj_token(user: KhojUser, name="Secret Key"):
    "Create Khoj API key for user"
    token = f"kk-{secrets.token_urlsafe(32)}"
    api_config = await KhojApiUser.objects.acreate(token=token, user=user, name=name)
    await api_config.asave()
    return api_config


def get_khoj_tokens(user: KhojUser):
    "Get all Khoj API keys for user"
    return list(KhojApiUser.objects.filter(user=user))


async def delete_khoj_token(user: KhojUser, token: str):
    "Delete Khoj API Key for user"
    await KhojApiUser.objects.filter(token=token, user=user).adelete()


async def get_or_create_user(token: dict) -> KhojUser:
    user = await get_user_by_token(token)
    if not user:
        user = await create_google_user(token)
    return user


async def create_google_user(token: dict) -> KhojUser:
    user = await KhojUser.objects.acreate(username=token.get("email"), email=token.get("email"))
    await user.asave()
    await GoogleUser.objects.acreate(
        sub=token.get("sub"),
        azp=token.get("azp"),
        email=token.get("email"),
        name=token.get("name"),
        given_name=token.get("given_name"),
        family_name=token.get("family_name"),
        picture=token.get("picture"),
        locale=token.get("locale"),
        user=user,
    )

    return user


async def get_user_by_token(token: dict) -> KhojUser:
    google_user = await GoogleUser.objects.filter(sub=token.get("sub")).select_related("user").afirst()
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


def get_all_users() -> BaseManager[KhojUser]:
    return KhojUser.objects.all()


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


class ConversationAdapters:
    @staticmethod
    def get_conversation_by_user(user: KhojUser):
        conversation = Conversation.objects.filter(user=user)
        if conversation.exists():
            return conversation.first()
        return Conversation.objects.create(user=user)

    @staticmethod
    async def aget_conversation_by_user(user: KhojUser):
        conversation = Conversation.objects.filter(user=user)
        if await conversation.aexists():
            return await conversation.afirst()
        return await Conversation.objects.acreate(user=user)

    @staticmethod
    def has_any_conversation_config(user: KhojUser):
        return ConversationProcessorConfig.objects.filter(user=user).exists()

    @staticmethod
    def get_openai_conversation_config(user: KhojUser):
        return OpenAIProcessorConversationConfig.objects.filter(user=user).first()

    @staticmethod
    def get_offline_chat_conversation_config(user: KhojUser):
        return OfflineChatProcessorConversationConfig.objects.filter(user=user).first()

    @staticmethod
    def has_valid_offline_conversation_config(user: KhojUser):
        return OfflineChatProcessorConversationConfig.objects.filter(user=user, enable_offline_chat=True).exists()

    @staticmethod
    def has_valid_openai_conversation_config(user: KhojUser):
        return OpenAIProcessorConversationConfig.objects.filter(user=user).exists()

    @staticmethod
    def get_conversation_config(user: KhojUser):
        return ConversationProcessorConfig.objects.filter(user=user).first()

    @staticmethod
    def save_conversation(user: KhojUser, conversation_log: dict):
        conversation = Conversation.objects.filter(user=user)
        if conversation.exists():
            conversation.update(conversation_log=conversation_log)
        else:
            Conversation.objects.create(user=user, conversation_log=conversation_log)

    @staticmethod
    def set_conversation_processor_config(user: KhojUser, new_config: UserConversationProcessorConfig):
        conversation_config, _ = ConversationProcessorConfig.objects.get_or_create(user=user)
        conversation_config.max_prompt_size = new_config.max_prompt_size
        conversation_config.tokenizer = new_config.tokenizer
        conversation_config.save()

        if new_config.openai:
            default_values = {
                "api_key": new_config.openai.api_key,
            }
            if new_config.openai.chat_model:
                default_values["chat_model"] = new_config.openai.chat_model

            OpenAIProcessorConversationConfig.objects.update_or_create(user=user, defaults=default_values)

        if new_config.offline_chat:
            default_values = {
                "enable_offline_chat": str(new_config.offline_chat.enable_offline_chat),
            }

            if new_config.offline_chat.chat_model:
                default_values["chat_model"] = new_config.offline_chat.chat_model

            OfflineChatProcessorConversationConfig.objects.update_or_create(user=user, defaults=default_values)

    @staticmethod
    def get_enabled_conversation_settings(user: KhojUser):
        openai_config = ConversationAdapters.get_openai_conversation_config(user)
        offline_chat_config = ConversationAdapters.get_offline_chat_conversation_config(user)

        return {
            "openai": True if openai_config is not None else False,
            "offline_chat": True
            if (offline_chat_config is not None and offline_chat_config.enable_offline_chat)
            else False,
        }

    @staticmethod
    def clear_conversation_config(user: KhojUser):
        ConversationProcessorConfig.objects.filter(user=user).delete()
        ConversationAdapters.clear_openai_conversation_config(user)
        ConversationAdapters.clear_offline_chat_conversation_config(user)

    @staticmethod
    def clear_openai_conversation_config(user: KhojUser):
        OpenAIProcessorConversationConfig.objects.filter(user=user).delete()

    @staticmethod
    def clear_offline_chat_conversation_config(user: KhojUser):
        OfflineChatProcessorConversationConfig.objects.filter(user=user).delete()

    @staticmethod
    async def has_offline_chat(user: KhojUser):
        return await OfflineChatProcessorConversationConfig.objects.filter(
            user=user, enable_offline_chat=True
        ).aexists()

    @staticmethod
    async def get_offline_chat(user: KhojUser):
        return await OfflineChatProcessorConversationConfig.objects.filter(user=user).afirst()

    @staticmethod
    async def has_openai_chat(user: KhojUser):
        return await OpenAIProcessorConversationConfig.objects.filter(user=user).aexists()

    @staticmethod
    async def get_openai_chat(user: KhojUser):
        return await OpenAIProcessorConversationConfig.objects.filter(user=user).afirst()


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
    async def user_has_embeddings(user: KhojUser):
        return await Embeddings.objects.filter(user=user).aexists()

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

        q_file_filter_terms = Q()

        if len(file_filters) > 0:
            for term in file_filters:
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
        relevant_embeddings = relevant_embeddings.order_by("distance")
        return relevant_embeddings[:max_results]

    @staticmethod
    def get_unique_file_types(user: KhojUser):
        return Embeddings.objects.filter(user=user).values_list("file_type", flat=True).distinct()
