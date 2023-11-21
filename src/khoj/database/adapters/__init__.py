import math
from typing import Optional, Type, List
from datetime import date, datetime
import secrets
from typing import Type, List
from datetime import date, timezone

from django.db import models
from django.contrib.sessions.backends.db import SessionStore
from pgvector.django import CosineDistance
from django.db.models.manager import BaseManager
from django.db.models import Q
from torch import Tensor

# Import sync_to_async from Django Channels
from asgiref.sync import sync_to_async

from fastapi import HTTPException

from khoj.database.models import (
    KhojUser,
    GoogleUser,
    KhojApiUser,
    NotionConfig,
    GithubConfig,
    Entry,
    GithubRepoConfig,
    Conversation,
    ChatModelOptions,
    SearchModelConfig,
    Subscription,
    UserConversationConfig,
    OpenAIProcessorConversationConfig,
    OfflineChatProcessorConversationConfig,
)
from khoj.utils.helpers import generate_random_name
from khoj.search_filter.word_filter import WordFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.date_filter import DateFilter


async def set_notion_config(token: str, user: KhojUser):
    notion_config = await NotionConfig.objects.filter(user=user).afirst()
    if not notion_config:
        notion_config = await NotionConfig.objects.acreate(token=token, user=user)
    else:
        notion_config.token = token
        await notion_config.asave()
    return notion_config


async def create_khoj_token(user: KhojUser, name=None):
    "Create Khoj API key for user"
    token = f"kk-{secrets.token_urlsafe(32)}"
    name = name or f"{generate_random_name().title()}"
    return await KhojApiUser.objects.acreate(token=token, user=user, name=name)


def get_khoj_tokens(user: KhojUser):
    "Get all Khoj API keys for user"
    return list(KhojApiUser.objects.filter(user=user))


async def delete_khoj_token(user: KhojUser, token: str):
    "Delete Khoj API Key for user"
    await KhojApiUser.objects.filter(token=token, user=user).adelete()


async def get_or_create_user(token: dict) -> KhojUser:
    user = await get_user_by_token(token)
    if not user:
        user = await create_user_by_google_token(token)
    return user


async def create_user_by_google_token(token: dict) -> KhojUser:
    user, _ = await KhojUser.objects.filter(email=token.get("email")).aupdate_or_create(
        defaults={"username": token.get("email"), "email": token.get("email")}
    )
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

    await Subscription.objects.acreate(user=user, type="trial")

    return user


def get_user_subscription(email: str) -> Optional[Subscription]:
    return Subscription.objects.filter(user__email=email).first()


async def set_user_subscription(
    email: str, is_recurring=None, renewal_date=None, type="standard"
) -> Optional[Subscription]:
    user_subscription = await Subscription.objects.filter(user__email=email).afirst()
    if not user_subscription:
        user = await get_user_by_email(email)
        if not user:
            return None
        user_subscription = await Subscription.objects.acreate(
            user=user, type=type, is_recurring=is_recurring, renewal_date=renewal_date
        )
        return user_subscription
    elif user_subscription:
        user_subscription.type = type
        if is_recurring is not None:
            user_subscription.is_recurring = is_recurring
        if renewal_date is False:
            user_subscription.renewal_date = None
        elif renewal_date is not None:
            user_subscription.renewal_date = renewal_date
        await user_subscription.asave()
        return user_subscription
    else:
        return None


def get_user_subscription_state(email: str) -> str:
    """Get subscription state of user
    Valid state transitions: trial -> subscribed <-> unsubscribed OR expired
    """
    user_subscription = Subscription.objects.filter(user__email=email).first()
    if not user_subscription:
        return "trial"
    elif user_subscription.type == Subscription.Type.TRIAL:
        return "trial"
    elif user_subscription.is_recurring and user_subscription.renewal_date >= datetime.now(tz=timezone.utc):
        return "subscribed"
    elif not user_subscription.is_recurring and user_subscription.renewal_date >= datetime.now(tz=timezone.utc):
        return "unsubscribed"
    elif not user_subscription.is_recurring and user_subscription.renewal_date < datetime.now(tz=timezone.utc):
        return "expired"
    return "invalid"


async def get_user_by_email(email: str) -> KhojUser:
    return await KhojUser.objects.filter(email=email).afirst()


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
    return config


def get_user_notion_config(user: KhojUser):
    config = NotionConfig.objects.filter(user=user).first()
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


def get_or_create_search_model():
    search_model = SearchModelConfig.objects.filter().first()
    if not search_model:
        search_model = SearchModelConfig.objects.create()

    return search_model


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
        return ChatModelOptions.objects.filter(user=user).exists()

    @staticmethod
    def get_openai_conversation_config():
        return OpenAIProcessorConversationConfig.objects.filter().first()

    @staticmethod
    async def aget_openai_conversation_config():
        return await OpenAIProcessorConversationConfig.objects.filter().afirst()

    @staticmethod
    def get_offline_chat_conversation_config():
        return OfflineChatProcessorConversationConfig.objects.filter().first()

    @staticmethod
    async def aget_offline_chat_conversation_config():
        return await OfflineChatProcessorConversationConfig.objects.filter().afirst()

    @staticmethod
    def has_valid_offline_conversation_config():
        return OfflineChatProcessorConversationConfig.objects.filter(enabled=True).exists()

    @staticmethod
    def has_valid_openai_conversation_config():
        return OpenAIProcessorConversationConfig.objects.filter().exists()

    @staticmethod
    async def aset_user_conversation_processor(user: KhojUser, conversation_processor_config_id: int):
        config = await ChatModelOptions.objects.filter(id=conversation_processor_config_id).afirst()
        if not config:
            return None
        new_config = await UserConversationConfig.objects.aupdate_or_create(user=user, defaults={"setting": config})
        return new_config

    @staticmethod
    def get_conversation_config(user: KhojUser):
        config = UserConversationConfig.objects.filter(user=user).first()
        if not config:
            return None
        return config.setting

    @staticmethod
    async def aget_conversation_config(user: KhojUser):
        config = await UserConversationConfig.objects.filter(user=user).prefetch_related("setting").afirst()
        if not config:
            return None
        return config.setting

    @staticmethod
    def get_default_conversation_config():
        return ChatModelOptions.objects.filter().first()

    @staticmethod
    async def aget_default_conversation_config():
        return await ChatModelOptions.objects.filter().afirst()

    @staticmethod
    def save_conversation(user: KhojUser, conversation_log: dict):
        conversation = Conversation.objects.filter(user=user)
        if conversation.exists():
            conversation.update(conversation_log=conversation_log)
        else:
            Conversation.objects.create(user=user, conversation_log=conversation_log)

    @staticmethod
    def get_conversation_processor_options():
        return ChatModelOptions.objects.all()

    @staticmethod
    def set_conversation_processor_config(user: KhojUser, new_config: ChatModelOptions):
        user_conversation_config, _ = UserConversationConfig.objects.get_or_create(user=user)
        user_conversation_config.setting = new_config
        user_conversation_config.save()

    @staticmethod
    def has_offline_chat():
        return OfflineChatProcessorConversationConfig.objects.filter(enabled=True).exists()

    @staticmethod
    async def ahas_offline_chat():
        return await OfflineChatProcessorConversationConfig.objects.filter(enabled=True).aexists()

    @staticmethod
    async def get_offline_chat():
        return await ChatModelOptions.objects.filter(model_type="offline").afirst()

    @staticmethod
    async def aget_user_conversation_config(user: KhojUser):
        config = await UserConversationConfig.objects.filter(user=user).prefetch_related("setting").afirst()
        if not config:
            return None
        return config.setting

    @staticmethod
    async def has_openai_chat():
        return await OpenAIProcessorConversationConfig.objects.filter().aexists()

    @staticmethod
    async def get_openai_chat():
        return await ChatModelOptions.objects.filter(model_type="openai").afirst()

    @staticmethod
    async def get_openai_chat_config():
        return await OpenAIProcessorConversationConfig.objects.filter().afirst()


class EntryAdapters:
    word_filer = WordFilter()
    file_filter = FileFilter()
    date_filter = DateFilter()

    @staticmethod
    def does_entry_exist(user: KhojUser, hashed_value: str) -> bool:
        return Entry.objects.filter(user=user, hashed_value=hashed_value).exists()

    @staticmethod
    def delete_entry_by_file(user: KhojUser, file_path: str):
        deleted_count, _ = Entry.objects.filter(user=user, file_path=file_path).delete()
        return deleted_count

    @staticmethod
    def delete_all_entries_by_type(user: KhojUser, file_type: str = None):
        if file_type is None:
            deleted_count, _ = Entry.objects.filter(user=user).delete()
        else:
            deleted_count, _ = Entry.objects.filter(user=user, file_type=file_type).delete()
        return deleted_count

    @staticmethod
    def delete_all_entries(user: KhojUser, file_source: str = None):
        if file_source is None:
            deleted_count, _ = Entry.objects.filter(user=user).delete()
        else:
            deleted_count, _ = Entry.objects.filter(user=user, file_source=file_source).delete()
        return deleted_count

    @staticmethod
    def get_existing_entry_hashes_by_file(user: KhojUser, file_path: str):
        return Entry.objects.filter(user=user, file_path=file_path).values_list("hashed_value", flat=True)

    @staticmethod
    def delete_entry_by_hash(user: KhojUser, hashed_values: List[str]):
        Entry.objects.filter(user=user, hashed_value__in=hashed_values).delete()

    @staticmethod
    def get_entries_by_date_filter(entry: BaseManager[Entry], start_date: date, end_date: date):
        return entry.filter(
            entrydates__date__gte=start_date,
            entrydates__date__lte=end_date,
        )

    @staticmethod
    def user_has_entries(user: KhojUser):
        return Entry.objects.filter(user=user).exists()

    @staticmethod
    async def auser_has_entries(user: KhojUser):
        return await Entry.objects.filter(user=user).aexists()

    @staticmethod
    async def adelete_entry_by_file(user: KhojUser, file_path: str):
        return await Entry.objects.filter(user=user, file_path=file_path).adelete()

    @staticmethod
    def aget_all_filenames_by_source(user: KhojUser, file_source: str):
        return (
            Entry.objects.filter(user=user, file_source=file_source)
            .distinct("file_path")
            .values_list("file_path", flat=True)
        )

    @staticmethod
    async def adelete_all_entries(user: KhojUser):
        return await Entry.objects.filter(user=user).adelete()

    @staticmethod
    def apply_filters(user: KhojUser, query: str, file_type_filter: str = None):
        q_filter_terms = Q()

        explicit_word_terms = EntryAdapters.word_filer.get_filter_terms(query)
        file_filters = EntryAdapters.file_filter.get_filter_terms(query)
        date_filters = EntryAdapters.date_filter.get_query_date_range(query)

        if len(explicit_word_terms) == 0 and len(file_filters) == 0 and len(date_filters) == 0:
            return Entry.objects.filter(user=user)

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

        relevant_entries = Entry.objects.filter(user=user).filter(
            q_filter_terms,
        )
        if file_type_filter:
            relevant_entries = relevant_entries.filter(file_type=file_type_filter)
        return relevant_entries

    @staticmethod
    def search_with_embeddings(
        user: KhojUser,
        embeddings: Tensor,
        max_results: int = 10,
        file_type_filter: str = None,
        raw_query: str = None,
        max_distance: float = math.inf,
    ):
        relevant_entries = EntryAdapters.apply_filters(user, raw_query, file_type_filter)
        relevant_entries = relevant_entries.filter(user=user).annotate(
            distance=CosineDistance("embeddings", embeddings)
        )
        relevant_entries = relevant_entries.filter(distance__lte=max_distance)

        if file_type_filter:
            relevant_entries = relevant_entries.filter(file_type=file_type_filter)
        relevant_entries = relevant_entries.order_by("distance")
        return relevant_entries[:max_results]

    @staticmethod
    def get_unique_file_types(user: KhojUser):
        return Entry.objects.filter(user=user).values_list("file_type", flat=True).distinct()

    @staticmethod
    def get_unique_file_sources(user: KhojUser):
        return Entry.objects.filter(user=user).values_list("file_source", flat=True).distinct().all()
