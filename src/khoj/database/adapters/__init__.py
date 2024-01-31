import math
import random
import secrets
import sys
from datetime import date, datetime, timedelta, timezone
from enum import Enum
from typing import List, Optional, Type

from asgiref.sync import sync_to_async
from django.contrib.sessions.backends.db import SessionStore
from django.db import models
from django.db.models import Q
from django.db.models.manager import BaseManager
from fastapi import HTTPException
from pgvector.django import CosineDistance
from torch import Tensor

from khoj.database.models import (
    ChatModelOptions,
    ClientApplication,
    Conversation,
    Entry,
    GithubConfig,
    GithubRepoConfig,
    GoogleUser,
    KhojApiUser,
    KhojUser,
    NotionConfig,
    OfflineChatProcessorConversationConfig,
    OpenAIProcessorConversationConfig,
    ReflectiveQuestion,
    SearchModelConfig,
    SpeechToTextModelOptions,
    Subscription,
    TextToImageModelConfig,
    UserConversationConfig,
    UserRequests,
    UserSearchModelConfig,
)
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.file_filter import FileFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.utils import state
from khoj.utils.config import GPT4AllProcessorModel
from khoj.utils.helpers import generate_random_name, is_none_or_empty


class SubscriptionState(Enum):
    TRIAL = "trial"
    SUBSCRIBED = "subscribed"
    UNSUBSCRIBED = "unsubscribed"
    EXPIRED = "expired"
    INVALID = "invalid"


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


async def aget_or_create_user_by_phone_number(phone_number: str) -> KhojUser:
    if is_none_or_empty(phone_number):
        return None
    user = await aget_user_by_phone_number(phone_number)
    if not user:
        user = await acreate_user_by_phone_number(phone_number)
    return user


async def aset_user_phone_number(user: KhojUser, phone_number: str) -> KhojUser:
    if is_none_or_empty(phone_number):
        return None
    phone_number = phone_number.strip()
    if not phone_number.startswith("+"):
        phone_number = f"+{phone_number}"
    existing_user_with_phone_number = await aget_user_by_phone_number(phone_number)
    if existing_user_with_phone_number and existing_user_with_phone_number.id != user.id:
        if is_none_or_empty(existing_user_with_phone_number.email):
            # Transfer conversation history to the new user. If they don't have an associated email, they are effectively a new user
            async for conversation in Conversation.objects.filter(user=existing_user_with_phone_number).aiterator():
                conversation.user = user
                await conversation.asave()

            await existing_user_with_phone_number.adelete()
        else:
            raise HTTPException(status_code=400, detail="Phone number already exists")

    user.phone_number = phone_number
    await user.asave()
    return user


async def aremove_phone_number(user: KhojUser) -> KhojUser:
    user.phone_number = None
    user.verified_phone_number = False
    await user.asave()
    return user


async def acreate_user_by_phone_number(phone_number: str) -> KhojUser:
    if is_none_or_empty(phone_number):
        return None
    user, _ = await KhojUser.objects.filter(phone_number=phone_number).aupdate_or_create(
        defaults={"username": phone_number, "phone_number": phone_number}
    )
    await user.asave()

    await Subscription.objects.acreate(user=user, type="trial")

    return user


async def get_or_create_user_by_email(email: str) -> KhojUser:
    user, _ = await KhojUser.objects.filter(email=email).aupdate_or_create(defaults={"username": email, "email": email})
    await user.asave()

    user_subscription = await Subscription.objects.filter(user=user).afirst()
    if not user_subscription:
        await Subscription.objects.acreate(user=user, type="trial")

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
    # Get or create the user object and their subscription
    user = await get_or_create_user_by_email(email)
    user_subscription = await Subscription.objects.filter(user=user).afirst()

    # Update the user subscription state
    user_subscription.type = type
    if is_recurring is not None:
        user_subscription.is_recurring = is_recurring
    if renewal_date is False:
        user_subscription.renewal_date = None
    elif renewal_date is not None:
        user_subscription.renewal_date = renewal_date
    await user_subscription.asave()
    return user_subscription


def subscription_to_state(subscription: Subscription) -> str:
    if not subscription:
        return SubscriptionState.INVALID.value
    elif subscription.type == Subscription.Type.TRIAL:
        # Trial subscription is valid for 7 days
        if datetime.now(tz=timezone.utc) - subscription.created_at > timedelta(days=7):
            return SubscriptionState.EXPIRED.value

        return SubscriptionState.TRIAL.value
    elif subscription.is_recurring and subscription.renewal_date >= datetime.now(tz=timezone.utc):
        return SubscriptionState.SUBSCRIBED.value
    elif not subscription.is_recurring and subscription.renewal_date is None:
        return SubscriptionState.EXPIRED.value
    elif not subscription.is_recurring and subscription.renewal_date >= datetime.now(tz=timezone.utc):
        return SubscriptionState.UNSUBSCRIBED.value
    elif not subscription.is_recurring and subscription.renewal_date < datetime.now(tz=timezone.utc):
        return SubscriptionState.EXPIRED.value
    return SubscriptionState.INVALID.value


def get_user_subscription_state(email: str) -> str:
    """Get subscription state of user
    Valid state transitions: trial -> subscribed <-> unsubscribed OR expired
    """
    user_subscription = Subscription.objects.filter(user__email=email).first()
    return subscription_to_state(user_subscription)


async def aget_user_subscription_state(user: KhojUser) -> str:
    """Get subscription state of user
    Valid state transitions: trial -> subscribed <-> unsubscribed OR expired
    """
    user_subscription = await Subscription.objects.filter(user=user).afirst()
    return subscription_to_state(user_subscription)


async def get_user_by_email(email: str) -> KhojUser:
    return await KhojUser.objects.filter(email=email).afirst()


async def get_user_by_token(token: dict) -> KhojUser:
    google_user = await GoogleUser.objects.filter(sub=token.get("sub")).select_related("user").afirst()
    if not google_user:
        return None
    return google_user.user


async def aget_user_by_phone_number(phone_number: str) -> KhojUser:
    if is_none_or_empty(phone_number):
        return None
    matched_user = await KhojUser.objects.filter(phone_number=phone_number).prefetch_related("subscription").afirst()

    if not matched_user:
        return None

    # If the user with this phone number does not have an email account with Khoj, return the user
    if is_none_or_empty(matched_user.email):
        return matched_user

    # If the user has an email account with Khoj and a verified number, return the user
    if matched_user.verified_phone_number:
        return matched_user

    return None


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


def delete_user_requests(window: timedelta = timedelta(days=1)):
    return UserRequests.objects.filter(created_at__lte=datetime.now(tz=timezone.utc) - window).delete()


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


def get_user_search_model_or_default(user=None):
    if user and UserSearchModelConfig.objects.filter(user=user).exists():
        return UserSearchModelConfig.objects.filter(user=user).first().setting

    if SearchModelConfig.objects.filter(name="default").exists():
        return SearchModelConfig.objects.filter(name="default").first()
    else:
        SearchModelConfig.objects.create()

    return SearchModelConfig.objects.first()


def get_or_create_search_models():
    search_models = SearchModelConfig.objects.all()
    if search_models.count() == 0:
        SearchModelConfig.objects.create()
        search_models = SearchModelConfig.objects.all()

    return search_models


async def aset_user_search_model(user: KhojUser, search_model_config_id: int):
    config = await SearchModelConfig.objects.filter(id=search_model_config_id).afirst()
    if not config:
        return None
    new_config, _ = await UserSearchModelConfig.objects.aupdate_or_create(user=user, defaults={"setting": config})
    return new_config


class ClientApplicationAdapters:
    @staticmethod
    async def aget_client_application_by_id(client_id: str, client_secret: str):
        return await ClientApplication.objects.filter(client_id=client_id, client_secret=client_secret).afirst()


class ConversationAdapters:
    @staticmethod
    def get_conversation_by_user(user: KhojUser, client_application: ClientApplication = None):
        conversation = Conversation.objects.filter(user=user, client=client_application)
        if conversation.exists():
            return conversation.first()
        return Conversation.objects.create(user=user, client=client_application)

    @staticmethod
    async def aget_conversation_by_user(user: KhojUser, client_application: ClientApplication = None):
        conversation = Conversation.objects.filter(user=user, client=client_application)
        if await conversation.aexists():
            return await conversation.afirst()
        return await Conversation.objects.acreate(user=user, client=client_application)

    @staticmethod
    async def adelete_conversation_by_user(user: KhojUser):
        return await Conversation.objects.filter(user=user).adelete()

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
    def save_conversation(user: KhojUser, conversation_log: dict, client_application: ClientApplication = None):
        conversation = Conversation.objects.filter(user=user, client=client_application)
        if conversation.exists():
            conversation.update(conversation_log=conversation_log)
        else:
            Conversation.objects.create(user=user, conversation_log=conversation_log, client=client_application)

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
    async def get_default_offline_llm():
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
    async def aget_default_openai_llm():
        return await ChatModelOptions.objects.filter(model_type="openai").afirst()

    @staticmethod
    async def get_openai_chat_config():
        return await OpenAIProcessorConversationConfig.objects.filter().afirst()

    @staticmethod
    async def get_speech_to_text_config():
        return await SpeechToTextModelOptions.objects.filter().afirst()

    @staticmethod
    async def aget_conversation_starters(user: KhojUser):
        all_questions = []
        if await ReflectiveQuestion.objects.filter(user=user).aexists():
            all_questions = await sync_to_async(ReflectiveQuestion.objects.filter(user=user).values_list)(
                "question", flat=True
            )

        all_questions = await sync_to_async(ReflectiveQuestion.objects.filter(user=None).values_list)(
            "question", flat=True
        )

        max_results = 3
        all_questions = await sync_to_async(list)(all_questions)  # type: ignore
        if len(all_questions) < max_results:
            return all_questions

        return random.sample(all_questions, max_results)

    @staticmethod
    def get_valid_conversation_config(user: KhojUser):
        offline_chat_config = ConversationAdapters.get_offline_chat_conversation_config()
        conversation_config = ConversationAdapters.get_conversation_config(user)
        if conversation_config is None:
            conversation_config = ConversationAdapters.get_default_conversation_config()

        if offline_chat_config and offline_chat_config.enabled and conversation_config.model_type == "offline":
            if state.gpt4all_processor_config is None or state.gpt4all_processor_config.loaded_model is None:
                state.gpt4all_processor_config = GPT4AllProcessorModel(conversation_config.chat_model)

            return conversation_config

        openai_chat_config = ConversationAdapters.get_openai_conversation_config()
        if openai_chat_config and conversation_config.model_type == "openai":
            return conversation_config

        else:
            raise ValueError("Invalid conversation config - either configure offline chat or openai chat")

    @staticmethod
    async def aget_text_to_image_model_config():
        return await TextToImageModelConfig.objects.filter().afirst()


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
    def get_size_of_indexed_data_in_mb(user: KhojUser):
        entries = Entry.objects.filter(user=user).iterator()
        total_size = sum(sys.getsizeof(entry.compiled) for entry in entries)
        return total_size / 1024 / 1024

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
