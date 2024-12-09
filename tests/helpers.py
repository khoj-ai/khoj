import os
from datetime import datetime

import factory
from django.utils.timezone import make_aware

from khoj.database.models import (
    AiModelApi,
    ChatModelOptions,
    Conversation,
    KhojApiUser,
    KhojUser,
    ProcessLock,
    SearchModelConfig,
    Subscription,
    UserConversationConfig,
)
from khoj.processor.conversation.utils import message_to_log


def get_chat_provider(default: ChatModelOptions.ModelType | None = ChatModelOptions.ModelType.OFFLINE):
    provider = os.getenv("KHOJ_TEST_CHAT_PROVIDER")
    if provider and provider in ChatModelOptions.ModelType:
        return ChatModelOptions.ModelType(provider)
    elif os.getenv("OPENAI_API_KEY"):
        return ChatModelOptions.ModelType.OPENAI
    elif os.getenv("GEMINI_API_KEY"):
        return ChatModelOptions.ModelType.GOOGLE
    elif os.getenv("ANTHROPIC_API_KEY"):
        return ChatModelOptions.ModelType.ANTHROPIC
    else:
        return default


def get_chat_api_key(provider: ChatModelOptions.ModelType = None):
    provider = provider or get_chat_provider()
    if provider == ChatModelOptions.ModelType.OPENAI:
        return os.getenv("OPENAI_API_KEY")
    elif provider == ChatModelOptions.ModelType.GOOGLE:
        return os.getenv("GEMINI_API_KEY")
    elif provider == ChatModelOptions.ModelType.ANTHROPIC:
        return os.getenv("ANTHROPIC_API_KEY")
    else:
        return os.getenv("OPENAI_API_KEY") or os.getenv("GEMINI_API_KEY") or os.getenv("ANTHROPIC_API_KEY")


def generate_chat_history(message_list):
    # Generate conversation logs
    conversation_log = {"chat": []}
    for user_message, chat_response, context in message_list:
        message_to_log(
            user_message,
            chat_response,
            {"context": context, "intent": {"query": user_message, "inferred-queries": f'["{user_message}"]'}},
            conversation_log=conversation_log.get("chat", []),
        )
    return conversation_log


class UserFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = KhojUser

    username = factory.Faker("name")
    email = factory.Faker("email")
    password = factory.Faker("password")
    uuid = factory.Faker("uuid4")


class ApiUserFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = KhojApiUser

    user = None
    name = factory.Faker("name")
    token = factory.Faker("password")


class AiModelApiFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = AiModelApi

    api_key = get_chat_api_key()


class ChatModelOptionsFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ChatModelOptions

    max_prompt_size = 20000
    tokenizer = None
    chat_model = "bartowski/Meta-Llama-3.2-3B-Instruct-GGUF"
    model_type = get_chat_provider()
    ai_model_api = factory.LazyAttribute(lambda obj: AiModelApiFactory() if get_chat_api_key() else None)


class UserConversationProcessorConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = UserConversationConfig

    user = factory.SubFactory(UserFactory)
    setting = factory.SubFactory(ChatModelOptionsFactory)


class ConversationFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = Conversation

    user = factory.SubFactory(UserFactory)


class SearchModelFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = SearchModelConfig

    name = "default"
    model_type = "text"
    bi_encoder = "thenlper/gte-small"
    cross_encoder = "mixedbread-ai/mxbai-rerank-xsmall-v1"


class SubscriptionFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = Subscription

    user = factory.SubFactory(UserFactory)
    type = Subscription.Type.STANDARD
    is_recurring = False
    renewal_date = make_aware(datetime.strptime("2100-04-01", "%Y-%m-%d"))


class ProcessLockFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ProcessLock

    name = "test_lock"
