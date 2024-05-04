import os
from datetime import datetime

import factory
from django.utils.timezone import make_aware

from khoj.database.models import (
    ChatModelOptions,
    Conversation,
    KhojApiUser,
    KhojUser,
    OpenAIProcessorConversationConfig,
    ProcessLock,
    SearchModelConfig,
    Subscription,
    UserConversationConfig,
)


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


class ChatModelOptionsFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ChatModelOptions

    max_prompt_size = 3500
    tokenizer = None
    chat_model = "NousResearch/Hermes-2-Pro-Mistral-7B-GGUF"
    model_type = "offline"


class UserConversationProcessorConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = UserConversationConfig

    user = factory.SubFactory(UserFactory)
    setting = factory.SubFactory(ChatModelOptionsFactory)


class OpenAIProcessorConversationConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = OpenAIProcessorConversationConfig

    api_key = os.getenv("OPENAI_API_KEY")


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
    type = "standard"
    is_recurring = False
    renewal_date = make_aware(datetime.strptime("2100-04-01", "%Y-%m-%d"))


class ProcessLockFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ProcessLock

    name = "test_lock"
