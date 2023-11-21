import factory
import os

from khoj.database.models import (
    KhojUser,
    KhojApiUser,
    ChatModelOptions,
    OfflineChatProcessorConversationConfig,
    OpenAIProcessorConversationConfig,
    SearchModelConfig,
    UserConversationConfig,
    Conversation,
    Subscription,
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

    max_prompt_size = 2000
    tokenizer = None
    chat_model = "mistral-7b-instruct-v0.1.Q4_0.gguf"
    model_type = "offline"


class UserConversationProcessorConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = UserConversationConfig

    user = factory.SubFactory(UserFactory)
    setting = factory.SubFactory(ChatModelOptionsFactory)


class OfflineChatProcessorConversationConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = OfflineChatProcessorConversationConfig

    enabled = True


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
    cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2"


class SubscriptionFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = Subscription

    user = factory.SubFactory(UserFactory)
    type = "standard"
    is_recurring = False
    renewal_date = "2100-04-01"
