import factory
import os

from database.models import (
    KhojUser,
    KhojApiUser,
    ChatModelOptions,
    OfflineChatProcessorConversationConfig,
    OpenAIProcessorConversationConfig,
    UserConversationConfig,
    Conversation,
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
    chat_model = "llama-2-7b-chat.ggmlv3.q4_0.bin"
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
