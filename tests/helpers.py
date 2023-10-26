import factory
import os

from database.models import (
    KhojUser,
    ConversationProcessorConfig,
    OfflineChatProcessorConversationConfig,
    OpenAIProcessorConversationConfig,
    Conversation,
)


class UserFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = KhojUser

    username = factory.Faker("name")
    email = factory.Faker("email")
    password = factory.Faker("password")
    uuid = factory.Faker("uuid4")


class ConversationProcessorConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ConversationProcessorConfig

    max_prompt_size = 2000
    tokenizer = None


class OfflineChatProcessorConversationConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = OfflineChatProcessorConversationConfig

    enable_offline_chat = True
    chat_model = "llama-2-7b-chat.ggmlv3.q4_0.bin"


class OpenAIProcessorConversationConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = OpenAIProcessorConversationConfig

    api_key = os.getenv("OPENAI_API_KEY")
    chat_model = "gpt-3.5-turbo"


class ConversationFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = Conversation

    user = factory.SubFactory(UserFactory)
