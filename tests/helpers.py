import glob
import logging
import os
from datetime import datetime

import factory
from asgiref.sync import sync_to_async
from django.utils.timezone import make_aware

from khoj.database.adapters import AgentAdapters
from khoj.database.models import (
    Agent,
    AiModelApi,
    ChatMessageModel,
    ChatModel,
    Conversation,
    KhojApiUser,
    KhojUser,
    ProcessLock,
    SearchModelConfig,
    ServerChatSettings,
    Subscription,
    UserConversationConfig,
    UserMemory,
)
from khoj.processor.conversation.utils import message_to_log
from khoj.utils.helpers import get_absolute_path, is_none_or_empty

logger = logging.getLogger(__name__)


def get_chat_provider(default: ChatModel.ModelType | None = ChatModel.ModelType.GOOGLE):
    provider = os.getenv("KHOJ_TEST_CHAT_PROVIDER")
    if provider and provider in ChatModel.ModelType:
        return ChatModel.ModelType(provider)
    elif os.getenv("OPENAI_API_KEY"):
        return ChatModel.ModelType.OPENAI
    elif os.getenv("GEMINI_API_KEY"):
        return ChatModel.ModelType.GOOGLE
    elif os.getenv("ANTHROPIC_API_KEY"):
        return ChatModel.ModelType.ANTHROPIC
    else:
        return default


def get_chat_api_key(provider: ChatModel.ModelType = None):
    provider = provider or get_chat_provider()
    if provider == ChatModel.ModelType.OPENAI:
        return os.getenv("OPENAI_API_KEY")
    elif provider == ChatModel.ModelType.GOOGLE:
        return os.getenv("GEMINI_API_KEY")
    elif provider == ChatModel.ModelType.ANTHROPIC:
        return os.getenv("ANTHROPIC_API_KEY")
    else:
        return os.getenv("OPENAI_API_KEY") or os.getenv("GEMINI_API_KEY") or os.getenv("ANTHROPIC_API_KEY")


def generate_chat_history(message_list):
    # Generate conversation logs
    chat_history: list[ChatMessageModel] = []
    for user_message, chat_response, context in message_list:
        message_to_log(
            user_message,
            chat_response,
            {
                "context": context,
                "intent": {"type": "memory", "query": user_message, "inferred-queries": [user_message]},
            },
            chat_history=chat_history,
        )
    return chat_history


def get_sample_data(type):
    sample_data = {
        "org": {
            "elisp.org": """
* Emacs Khoj
  /An Emacs interface for [[https://github.com/khoj-ai/khoj][khoj]]/

** Requirements
   - Install and Run [[https://github.com/khoj-ai/khoj][khoj]]

** Installation
*** Direct
     - Put ~khoj.el~ in your Emacs load path. For e.g. ~/.emacs.d/lisp
     - Load via ~use-package~ in your ~/.emacs.d/init.el or .emacs file by adding below snippet
       #+begin_src elisp
         ;; Khoj Package
         (use-package khoj
           :load-path "~/.emacs.d/lisp/khoj.el"
           :bind ("C-c s" . 'khoj))
       #+end_src

*** Using [[https://github.com/quelpa/quelpa#installation][Quelpa]]
     - Ensure [[https://github.com/quelpa/quelpa#installation][Quelpa]], [[https://github.com/quelpa/quelpa-use-package#installation][quelpa-use-package]] are installed
     - Add below snippet to your ~/.emacs.d/init.el or .emacs config file and execute it.
       #+begin_src elisp
         ;; Khoj Package
         (use-package khoj
           :quelpa (khoj :fetcher url :url "https://raw.githubusercontent.com/khoj-ai/khoj/master/interface/emacs/khoj.el")
           :bind ("C-c s" . 'khoj))
       #+end_src

** Usage
   1. Call ~khoj~ using keybinding ~C-c s~ or ~M-x khoj~
   2. Enter Query in Natural Language
      e.g. "What is the meaning of life?" "What are my life goals?"
   3. Wait for results
      *Note: It takes about 15s on a Mac M1 and a ~100K lines corpus of org-mode files*
   4. (Optional) Narrow down results further
      Include/Exclude specific words from results by adding to query
      e.g. "What is the meaning of life? -god +none"

""",
            "readme.org": """
* Khoj
  /Allow natural language search on user content like notes, images using transformer based models/

  All data is processed locally. User can interface with khoj app via [[./interface/emacs/khoj.el][Emacs]], API or Commandline

** Dependencies
   - Python3
   - [[https://docs.conda.io/en/latest/miniconda.html#latest-miniconda-installer-links][Miniconda]]

** Install
   #+begin_src shell
   git clone https://github.com/khoj-ai/khoj && cd khoj
   conda env create -f environment.yml
   conda activate khoj
   #+end_src""",
        },
        "markdown": {
            "readme.markdown": """
# Khoj
Allow natural language search on user content like notes, images using transformer based models

All data is processed locally. User can interface with khoj app via [Emacs](./interface/emacs/khoj.el), API or Commandline

## Dependencies
- Python3
- [Miniconda](https://docs.conda.io/en/latest/miniconda.html#latest-miniconda-installer-links)

## Install
```shell
git clone
conda env create -f environment.yml
conda activate khoj
```
"""
        },
        "plaintext": {
            "readme.txt": """
Khoj
Allow natural language search on user content like notes, images using transformer based models

All data is processed locally. User can interface with khoj app via Emacs, API or Commandline

Dependencies
- Python3
- Miniconda

Install
git clone
conda env create -f environment.yml
conda activate khoj
"""
        },
    }

    return sample_data[type]


def get_index_files(
    input_files: list[str] = None, input_filters: list[str] | None = ["tests/data/org/*.org"]
) -> dict[str, str]:
    # Input Validation
    if is_none_or_empty(input_files) and is_none_or_empty(input_filters):
        logger.debug("At least one of input_files or input_filter is required to be specified")
        return {}

    # Get files to process
    absolute_files, filtered_files = set(), set()
    if input_files:
        absolute_files = {get_absolute_path(input_file) for input_file in input_files}
    if input_filters:
        filtered_files = {
            filtered_file
            for file_filter in input_filters
            for filtered_file in glob.glob(get_absolute_path(file_filter), recursive=True)
            if os.path.isfile(filtered_file)
        }

    all_files = sorted(absolute_files | filtered_files)

    filename_to_content_map = {}
    for file in all_files:
        with open(file, "r", encoding="utf8") as f:
            try:
                filename_to_content_map[file] = f.read()
            except Exception as e:
                logger.warning(f"Unable to read file: {file}. Skipping file.")
                logger.warning(e, exc_info=True)

    return filename_to_content_map


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


class ChatModelFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ChatModel

    max_prompt_size = 20000
    tokenizer = None
    name = "gemini-2.5-flash"
    model_type = get_chat_provider()
    ai_model_api = factory.LazyAttribute(lambda obj: AiModelApiFactory() if get_chat_api_key() else None)


class UserConversationProcessorConfigFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = UserConversationConfig

    user = factory.SubFactory(UserFactory)
    setting = factory.SubFactory(ChatModelFactory)


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


class ServerChatSettingsFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = ServerChatSettings

    memory_mode = ServerChatSettings.MemoryMode.ENABLED_DEFAULT_ON


# Async-safe wrappers for factories and ORM operations
async def acreate_user():
    return await sync_to_async(UserFactory)()


async def acreate_subscription(user):
    return await sync_to_async(SubscriptionFactory)(user=user)


async def acreate_chat_model():
    return await sync_to_async(ChatModelFactory)()


async def acreate_default_agent():
    return await sync_to_async(AgentAdapters.create_default_agent)()


async def acreate_agent(name, chat_model, personality):
    return await sync_to_async(Agent.objects.create)(
        name=name,
        chat_model=chat_model,
        personality=personality,
    )


async def acreate_test_memory(user, agent=None, raw_text="test memory"):
    """Create a memory directly in DB without embeddings for testing."""
    return await sync_to_async(UserMemory.objects.create)(
        user=user,
        agent=agent,
        raw=raw_text,
        embeddings=[0.1] * 384,  # Dummy embeddings
    )
