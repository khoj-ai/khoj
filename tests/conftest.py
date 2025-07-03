import os
from pathlib import Path

import pytest
from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from fastapi.testclient import TestClient

from khoj.configure import (
    configure_middleware,
    configure_routes,
    configure_search_types,
)
from khoj.database.models import (
    Agent,
    ChatModel,
    FileObject,
    GithubConfig,
    GithubRepoConfig,
    KhojApiUser,
    KhojUser,
    LocalMarkdownConfig,
    LocalOrgConfig,
    LocalPdfConfig,
    LocalPlaintextConfig,
)
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.processor.embeddings import CrossEncoderModel, EmbeddingsModel
from khoj.routers.api_content import configure_content
from khoj.search_type import text_search
from khoj.utils import fs_syncer, state
from khoj.utils.config import SearchModels
from khoj.utils.constants import web_directory
from khoj.utils.helpers import resolve_absolute_path
from khoj.utils.rawconfig import ContentConfig, SearchConfig
from tests.helpers import (
    AiModelApiFactory,
    ChatModelFactory,
    ProcessLockFactory,
    SubscriptionFactory,
    UserConversationProcessorConfigFactory,
    UserFactory,
    get_chat_api_key,
    get_chat_provider,
)


@pytest.fixture(autouse=True)
def enable_db_access_for_all_tests(db):
    pass


@pytest.fixture(scope="session", autouse=True)
def django_db_setup(django_db_setup, django_db_blocker):
    """Ensure proper database setup and teardown for all tests."""
    with django_db_blocker.unblock():
        yield


@pytest.fixture(scope="session")
def search_config() -> SearchConfig:
    state.embeddings_model = dict()
    state.embeddings_model["default"] = EmbeddingsModel()
    state.cross_encoder_model = dict()
    state.cross_encoder_model["default"] = CrossEncoderModel()

    model_dir = resolve_absolute_path("~/.khoj/search")
    model_dir.mkdir(parents=True, exist_ok=True)
    search_config = SearchConfig()

    return search_config


@pytest.mark.django_db
@pytest.fixture
def default_user():
    user = UserFactory()
    SubscriptionFactory(user=user)
    return user


@pytest.mark.django_db
@pytest.fixture
def default_user2():
    if KhojUser.objects.filter(username="default").exists():
        return KhojUser.objects.get(username="default")

    user = KhojUser.objects.create(
        username="default",
        email="default@example.com",
        password="default",
    )
    SubscriptionFactory(user=user)
    return user


@pytest.mark.django_db
@pytest.fixture
def default_user3():
    """
    This user should not have any data associated with it
    """
    if KhojUser.objects.filter(username="default3").exists():
        return KhojUser.objects.get(username="default3")

    user = KhojUser.objects.create(
        username="default3",
        email="default3@example.com",
        password="default3",
    )
    SubscriptionFactory(user=user)
    return user


@pytest.mark.django_db
@pytest.fixture
def default_user4():
    """
    This user should not have a valid subscription
    """
    if KhojUser.objects.filter(username="default4").exists():
        return KhojUser.objects.get(username="default4")

    user = KhojUser.objects.create(
        username="default4",
        email="default4@example.com",
        password="default4",
    )
    SubscriptionFactory(user=user, renewal_date=None)
    return user


@pytest.mark.django_db
@pytest.fixture
def api_user(default_user):
    if KhojApiUser.objects.filter(user=default_user).exists():
        return KhojApiUser.objects.get(user=default_user)

    return KhojApiUser.objects.create(
        user=default_user,
        name="api-key",
        token="kk-secret",
    )


@pytest.mark.django_db
@pytest.fixture
def api_user2(default_user2):
    if KhojApiUser.objects.filter(user=default_user2).exists():
        return KhojApiUser.objects.get(user=default_user2)

    return KhojApiUser.objects.create(
        user=default_user2,
        name="api-key",
        token="kk-diff-secret",
    )


@pytest.mark.django_db
@pytest.fixture
def api_user3(default_user3):
    if KhojApiUser.objects.filter(user=default_user3).exists():
        return KhojApiUser.objects.get(user=default_user3)

    return KhojApiUser.objects.create(
        user=default_user3,
        name="api-key",
        token="kk-diff-secret-3",
    )


@pytest.mark.django_db
@pytest.fixture
def api_user4(default_user4):
    if KhojApiUser.objects.filter(user=default_user4).exists():
        return KhojApiUser.objects.get(user=default_user4)

    return KhojApiUser.objects.create(
        user=default_user4,
        name="api-key",
        token="kk-diff-secret-4",
    )


@pytest.mark.django_db
@pytest.fixture
def default_openai_chat_model_option():
    chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    return chat_model


@pytest.mark.django_db
@pytest.fixture
def openai_agent():
    chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    return Agent.objects.create(
        name="Accountant",
        chat_model=chat_model,
        personality="You are a certified CPA. You are able to tell me how much I've spent based on my notes. Regardless of what I ask, you should always respond with the total amount I've spent. ALWAYS RESPOND WITH A SUMMARY TOTAL OF HOW MUCH MONEY I HAVE SPENT.",
    )


@pytest.fixture(scope="session")
def search_models(search_config: SearchConfig):
    search_models = SearchModels()

    return search_models


@pytest.mark.django_db
@pytest.fixture
def default_process_lock():
    return ProcessLockFactory()


@pytest.fixture
def anyio_backend():
    return "asyncio"


@pytest.mark.django_db
@pytest.fixture(scope="function")
def content_config(tmp_path_factory, search_models: SearchModels, default_user: KhojUser):
    content_dir = tmp_path_factory.mktemp("content")

    # Generate Image Embeddings from Test Images
    content_config = ContentConfig()

    LocalOrgConfig.objects.create(
        input_files=None,
        input_filter=["tests/data/org/*.org"],
        index_heading_entries=False,
        user=default_user,
    )

    text_search.setup(OrgToEntries, get_sample_data("org"), regenerate=False, user=default_user)

    if os.getenv("GITHUB_PAT_TOKEN"):
        GithubConfig.objects.create(
            pat_token=os.getenv("GITHUB_PAT_TOKEN"),
            user=default_user,
        )

        GithubRepoConfig.objects.create(
            owner="khoj-ai",
            name="lantern",
            branch="master",
            github_config=GithubConfig.objects.get(user=default_user),
        )

    LocalPlaintextConfig.objects.create(
        input_files=None,
        input_filter=["tests/data/plaintext/*.txt", "tests/data/plaintext/*.md", "tests/data/plaintext/*.html"],
        user=default_user,
    )

    return content_config


@pytest.fixture(scope="session")
def md_content_config():
    markdown_config = LocalMarkdownConfig.objects.create(
        input_files=None,
        input_filter=["tests/data/markdown/*.markdown"],
    )

    return markdown_config


@pytest.fixture(scope="function")
def chat_client(search_config: SearchConfig, default_user2: KhojUser):
    return chat_client_builder(search_config, default_user2, require_auth=False)


@pytest.fixture(scope="function")
def chat_client_with_auth(search_config: SearchConfig, default_user2: KhojUser):
    return chat_client_builder(search_config, default_user2, require_auth=True)


@pytest.fixture(scope="function")
def chat_client_no_background(search_config: SearchConfig, default_user2: KhojUser):
    return chat_client_builder(search_config, default_user2, index_content=False, require_auth=False)


@pytest.fixture(scope="function")
def chat_client_with_large_kb(search_config: SearchConfig, default_user2: KhojUser):
    """
    Chat client fixture that creates a large knowledge base with many files
    for stress testing atomic agent updates.
    """
    return large_kb_chat_client_builder(search_config, default_user2)


@pytest.mark.django_db
def chat_client_builder(search_config, user, index_content=True, require_auth=False):
    # Initialize app state
    state.SearchType = configure_search_types()

    if index_content:
        LocalMarkdownConfig.objects.create(
            input_files=None,
            input_filter=["tests/data/markdown/*.markdown"],
            user=user,
        )

        # Index Markdown Content for Search
        all_files = fs_syncer.collect_files(user=user)
        configure_content(user, all_files)

    # Initialize Processor from Config
    chat_provider = get_chat_provider()
    online_chat_model: ChatModelFactory = None
    if chat_provider == ChatModel.ModelType.OPENAI:
        online_chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    elif chat_provider == ChatModel.ModelType.GOOGLE:
        online_chat_model = ChatModelFactory(name="gemini-2.0-flash", model_type="google")
    elif chat_provider == ChatModel.ModelType.ANTHROPIC:
        online_chat_model = ChatModelFactory(name="claude-3-5-haiku-20241022", model_type="anthropic")
    if online_chat_model:
        online_chat_model.ai_model_api = AiModelApiFactory(api_key=get_chat_api_key(chat_provider))
        UserConversationProcessorConfigFactory(user=user, setting=online_chat_model)

    state.anonymous_mode = not require_auth

    app = FastAPI()

    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")
    return TestClient(app)


@pytest.mark.django_db
def large_kb_chat_client_builder(search_config, user):
    """
    Build a chat client with a large knowledge base for stress testing.
    Creates 200+ markdown files with substantial content.
    """
    import os
    import shutil
    import tempfile

    # Initialize app state
    state.SearchType = configure_search_types()

    # Create temporary directory for large number of test files
    temp_dir = tempfile.mkdtemp(prefix="khoj_test_large_kb_")
    large_file_list = []

    try:
        # Generate 200 test files with substantial content
        for i in range(300):
            file_path = os.path.join(temp_dir, f"test_file_{i:03d}.markdown")
            content = f"""
# Test File {i}

This is test file {i} with substantial content for stress testing agent knowledge base updates.

## Section 1: Introduction
This section introduces the topic of file {i}. It contains enough text to create meaningful
embeddings and entries in the database for realistic testing.

## Section 2: Technical Details
Technical content for file {i}:
- Implementation details
- Best practices
- Code examples
- Architecture notes

## Section 3: Code Examples
```python
def example_function_{i}():
    '''Example function from file {i}'''
    return f"Result from file {i}"

class TestClass{i}:
    def __init__(self):
        self.value = {i}
        self.data = [f"item_{{j}}" for j in range(10)]

    def process(self):
        return f"Processing {{len(self.data)}} items from file {i}"
```

## Section 4: Additional Content
More substantial content to make the files realistic and ensure proper
database entry creation during content processing.

File statistics:
- File number: {i}
- Content sections: 4
- Code examples: Yes
- Purpose: Stress testing atomic agent updates

{'Additional padding content. ' * 20}

End of file {i}.
"""
            with open(file_path, "w") as f:
                f.write(content)
            large_file_list.append(file_path)

        # Create LocalMarkdownConfig with all the generated files
        LocalMarkdownConfig.objects.create(
            input_files=large_file_list,
            input_filter=None,
            user=user,
        )

        # Index all the files into the user's knowledge base
        all_files = fs_syncer.collect_files(user=user)
        configure_content(user, all_files)

        # Verify we have a substantial knowledge base
        file_count = FileObject.objects.filter(user=user, agent=None).count()
        if file_count < 150:
            raise RuntimeError(f"Large KB fixture failed: only {file_count} files indexed, expected at least 150")

    except Exception as e:
        # Cleanup on error
        if os.path.exists(temp_dir):
            shutil.rmtree(temp_dir)
        raise e

    # Initialize chat processor
    chat_provider = get_chat_provider()
    online_chat_model = None
    if chat_provider == ChatModel.ModelType.OPENAI:
        online_chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    elif chat_provider == ChatModel.ModelType.GOOGLE:
        online_chat_model = ChatModelFactory(name="gemini-2.0-flash", model_type="google")
    elif chat_provider == ChatModel.ModelType.ANTHROPIC:
        online_chat_model = ChatModelFactory(name="claude-3-5-haiku-20241022", model_type="anthropic")

    if online_chat_model:
        online_chat_model.ai_model_api = AiModelApiFactory(api_key=get_chat_api_key(chat_provider))
        UserConversationProcessorConfigFactory(user=user, setting=online_chat_model)

    state.anonymous_mode = False

    app = FastAPI()
    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")

    # Store temp_dir for cleanup (though Django test cleanup should handle it)
    client = TestClient(app)
    client._temp_dir = temp_dir  # Store for potential cleanup

    return client


@pytest.fixture(scope="function")
def fastapi_app():
    app = FastAPI()
    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")
    return app


@pytest.fixture(scope="function")
def client(
    api_user: KhojApiUser,
):
    state.SearchType = configure_search_types()
    state.embeddings_model = dict()
    state.embeddings_model["default"] = EmbeddingsModel()
    state.cross_encoder_model = dict()
    state.cross_encoder_model["default"] = CrossEncoderModel()

    # These lines help us Mock the Search models for these search types
    text_search.setup(
        OrgToEntries,
        get_sample_data("org"),
        regenerate=False,
        user=api_user.user,
    )
    text_search.setup(
        PlaintextToEntries,
        get_sample_data("plaintext"),
        regenerate=False,
        user=api_user.user,
    )

    state.anonymous_mode = False

    app = FastAPI()
    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")
    return TestClient(app)


@pytest.fixture(scope="function")
def new_org_file(default_user: KhojUser, content_config: ContentConfig):
    # Setup
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    input_filters = org_config.input_filter
    new_org_file = Path(input_filters[0]).parent / "new_file.org"
    new_org_file.touch()

    yield new_org_file

    # Cleanup
    if new_org_file.exists():
        new_org_file.unlink()


@pytest.fixture(scope="function")
def org_config_with_only_new_file(new_org_file: Path, default_user: KhojUser):
    LocalOrgConfig.objects.update(input_files=[str(new_org_file)], input_filter=None)
    return LocalOrgConfig.objects.filter(user=default_user).first()


@pytest.fixture(scope="function")
def pdf_configured_user1(default_user: KhojUser):
    LocalPdfConfig.objects.create(
        input_files=None,
        input_filter=["tests/data/pdf/singlepage.pdf"],
        user=default_user,
    )
    # Index Markdown Content for Search
    all_files = fs_syncer.collect_files(user=default_user)
    configure_content(default_user, all_files)


@pytest.fixture(scope="function")
def sample_org_data():
    return get_sample_data("org")


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
