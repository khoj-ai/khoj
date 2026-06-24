import pytest
from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from fastapi.testclient import TestClient

from khoj.configure import (
    configure_middleware,
    configure_routes,
    configure_search_types,
)
from khoj.database.adapters import get_default_search_model
from khoj.database.models import (
    Agent,
    ChatModel,
    FileObject,
    GithubConfig,
    GithubRepoConfig,
    KhojApiUser,
    KhojUser,
)
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.processor.embeddings import CrossEncoderModel, EmbeddingsModel
from khoj.routers.api_content import configure_content
from khoj.search_type import text_search
from khoj.utils import state
from khoj.utils.constants import web_directory
from tests.helpers import (
    AiModelApiFactory,
    ChatModelFactory,
    ProcessLockFactory,
    SubscriptionFactory,
    UserConversationProcessorConfigFactory,
    UserFactory,
    get_chat_api_key,
    get_chat_provider,
    get_index_files,
    get_sample_data,
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
def search_config():
    search_model = get_default_search_model()
    state.embeddings_model = dict()
    state.embeddings_model["default"] = EmbeddingsModel(
        model_name=search_model.bi_encoder, model_kwargs=search_model.bi_encoder_model_config
    )
    state.cross_encoder_model = dict()
    state.cross_encoder_model["default"] = CrossEncoderModel(
        model_name=search_model.cross_encoder, model_kwargs=search_model.cross_encoder_model_config
    )


@pytest.fixture
def default_user():
    user = UserFactory()
    SubscriptionFactory(user=user)
    return user


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


@pytest.fixture
def api_user(default_user):
    if KhojApiUser.objects.filter(user=default_user).exists():
        return KhojApiUser.objects.get(user=default_user)

    return KhojApiUser.objects.create(
        user=default_user,
        name="api-key",
        token="kk-secret",
    )


@pytest.fixture
def api_user2(default_user2):
    if KhojApiUser.objects.filter(user=default_user2).exists():
        return KhojApiUser.objects.get(user=default_user2)

    return KhojApiUser.objects.create(
        user=default_user2,
        name="api-key",
        token="kk-diff-secret",
    )


@pytest.fixture
def api_user3(default_user3):
    if KhojApiUser.objects.filter(user=default_user3).exists():
        return KhojApiUser.objects.get(user=default_user3)

    return KhojApiUser.objects.create(
        user=default_user3,
        name="api-key",
        token="kk-diff-secret-3",
    )


@pytest.fixture
def api_user4(default_user4):
    if KhojApiUser.objects.filter(user=default_user4).exists():
        return KhojApiUser.objects.get(user=default_user4)

    return KhojApiUser.objects.create(
        user=default_user4,
        name="api-key",
        token="kk-diff-secret-4",
    )


@pytest.fixture
def default_openai_chat_model_option():
    chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    return chat_model


@pytest.fixture
def openai_agent():
    chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    return Agent.objects.create(
        name="Accountant",
        chat_model=chat_model,
        personality="You are a certified CPA. You are able to tell me how much I've spent based on my notes. Regardless of what I ask, you should always respond with the total amount I've spent. ALWAYS RESPOND WITH A SUMMARY TOTAL OF HOW MUCH MONEY I HAVE SPENT.",
    )


@pytest.fixture
def default_process_lock():
    return ProcessLockFactory()


@pytest.fixture
def anyio_backend():
    return "asyncio"


@pytest.fixture(scope="function")
def chat_client(search_config, default_user2: KhojUser):
    return chat_client_builder(search_config, default_user2, require_auth=False)


@pytest.fixture(scope="function")
def chat_client_with_auth(search_config, default_user2: KhojUser):
    return chat_client_builder(search_config, default_user2, require_auth=True)


@pytest.fixture(scope="function")
def chat_client_no_background(search_config, default_user2: KhojUser):
    return chat_client_builder(search_config, default_user2, index_content=False, require_auth=False)


@pytest.fixture(scope="function")
def chat_client_with_large_kb(search_config, default_user2: KhojUser):
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
        file_type = "markdown"
        files_to_index = {file_type: get_index_files(input_filters=[f"tests/data/{file_type}/*.{file_type}"])}

        # Index Markdown Content for Search
        configure_content(user, files_to_index)

    # Initialize Processor from Config
    chat_provider = get_chat_provider()
    online_chat_model: ChatModelFactory = None
    if chat_provider == ChatModel.ModelType.OPENAI:
        online_chat_model = ChatModelFactory(name="gpt-4o-mini", model_type="openai")
    elif chat_provider == ChatModel.ModelType.GOOGLE:
        online_chat_model = ChatModelFactory(name="gemini-2.5-flash", model_type="google")
    elif chat_provider == ChatModel.ModelType.ANTHROPIC:
        online_chat_model = ChatModelFactory(name="claude-haiku-4-5-20251001", model_type="anthropic")
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
    file_type = "markdown"
    large_file_list = []

    try:
        # Generate 200 test files with substantial content
        for i in range(300):
            file_path = os.path.join(temp_dir, f"test_file_{i:03d}.{file_type}")
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

{"Additional padding content. " * 20}

End of file {i}.
"""
            with open(file_path, "w") as f:
                f.write(content)
            large_file_list.append(file_path)

        # Index all generated files into the user's knowledge base
        files_to_index = {file_type: get_index_files(input_files=large_file_list, input_filters=None)}
        configure_content(user, files_to_index)

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
        online_chat_model = ChatModelFactory(name="gemini-2.5-flash", model_type="google")
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
def pdf_configured_user1(default_user: KhojUser):
    # Read data from pdf file at tests/data/pdf/singlepage.pdf
    pdf_file_path = "tests/data/pdf/singlepage.pdf"
    with open(pdf_file_path, "rb") as pdf_file:
        pdf_data = pdf_file.read()

    knowledge_base = {"pdf": {"singlepage.pdf": pdf_data}}
    # Index Content for Search
    configure_content(default_user, knowledge_base)


@pytest.fixture(scope="function")
def sample_org_data():
    return get_sample_data("org")
