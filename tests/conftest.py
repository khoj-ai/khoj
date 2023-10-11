# External Packages
import os
from fastapi.testclient import TestClient
from pathlib import Path
import pytest
from fastapi.staticfiles import StaticFiles
from fastapi import FastAPI
import factory
import os
from fastapi import FastAPI

app = FastAPI()


# Internal Packages
from khoj.configure import configure_processor, configure_routes, configure_search_types, configure_middleware
from khoj.processor.plaintext.plaintext_to_jsonl import PlaintextToJsonl
from khoj.search_type import image_search, text_search
from khoj.utils.config import SearchModels
from khoj.utils.constants import web_directory
from khoj.utils.helpers import resolve_absolute_path
from khoj.utils.rawconfig import (
    ContentConfig,
    ConversationProcessorConfig,
    OpenAIProcessorConfig,
    ProcessorConfig,
    TextContentConfig,
    ImageContentConfig,
    SearchConfig,
    TextSearchConfig,
    ImageSearchConfig,
)
from khoj.utils import state, fs_syncer
from khoj.routers.indexer import configure_content
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from database.models import (
    LocalOrgConfig,
    LocalMarkdownConfig,
    LocalPlaintextConfig,
    LocalPdfConfig,
    GithubConfig,
    KhojUser,
    GithubRepoConfig,
)


@pytest.fixture(autouse=True)
def enable_db_access_for_all_tests(db):
    pass


class UserFactory(factory.django.DjangoModelFactory):
    class Meta:
        model = KhojUser

    username = factory.Faker("name")
    email = factory.Faker("email")
    password = factory.Faker("password")
    uuid = factory.Faker("uuid4")


@pytest.fixture(scope="session")
def search_config() -> SearchConfig:
    model_dir = resolve_absolute_path("~/.khoj/search")
    model_dir.mkdir(parents=True, exist_ok=True)
    search_config = SearchConfig()

    search_config.symmetric = TextSearchConfig(
        encoder="sentence-transformers/all-MiniLM-L6-v2",
        cross_encoder="cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory=model_dir / "symmetric/",
        encoder_type=None,
    )

    search_config.asymmetric = TextSearchConfig(
        encoder="sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
        cross_encoder="cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory=model_dir / "asymmetric/",
        encoder_type=None,
    )

    search_config.image = ImageSearchConfig(
        encoder="sentence-transformers/clip-ViT-B-32",
        model_directory=model_dir / "image/",
        encoder_type=None,
    )

    return search_config


@pytest.mark.django_db
@pytest.fixture
def default_user():
    return UserFactory()


@pytest.fixture(scope="session")
def search_models(search_config: SearchConfig):
    search_models = SearchModels()
    search_models.image_search = image_search.initialize_model(search_config.image)

    return search_models


@pytest.fixture
def anyio_backend():
    return "asyncio"


@pytest.mark.django_db
@pytest.fixture(scope="function")
def content_config(tmp_path_factory, search_models: SearchModels, default_user: KhojUser):
    content_dir = tmp_path_factory.mktemp("content")

    # Generate Image Embeddings from Test Images
    content_config = ContentConfig()
    content_config.image = ImageContentConfig(
        input_filter=None,
        input_directories=["tests/data/images"],
        embeddings_file=content_dir.joinpath("image_embeddings.pt"),
        batch_size=1,
        use_xmp_metadata=False,
    )

    image_search.setup(content_config.image, search_models.image_search.image_encoder, regenerate=False)

    LocalOrgConfig.objects.create(
        input_files=None,
        input_filter=["tests/data/org/*.org"],
        index_heading_entries=False,
        user=default_user,
    )

    text_search.setup(OrgToJsonl, get_sample_data("org"), regenerate=False, user=default_user)

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


@pytest.fixture(scope="session")
def processor_config(tmp_path_factory):
    openai_api_key = os.getenv("OPENAI_API_KEY")
    processor_dir = tmp_path_factory.mktemp("processor")

    # The conversation processor is the only configured processor
    # It needs an OpenAI API key to work.
    if not openai_api_key:
        return

    # Setup conversation processor, if OpenAI API key is set
    processor_config = ProcessorConfig()
    processor_config.conversation = ConversationProcessorConfig(
        openai=OpenAIProcessorConfig(api_key=openai_api_key),
        conversation_logfile=processor_dir.joinpath("conversation_logs.json"),
    )

    return processor_config


@pytest.fixture(scope="session")
def processor_config_offline_chat(tmp_path_factory):
    processor_dir = tmp_path_factory.mktemp("processor")

    # Setup conversation processor
    processor_config = ProcessorConfig()
    processor_config.conversation = ConversationProcessorConfig(
        enable_offline_chat=True,
        conversation_logfile=processor_dir.joinpath("conversation_logs.json"),
    )

    return processor_config


@pytest.fixture(scope="session")
def chat_client(md_content_config: ContentConfig, search_config: SearchConfig, processor_config: ProcessorConfig):
    # Initialize app state
    state.config.search_type = search_config
    state.SearchType = configure_search_types(state.config)

    # Index Markdown Content for Search
    all_files = fs_syncer.collect_files()
    state.content_index = configure_content(
        state.content_index, state.config.content_type, all_files, state.search_models
    )

    # Initialize Processor from Config
    state.processor_config = configure_processor(processor_config)
    state.anonymous_mode = True

    app = FastAPI()

    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")
    return TestClient(app)


@pytest.fixture(scope="function")
def fastapi_app():
    app = FastAPI()
    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")
    return app


@pytest.fixture(scope="function")
def client(
    content_config: ContentConfig,
    search_config: SearchConfig,
    processor_config: ProcessorConfig,
    default_user: KhojUser,
):
    state.config.content_type = content_config
    state.config.search_type = search_config
    state.SearchType = configure_search_types(state.config)

    # These lines help us Mock the Search models for these search types
    state.search_models.image_search = image_search.initialize_model(search_config.image)
    text_search.setup(
        OrgToJsonl,
        get_sample_data("org"),
        regenerate=False,
        user=default_user,
    )
    state.content_index.image = image_search.setup(
        content_config.image, state.search_models.image_search, regenerate=False
    )
    text_search.setup(
        PlaintextToJsonl,
        get_sample_data("plaintext"),
        regenerate=False,
        user=default_user,
    )

    state.processor_config = configure_processor(processor_config)
    state.anonymous_mode = True

    configure_routes(app)
    configure_middleware(app)
    app.mount("/static", StaticFiles(directory=web_directory), name="static")
    return TestClient(app)


@pytest.fixture(scope="function")
def client_offline_chat(
    search_config: SearchConfig,
    processor_config_offline_chat: ProcessorConfig,
    content_config: ContentConfig,
    md_content_config,
):
    # Initialize app state
    state.config.content_type = md_content_config
    state.config.search_type = search_config
    state.SearchType = configure_search_types(state.config)

    # Index Markdown Content for Search
    state.search_models.image_search = image_search.initialize_model(search_config.image)

    all_files = fs_syncer.collect_files(state.config.content_type)
    state.content_index = configure_content(
        state.content_index, state.config.content_type, all_files, state.search_models
    )

    # Initialize Processor from Config
    state.processor_config = configure_processor(processor_config_offline_chat)
    state.anonymous_mode = True

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
def sample_org_data():
    return get_sample_data("org")


def get_sample_data(type):
    sample_data = {
        "org": {
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
   #+end_src"""
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
