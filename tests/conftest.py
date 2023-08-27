# External Packages
import os
from copy import deepcopy
from fastapi.testclient import TestClient
from pathlib import Path
import pytest

# Internal Packages
from khoj.main import app
from khoj.configure import configure_processor, configure_routes, configure_search_types
from khoj.processor.markdown.markdown_to_jsonl import MarkdownToJsonl
from khoj.processor.plaintext.plaintext_to_jsonl import PlaintextToJsonl
from khoj.search_type import image_search, text_search
from khoj.utils.config import SearchModels
from khoj.utils.helpers import resolve_absolute_path
from khoj.utils.rawconfig import (
    ContentConfig,
    ConversationProcessorConfig,
    OpenAIProcessorConfig,
    ProcessorConfig,
    TextContentConfig,
    GithubContentConfig,
    GithubRepoConfig,
    ImageContentConfig,
    SearchConfig,
    TextSearchConfig,
    ImageSearchConfig,
)
from khoj.utils import state
from khoj.processor.jsonl.jsonl_to_jsonl import JsonlToJsonl
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from khoj.search_filter.date_filter import DateFilter
from khoj.search_filter.word_filter import WordFilter
from khoj.search_filter.file_filter import FileFilter


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


@pytest.fixture(scope="session")
def search_models(search_config: SearchConfig):
    search_models = SearchModels()
    search_models.text_search = text_search.initialize_model(search_config.asymmetric)
    search_models.image_search = image_search.initialize_model(search_config.image)

    return search_models


@pytest.fixture(scope="session")
def content_config(tmp_path_factory, search_models: SearchModels, search_config: SearchConfig):
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

    # Generate Notes Embeddings from Test Notes
    content_config.org = TextContentConfig(
        input_files=None,
        input_filter=["tests/data/org/*.org"],
        compressed_jsonl=content_dir.joinpath("notes.jsonl.gz"),
        embeddings_file=content_dir.joinpath("note_embeddings.pt"),
    )

    filters = [DateFilter(), WordFilter(), FileFilter()]
    text_search.setup(
        OrgToJsonl,
        get_sample_data("org"),
        content_config.org,
        search_models.text_search.bi_encoder,
        regenerate=False,
        filters=filters,
    )

    content_config.plugins = {
        "plugin1": TextContentConfig(
            input_files=[content_dir.joinpath("notes.jsonl.gz")],
            input_filter=None,
            compressed_jsonl=content_dir.joinpath("plugin.jsonl.gz"),
            embeddings_file=content_dir.joinpath("plugin_embeddings.pt"),
        )
    }

    if os.getenv("GITHUB_PAT_TOKEN"):
        content_config.github = GithubContentConfig(
            pat_token=os.getenv("GITHUB_PAT_TOKEN", ""),
            repos=[
                GithubRepoConfig(
                    owner="khoj-ai",
                    name="lantern",
                    branch="master",
                )
            ],
            compressed_jsonl=content_dir.joinpath("github.jsonl.gz"),
            embeddings_file=content_dir.joinpath("github_embeddings.pt"),
        )

    content_config.plaintext = TextContentConfig(
        input_files=None,
        input_filter=["tests/data/plaintext/*.txt", "tests/data/plaintext/*.md", "tests/data/plaintext/*.html"],
        compressed_jsonl=content_dir.joinpath("plaintext.jsonl.gz"),
        embeddings_file=content_dir.joinpath("plaintext_embeddings.pt"),
    )

    content_config.github = GithubContentConfig(
        pat_token=os.getenv("GITHUB_PAT_TOKEN", ""),
        repos=[
            GithubRepoConfig(
                owner="khoj-ai",
                name="lantern",
                branch="master",
            )
        ],
        compressed_jsonl=content_dir.joinpath("github.jsonl.gz"),
        embeddings_file=content_dir.joinpath("github_embeddings.pt"),
    )

    filters = [DateFilter(), WordFilter(), FileFilter()]
    text_search.setup(
        JsonlToJsonl,
        None,
        content_config.plugins["plugin1"],
        search_models.text_search.bi_encoder,
        regenerate=False,
        filters=filters,
    )

    return content_config


@pytest.fixture(scope="session")
def md_content_config(tmp_path_factory):
    content_dir = tmp_path_factory.mktemp("content")

    # Generate Embeddings for Markdown Content
    content_config = ContentConfig()
    content_config.markdown = TextContentConfig(
        input_files=None,
        input_filter=["tests/data/markdown/*.markdown"],
        compressed_jsonl=content_dir.joinpath("markdown.jsonl.gz"),
        embeddings_file=content_dir.joinpath("markdown_embeddings.pt"),
    )

    return content_config


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
    state.config.content_type = md_content_config
    state.config.search_type = search_config
    state.SearchType = configure_search_types(state.config)

    # Index Markdown Content for Search
    filters = [DateFilter(), WordFilter(), FileFilter()]
    state.search_models.text_search = text_search.initialize_model(search_config.asymmetric)
    state.content_index.markdown = text_search.setup(
        MarkdownToJsonl,
        get_sample_data("markdown"),
        md_content_config.markdown,
        state.search_models.text_search.bi_encoder,
        regenerate=False,
        filters=filters,
    )

    # Initialize Processor from Config
    state.processor_config = configure_processor(processor_config)

    configure_routes(app)
    return TestClient(app)


@pytest.fixture(scope="function")
def client(content_config: ContentConfig, search_config: SearchConfig, processor_config: ProcessorConfig):
    state.config.content_type = content_config
    state.config.search_type = search_config
    state.SearchType = configure_search_types(state.config)

    # These lines help us Mock the Search models for these search types
    state.search_models.text_search = text_search.initialize_model(search_config.asymmetric)
    state.search_models.image_search = image_search.initialize_model(search_config.image)
    state.content_index.org = text_search.setup(
        OrgToJsonl,
        get_sample_data("org"),
        content_config.org,
        state.search_models.text_search.bi_encoder,
        regenerate=False,
    )
    state.content_index.image = image_search.setup(
        content_config.image, state.search_models.image_search, regenerate=False
    )
    state.content_index.plaintext = text_search.setup(
        PlaintextToJsonl,
        get_sample_data("plaintext"),
        content_config.plaintext,
        state.search_models.text_search.bi_encoder,
        regenerate=False,
    )

    state.processor_config = configure_processor(processor_config)

    configure_routes(app)
    return TestClient(app)


@pytest.fixture(scope="function")
def client_offline_chat(
    md_content_config: ContentConfig, search_config: SearchConfig, processor_config_offline_chat: ProcessorConfig
):
    # Initialize app state
    state.config.content_type = md_content_config
    state.config.search_type = search_config
    state.SearchType = configure_search_types(state.config)

    # Index Markdown Content for Search
    filters = [DateFilter(), WordFilter(), FileFilter()]
    state.search_models.text_search = text_search.initialize_model(search_config.asymmetric)
    state.search_models.image_search = image_search.initialize_model(search_config.image)
    state.content_index.org = text_search.setup(
        OrgToJsonl,
        get_sample_data("org"),
        content_config.org,
        state.search_models.text_search.bi_encoder,
        regenerate=False,
    )
    state.content_index.image = image_search.setup(
        content_config.image, state.search_models.image_search, regenerate=False
    )

    state.content_index.markdown = text_search.setup(
        MarkdownToJsonl,
        get_sample_data("markdown"),
        md_content_config.markdown,
        state.search_models.text_search.bi_encoder,
        regenerate=False,
        filters=filters,
    )

    # Initialize Processor from Config
    state.processor_config = configure_processor(processor_config_offline_chat)

    configure_routes(app)
    return TestClient(app)


@pytest.fixture(scope="function")
def new_org_file(content_config: ContentConfig):
    # Setup
    new_org_file = Path(content_config.org.input_filter[0]).parent / "new_file.org"
    new_org_file.touch()

    yield new_org_file

    # Cleanup
    if new_org_file.exists():
        new_org_file.unlink()


@pytest.fixture(scope="function")
def org_config_with_only_new_file(content_config: ContentConfig, new_org_file: Path):
    new_org_config = deepcopy(content_config.org)
    new_org_config.input_files = [f"{new_org_file}"]
    new_org_config.input_filter = None
    return new_org_config


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
