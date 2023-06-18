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
from khoj.search_type import image_search, text_search
from khoj.utils.helpers import resolve_absolute_path
from khoj.utils.rawconfig import (
    ContentConfig,
    ConversationProcessorConfig,
    ProcessorConfig,
    TextContentConfig,
    GithubContentConfig,
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
    )

    search_config.asymmetric = TextSearchConfig(
        encoder="sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
        cross_encoder="cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory=model_dir / "asymmetric/",
    )

    search_config.image = ImageSearchConfig(
        encoder="sentence-transformers/clip-ViT-B-32", model_directory=model_dir / "image/"
    )

    return search_config


@pytest.fixture(scope="session")
def content_config(tmp_path_factory, search_config: SearchConfig):
    content_dir = tmp_path_factory.mktemp("content")

    # Generate Image Embeddings from Test Images
    content_config = ContentConfig()
    content_config.image = ImageContentConfig(
        input_directories=["tests/data/images"],
        embeddings_file=content_dir.joinpath("image_embeddings.pt"),
        batch_size=1,
        use_xmp_metadata=False,
    )

    image_search.setup(content_config.image, search_config.image, regenerate=False)

    # Generate Notes Embeddings from Test Notes
    content_config.org = TextContentConfig(
        input_files=None,
        input_filter=["tests/data/org/*.org"],
        compressed_jsonl=content_dir.joinpath("notes.jsonl"),
        embeddings_file=content_dir.joinpath("note_embeddings.pt"),
    )

    filters = [DateFilter(), WordFilter(), FileFilter()]
    text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False, filters=filters)

    content_config.plugins = {
        "plugin1": TextContentConfig(
            input_files=[content_dir.joinpath("notes.jsonl")],
            input_filter=None,
            compressed_jsonl=content_dir.joinpath("plugin.jsonl.gz"),
            embeddings_file=content_dir.joinpath("plugin_embeddings.pt"),
        )
    }

    content_config.github = GithubContentConfig(
        pat_token=os.getenv("GITHUB_PAT_TOKEN", ""),
        repo_name="lantern",
        repo_owner="khoj-ai",
        repo_branch="master",
        compressed_jsonl=content_dir.joinpath("github.jsonl.gz"),
        embeddings_file=content_dir.joinpath("github_embeddings.pt"),
    )

    filters = [DateFilter(), WordFilter(), FileFilter()]
    text_search.setup(
        JsonlToJsonl, content_config.plugins["plugin1"], search_config.asymmetric, regenerate=False, filters=filters
    )

    return content_config


@pytest.fixture(scope="session")
def md_content_config(tmp_path_factory):
    content_dir = tmp_path_factory.mktemp("content")

    # Generate Embeddings for Markdown Content
    content_config = ContentConfig()
    content_config.markdown = TextContentConfig(
        input_files=None,
        input_filter=["tests/data/markdown/*.md"],
        compressed_jsonl=content_dir.joinpath("markdown.jsonl"),
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
        openai_api_key=openai_api_key,
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
    state.model.markdown_search = text_search.setup(
        MarkdownToJsonl, md_content_config.markdown, search_config.asymmetric, regenerate=False, filters=filters
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
    state.model.org_search = {}
    state.model.image_search = {}

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
