# External Packages
from copy import deepcopy
from pathlib import Path
import pytest

# Internal Packages
from src.search_type import image_search, text_search
from src.utils.helpers import resolve_absolute_path
from src.utils.rawconfig import ContentConfig, TextContentConfig, ImageContentConfig, SearchConfig, TextSearchConfig, ImageSearchConfig
from src.processor.org_mode.org_to_jsonl import OrgToJsonl
from src.search_filter.date_filter import DateFilter
from src.search_filter.word_filter import WordFilter
from src.search_filter.file_filter import FileFilter


@pytest.fixture(scope='session')
def search_config() -> SearchConfig:
    model_dir = resolve_absolute_path('~/.khoj/search')
    model_dir.mkdir(parents=True, exist_ok=True)
    search_config = SearchConfig()

    search_config.symmetric = TextSearchConfig(
        encoder = "sentence-transformers/all-MiniLM-L6-v2",
        cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory = model_dir / 'symmetric/'
    )

    search_config.asymmetric = TextSearchConfig(
        encoder = "sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
        cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory = model_dir / 'asymmetric/'
    )

    search_config.image = ImageSearchConfig(
        encoder = "sentence-transformers/clip-ViT-B-32",
        model_directory = model_dir / 'image/'
    )

    return search_config


@pytest.fixture(scope='session')
def content_config(tmp_path_factory, search_config: SearchConfig):
    content_dir = tmp_path_factory.mktemp('content')

    # Generate Image Embeddings from Test Images
    content_config = ContentConfig()
    content_config.image = ImageContentConfig(
        input_directories = ['tests/data/images'],
        embeddings_file = content_dir.joinpath('image_embeddings.pt'),
        batch_size = 1,
        use_xmp_metadata = False)

    image_search.setup(content_config.image, search_config.image, regenerate=False)

    # Generate Notes Embeddings from Test Notes
    content_config.org = TextContentConfig(
        input_files = None,
        input_filter = ['tests/data/org/*.org'],
        compressed_jsonl = content_dir.joinpath('notes.jsonl.gz'),
        embeddings_file = content_dir.joinpath('note_embeddings.pt'))

    filters = [DateFilter(), WordFilter(), FileFilter()]
    text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False, filters=filters)

    return content_config


@pytest.fixture(scope='function')
def new_org_file(content_config: ContentConfig):
    # Setup
    new_org_file = Path(content_config.org.input_filter[0]).parent / "new_file.org"
    new_org_file.touch()

    yield new_org_file

    # Cleanup
    if new_org_file.exists():
        new_org_file.unlink()


@pytest.fixture(scope='function')
def org_config_with_only_new_file(content_config: ContentConfig, new_org_file: Path):
    new_org_config = deepcopy(content_config.org)
    new_org_config.input_files = [f'{new_org_file}']
    new_org_config.input_filter = None
    return new_org_config