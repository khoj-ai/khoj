# Standard Packages
import pytest

# Internal Packages
from src.search_type import image_search, text_search
from src.utils.rawconfig import ContentConfig, TextContentConfig, ImageContentConfig, SearchConfig, TextSearchConfig, ImageSearchConfig
from src.processor.org_mode.org_to_jsonl import org_to_jsonl
from src.utils import state


@pytest.fixture(scope='session')
def search_config(tmp_path_factory):
    model_dir = tmp_path_factory.mktemp('data')

    search_config = SearchConfig()

    search_config.symmetric = TextSearchConfig(
        encoder = "sentence-transformers/all-MiniLM-L6-v2",
        cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory = model_dir
    )

    search_config.asymmetric = TextSearchConfig(
        encoder = "sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
        cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory = model_dir
    )

    search_config.image = ImageSearchConfig(
        encoder = "sentence-transformers/clip-ViT-B-32",
        model_directory = model_dir
    )

    return search_config


@pytest.fixture(scope='session')
def model_dir(search_config):
    model_dir = search_config.asymmetric.model_directory

    # Generate Image Embeddings from Test Images
    # content_config = ContentConfig()
    # content_config.image = ImageContentConfig(
    #     input_directories = ['tests/data/images'],
    #     embeddings_file = model_dir.joinpath('image_embeddings.pt'),
    #     batch_size = 10,
    #     use_xmp_metadata = False)

    # image_search.setup(content_config.image, search_config.image, regenerate=False, verbose=True)

    # Generate Notes Embeddings from Test Notes
    content_config.org = TextContentConfig(
        input_files = None,
        input_filter = 'tests/data/org/*.org',
        compressed_jsonl = model_dir.joinpath('notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('note_embeddings.pt'))

    text_search.setup(org_to_jsonl, content_config.org, search_config.asymmetric, regenerate=False, verbose=True)

    return model_dir


@pytest.fixture(scope='session')
def content_config(model_dir):
    content_config = ContentConfig()
    content_config.org = TextContentConfig(
        input_files = None,
        input_filter = 'tests/data/org/*.org',
        compressed_jsonl = model_dir.joinpath('notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('note_embeddings.pt'))

    # content_config.image = ImageContentConfig(
    #     input_directories = ['tests/data/images'],
    #     embeddings_file = model_dir.joinpath('image_embeddings.pt'),
    #     batch_size = 10,
    #     use_xmp_metadata = False)

    return content_config