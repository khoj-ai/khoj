# Standard Packages
import pytest

# Internal Packages
from src.search_type import asymmetric, image_search
from src.utils.rawconfig import ContentConfig, TextContentConfig, ImageContentConfig, SearchConfig, SymmetricSearchConfig, AsymmetricSearchConfig, ImageSearchConfig


@pytest.fixture(scope='session')
def search_config(tmp_path_factory):
    model_dir = tmp_path_factory.mktemp('data')

    search_config = SearchConfig()

    search_config.asymmetric = SymmetricSearchConfig(
        encoder = "sentence-transformers/paraphrase-MiniLM-L6-v2",
        cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory = model_dir
    )

    search_config.asymmetric = AsymmetricSearchConfig(
        encoder = "sentence-transformers/msmarco-MiniLM-L-6-v3",
        cross_encoder = "cross-encoder/ms-marco-MiniLM-L-6-v2",
        model_directory = model_dir
    )

    search_config.image = ImageSearchConfig(
        encoder = "clip-ViT-B-32",
        model_directory = model_dir
    )

    return search_config


@pytest.fixture(scope='session')
def model_dir(search_config):
    model_dir = search_config.asymmetric.model_directory

    # Generate Image Embeddings from Test Images
    content_config = ContentConfig()
    content_config.image = ImageContentConfig(
        input_directory = 'tests/data',
        embeddings_file = model_dir.joinpath('.image_embeddings.pt'),
        batch_size = 10,
        use_xmp_metadata = False)

    image_search.setup(content_config.image, search_config.image, regenerate=False, verbose=True)

    # Generate Notes Embeddings from Test Notes
    content_config.org = TextContentConfig(
        input_files = ['tests/data/main_readme.org', 'tests/data/interface_emacs_readme.org'],
        input_filter = None,
        compressed_jsonl = model_dir.joinpath('.notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('.note_embeddings.pt'))

    asymmetric.setup(content_config.org, search_config.asymmetric, regenerate=False, verbose=True)

    return model_dir


@pytest.fixture(scope='session')
def content_config(model_dir):
    content_config = ContentConfig()
    content_config.org = TextContentConfig(
        input_files = ['tests/data/main_readme.org', 'tests/data/interface_emacs_readme.org'],
        input_filter = None,
        compressed_jsonl = model_dir.joinpath('.notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('.note_embeddings.pt'))

    content_config.image = ImageContentConfig(
        input_directory = 'tests/data',
        embeddings_file = model_dir.joinpath('.image_embeddings.pt'),
        batch_size = 10,
        use_xmp_metadata = False)

    return content_config
