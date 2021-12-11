# Standard Packages
import pytest
from pathlib import Path

# Internal Packages
from src.search_type import asymmetric, image_search
from src.utils.rawconfig import ContentTypeConfig, ImageSearchConfig, TextSearchConfig


@pytest.fixture(scope='session')
def model_dir(tmp_path_factory):
    model_dir = tmp_path_factory.mktemp('data')

    # Generate Image Embeddings from Test Images
    search_config = ContentTypeConfig()
    search_config.image = ImageSearchConfig(
        input_directory = 'tests/data',
        embeddings_file = model_dir.joinpath('.image_embeddings.pt'),
        batch_size = 10,
        use_xmp_metadata = False)

    image_search.setup(search_config.image, regenerate=False, verbose=True)

    # Generate Notes Embeddings from Test Notes
    search_config.org = TextSearchConfig(
        input_files = ['tests/data/main_readme.org', 'tests/data/interface_emacs_readme.org'],
        input_filter = None,
        compressed_jsonl = model_dir.joinpath('.notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('.note_embeddings.pt'))

    asymmetric.setup(search_config.org, regenerate=False, verbose=True)

    return model_dir


@pytest.fixture(scope='session')
def search_config(model_dir):
    search_config = ContentTypeConfig()
    search_config.org = TextSearchConfig(
        input_files = ['tests/data/main_readme.org', 'tests/data/interface_emacs_readme.org'],
        input_filter = None,
        compressed_jsonl = model_dir.joinpath('.notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('.note_embeddings.pt'))

    search_config.image = ImageSearchConfig(
        input_directory = 'tests/data',
        embeddings_file = 'tests/data/.image_embeddings.pt',
        batch_size = 10,
        use_xmp_metadata = False)

    return search_config
