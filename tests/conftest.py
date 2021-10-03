# Standard Packages
import pytest
from pathlib import Path

# Internal Packages
from src.utils.config import SearchConfig, TextSearchConfig, ImageSearchConfig
from src.search_type import asymmetric, image_search


@pytest.fixture(scope='session')
def model_dir(tmp_path_factory):
    model_dir = tmp_path_factory.mktemp('data')

    # Generate Image Embeddings from Test Images
    search_config = SearchConfig()
    search_config.image = ImageSearchConfig(
        input_directory = Path('tests/data'),
        embeddings_file = model_dir.joinpath('.image_embeddings.pt'),
        batch_size = 10,
        use_xmp_metadata = False,
        verbose = 2)

    image_search.setup(search_config.image, regenerate=False)

    # Generate Notes Embeddings from Test Notes
    search_config.notes = TextSearchConfig(
        input_files = [Path('tests/data/main_readme.org'), Path('tests/data/interface_emacs_readme.org')],
        input_filter = None,
        compressed_jsonl = model_dir.joinpath('.notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('.note_embeddings.pt'),
        verbose = 0)

    asymmetric.setup(search_config.notes, regenerate=False)

    return model_dir


@pytest.fixture(scope='session')
def search_config(model_dir):
    search_config = SearchConfig()
    search_config.notes = TextSearchConfig(
        input_files = [Path('tests/data/main_readme.org'), Path('tests/data/interface_emacs_readme.org')],
        input_filter = None,
        compressed_jsonl = model_dir.joinpath('.notes.jsonl.gz'),
        embeddings_file = model_dir.joinpath('.note_embeddings.pt'),
        verbose = 2)

    search_config.image = ImageSearchConfig(
        input_directory = Path('tests/data'),
        embeddings_file = Path('tests/data/.image_embeddings.pt'),
        batch_size = 10,
        use_xmp_metadata = False,
        verbose = 2)

    return search_config
