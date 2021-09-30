# Standard Modules
from pathlib import Path

# External Packages
import pytest
from fastapi.testclient import TestClient

# Internal Packages
from src.main import app, search_config, model
from src.search_type import asymmetric, image_search
from src.utils.config import SearchConfig, TextSearchConfig, ImageSearchConfig
from src.utils.helpers import resolve_absolute_path


# Arrange
# ----------------------------------------------------------------------------------------------------
client = TestClient(app)


# Test
# ----------------------------------------------------------------------------------------------------
def test_search_with_invalid_search_type():
    # Arrange
    user_query = "How to call semantic search from Emacs?"

    # Act
    response = client.get(f"/search?q={user_query}&t=invalid_search_type")

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
def test_search_with_valid_search_type():
    # Arrange
    for search_type in ["notes", "ledger", "music", "image"]:
        # Act
        response = client.get(f"/search?q=random&t={search_type}")
        # Assert
        assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
def test_regenerate_with_invalid_search_type():
    # Act
    response = client.get(f"/regenerate?t=invalid_search_type")

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
def test_regenerate_with_valid_search_type():
    # Arrange
    for search_type in ["notes", "ledger", "music", "image"]:
        # Act
        response = client.get(f"/regenerate?t={search_type}")
        # Assert
        assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
def test_notes_search():
    # Arrange
    search_config = SearchConfig()
    search_config.notes = TextSearchConfig(
        input_files = [Path('tests/data/main_readme.org'), Path('tests/data/interface_emacs_readme.org')],
        input_filter = None,
        compressed_jsonl = Path('tests/data/.test.jsonl.gz'),
        embeddings_file = Path('tests/data/.test_embeddings.pt'),
        verbose = 0)

    # Act
    # Regenerate embeddings during asymmetric setup
    notes_model = asymmetric.setup(search_config.notes, regenerate=True)

    # Assert
    assert len(notes_model.entries) == 10
    assert len(notes_model.corpus_embeddings) == 10

    # Arrange
    model.notes_search = notes_model
    user_query = "How to call semantic search from Emacs?"

    # Act
    response = client.get(f"/search?q={user_query}&n=1&t=notes")

    # Assert
    assert response.status_code == 200
    # assert actual_data contains "Semantic Search via Emacs"
    search_result = response.json()[0]["Entry"]
    assert "Semantic Search via Emacs" in search_result


# ----------------------------------------------------------------------------------------------------
def test_image_search():
    # Arrange
    search_config = SearchConfig()
    search_config.image = ImageSearchConfig(
        input_directory = Path('tests/data'),
        embeddings_file = Path('tests/data/.image_embeddings.pt'),
        batch_size = 10,
        use_xmp_metadata = False,
        verbose = 2)

    # Act
    model.image_search = image_search.setup(search_config.image, regenerate=True)

    # Assert
    assert len(model.image_search.image_names) == 3
    assert len(model.image_search.image_embeddings) == 3

    # Arrange
    for query, expected_image_name in [("kitten in a park", "kitten_park.jpg"),
                                       ("horse and dog in a farm", "horse_dog.jpg"),
                                       ("A guinea pig eating grass", "guineapig_grass.jpg")]:
        # Act
        hits = image_search.query(
            query,
            count = 1,
            model = model.image_search)

        results = image_search.collate_results(
            hits,
            model.image_search.image_names,
            search_config.image.input_directory,
            count=1)

        actual_image = results[0]["Entry"]
        expected_image = resolve_absolute_path(search_config.image.input_directory.joinpath(expected_image_name))

        # Assert
        assert expected_image == actual_image


# ----------------------------------------------------------------------------------------------------
def test_notes_regenerate():
    # Arrange
    search_config = SearchConfig()
    search_config.notes = TextSearchConfig(
        input_files = [Path('tests/data/main_readme.org'), Path('tests/data/interface_emacs_readme.org')],
        input_filter = None,
        compressed_jsonl = Path('tests/data/.test.jsonl.gz'),
        embeddings_file = Path('tests/data/.test_embeddings.pt'),
        verbose = 0)

    # Act
    # Regenerate embeddings during asymmetric setup
    notes_model = asymmetric.setup(search_config.notes, regenerate=True)

    # Assert
    assert len(notes_model.entries) == 10
    assert len(notes_model.corpus_embeddings) == 10

    # Arrange
    model.notes_search = notes_model

    # Act
    response = client.get(f"/regenerate?t=notes")

    # Assert
    assert response.status_code == 200
