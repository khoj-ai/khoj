# External Packages
import pytest

# Internal Packages
from src.main import model
from src.search_type import image_search
from src.utils.helpers import resolve_absolute_path
from src.utils.rawconfig import ContentConfig, SearchConfig


# Test
# ----------------------------------------------------------------------------------------------------
def test_image_search_setup(content_config: ContentConfig, search_config: SearchConfig):
    # Act
    # Regenerate image search embeddings during image setup
    image_search_model = image_search.setup(content_config.image, search_config.image, regenerate=True)

    # Assert
    assert len(image_search_model.image_names) == 3
    assert len(image_search_model.image_embeddings) == 3


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skip(reason="results inconsistent currently")
def test_image_search(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    model.image_search = image_search.setup(content_config.image, search_config.image, regenerate=False)
    query_expected_image_pairs = [("brown kitten next to plant", "kitten_park.jpg"),
                                  ("horse and dog in a farm", "horse_dog.jpg"),
                                  ("A guinea pig eating grass", "guineapig_grass.jpg")]

    # Act
    for query, expected_image_name in query_expected_image_pairs:
        hits = image_search.query(
            query,
            count = 1,
            model = model.image_search)

        results = image_search.collate_results(
            hits,
            model.image_search.image_names,
            content_config.image.input_directory,
            count=1)

        actual_image = results[0]["Entry"]
        expected_image = resolve_absolute_path(content_config.image.input_directory.joinpath(expected_image_name))

        # Assert
        assert expected_image == actual_image
