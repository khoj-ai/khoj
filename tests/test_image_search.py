# Standard Modules
from pathlib import Path
from PIL import Image

# External Packages
import pytest

# Internal Packages
from src.utils.state import model
from src.utils.constants import web_directory
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
def test_image_search(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    output_directory = resolve_absolute_path(web_directory)
    model.image_search = image_search.setup(content_config.image, search_config.image, regenerate=False)
    query_expected_image_pairs = [("kitten", "kitten_park.jpg"),
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
            output_directory=output_directory,
            image_files_url='/static/images',
            count=1)

        actual_image_path = output_directory.joinpath(Path(results[0]["entry"]).name)
        actual_image = Image.open(actual_image_path)
        expected_image = Image.open(content_config.image.input_directories[0].joinpath(expected_image_name))

        # Assert
        assert expected_image == actual_image

    # Cleanup
    # Delete the image files copied to results directory
    actual_image_path.unlink()
