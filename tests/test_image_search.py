# Standard Modules
import logging
from pathlib import Path
from PIL import Image

# External Packages
import pytest
from khoj.utils.config import SearchModels

# Internal Packages
from khoj.utils.state import content_index, search_models
from khoj.utils.constants import web_directory
from khoj.search_type import image_search
from khoj.utils.helpers import resolve_absolute_path
from khoj.utils.rawconfig import ContentConfig, SearchConfig


# Test
# ----------------------------------------------------------------------------------------------------
def test_image_search_setup(content_config: ContentConfig, search_models: SearchModels):
    # Act
    # Regenerate image search embeddings during image setup
    image_search_model = image_search.setup(
        content_config.image, search_models.image_search.image_encoder, regenerate=True
    )

    # Assert
    assert len(image_search_model.image_names) == 3
    assert len(image_search_model.image_embeddings) == 3


# ----------------------------------------------------------------------------------------------------
def test_image_metadata(content_config: ContentConfig):
    "Verify XMP Description and Subjects Extracted from Image"
    # Arrange
    expected_metadata_image_name_pairs = [
        (["Billi Ka Bacha.", "Cat", "Grass"], "kitten_park.jpg"),
        (["Pasture.", "Horse", "Dog"], "horse_dog.jpg"),
        (["Guinea Pig Eating Celery.", "Rodent", "Whiskers"], "guineapig_grass.jpg"),
    ]

    test_image_paths = [
        Path(content_config.image.input_directories[0] / image_name[1])
        for image_name in expected_metadata_image_name_pairs
    ]

    for expected_metadata, test_image_path in zip(expected_metadata_image_name_pairs, test_image_paths):
        # Act
        actual_metadata = image_search.extract_metadata(test_image_path)

        # Assert
        for expected_snippet in expected_metadata[0]:
            assert expected_snippet in actual_metadata


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
async def test_image_search(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    search_models.image_search = image_search.initialize_model(search_config.image)
    content_index.image = image_search.setup(
        content_config.image, search_models.image_search.image_encoder, regenerate=False
    )
    output_directory = resolve_absolute_path(web_directory)
    query_expected_image_pairs = [
        ("kitten", "kitten_park.jpg"),
        ("horse and dog in a farm", "horse_dog.jpg"),
        ("A guinea pig eating grass", "guineapig_grass.jpg"),
    ]

    # Act
    for query, expected_image_name in query_expected_image_pairs:
        hits = await image_search.query(
            query, count=1, search_model=search_models.image_search, content=content_index.image
        )

        results = image_search.collate_results(
            hits,
            content_index.image.image_names,
            output_directory=output_directory,
            image_files_url="/static/images",
            count=1,
        )

        actual_image_path = output_directory.joinpath(Path(results[0].entry).name)
        actual_image = Image.open(actual_image_path)
        expected_image = Image.open(content_config.image.input_directories[0].joinpath(expected_image_name))

        # Assert
        assert expected_image == actual_image

    # Cleanup
    # Delete the image files copied to results directory
    actual_image_path.unlink()


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
async def test_image_search_query_truncated(content_config: ContentConfig, search_config: SearchConfig, caplog):
    # Arrange
    search_models.image_search = image_search.initialize_model(search_config.image)
    content_index.image = image_search.setup(
        content_config.image, search_models.image_search.image_encoder, regenerate=False
    )
    max_words_supported = 10
    query = " ".join(["hello"] * 100)
    truncated_query = " ".join(["hello"] * max_words_supported)

    # Act
    try:
        with caplog.at_level(logging.INFO, logger="khoj.search_type.image_search"):
            await image_search.query(
                query, count=1, search_model=search_models.image_search, content=content_index.image
            )
    # Assert
    except RuntimeError as e:
        if "The size of tensor a (102) must match the size of tensor b (77)" in str(e):
            assert False, f"Query length exceeds max tokens supported by model\n"
    assert f"Find Images by Text: {truncated_query}" in caplog.text, "Query not truncated"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
async def test_image_search_by_filepath(content_config: ContentConfig, search_config: SearchConfig, caplog):
    # Arrange
    search_models.image_search = image_search.initialize_model(search_config.image)
    content_index.image = image_search.setup(
        content_config.image, search_models.image_search.image_encoder, regenerate=False
    )
    output_directory = resolve_absolute_path(web_directory)
    image_directory = content_config.image.input_directories[0]

    query = f"file:{image_directory.joinpath('kitten_park.jpg')}"
    expected_image_path = f"{image_directory.joinpath('kitten_park.jpg')}"

    # Act
    with caplog.at_level(logging.INFO, logger="khoj.search_type.image_search"):
        hits = await image_search.query(
            query, count=1, search_model=search_models.image_search, content=content_index.image
        )

        results = image_search.collate_results(
            hits,
            content_index.image.image_names,
            output_directory=output_directory,
            image_files_url="/static/images",
            count=1,
        )

    actual_image_path = output_directory.joinpath(Path(results[0].entry).name)
    actual_image = Image.open(actual_image_path)
    expected_image = Image.open(expected_image_path)

    # Assert
    # Ensure file search triggered instead of query with file path as string
    assert (
        f"Find Images by Image: {resolve_absolute_path(expected_image_path)}" in caplog.text
    ), "File search not triggered"
    # Ensure the correct image is returned
    assert expected_image == actual_image, "Incorrect image returned by file search"

    # Cleanup
    # Delete the image files copied to results directory
    actual_image_path.unlink()
