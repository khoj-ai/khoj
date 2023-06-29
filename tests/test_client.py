# Standard Modules
from io import BytesIO
from PIL import Image
from urllib.parse import quote


# External Packages
from fastapi.testclient import TestClient

# Internal Packages
from khoj.main import app
from khoj.configure import configure_routes, configure_search_types
from khoj.utils import state
from khoj.utils.state import model, config
from khoj.search_type import text_search, image_search
from khoj.utils.rawconfig import ContentConfig, SearchConfig
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from khoj.search_filter.word_filter import WordFilter
from khoj.search_filter.file_filter import FileFilter


# Test
# ----------------------------------------------------------------------------------------------------
def test_search_with_invalid_content_type(client):
    # Arrange
    user_query = quote("How to call Khoj from Emacs?")

    # Act
    response = client.get(f"/api/search?q={user_query}&t=invalid_content_type")

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
def test_search_with_valid_content_type(client):
    for content_type in ["all", "org", "markdown", "ledger", "image", "music", "pdf", "plugin1"]:
        # Act
        response = client.get(f"/api/search?q=random&t={content_type}")
        # Assert
        assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
def test_update_with_invalid_content_type(client):
    # Act
    response = client.get(f"/api/update?t=invalid_content_type")

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
def test_update_with_valid_content_type(client):
    for content_type in ["org", "markdown", "ledger", "image", "music", "pdf", "plugin1"]:
        # Act
        response = client.get(f"/api/update?t={content_type}")
        # Assert
        assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
def test_regenerate_with_invalid_content_type(client):
    # Act
    response = client.get(f"/api/update?force=true&t=invalid_content_type")

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
def test_regenerate_with_valid_content_type(client):
    for content_type in ["org", "markdown", "ledger", "image", "music", "pdf", "plugin1"]:
        # Act
        response = client.get(f"/api/update?force=true&t={content_type}")
        # Assert
        assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
def test_get_configured_types_via_api(client):
    # Act
    response = client.get(f"/api/config/types")

    # Assert
    assert response.status_code == 200
    assert response.json() == ["all", "org", "image", "plugin1"]


# ----------------------------------------------------------------------------------------------------
def test_get_configured_types_with_only_plugin_content_config(content_config):
    # Arrange
    config.content_type = ContentConfig()
    config.content_type.plugins = content_config.plugins
    state.SearchType = configure_search_types(config)

    configure_routes(app)
    client = TestClient(app)

    # Act
    response = client.get(f"/api/config/types")

    # Assert
    assert response.status_code == 200
    assert response.json() == ["all", "plugin1"]


# ----------------------------------------------------------------------------------------------------
def test_get_configured_types_with_no_plugin_content_config(content_config):
    # Arrange
    config.content_type = content_config
    config.content_type.plugins = None
    state.SearchType = configure_search_types(config)

    configure_routes(app)
    client = TestClient(app)

    # Act
    response = client.get(f"/api/config/types")

    # Assert
    assert response.status_code == 200
    assert "plugin1" not in response.json()


# ----------------------------------------------------------------------------------------------------
def test_get_configured_types_with_no_content_config():
    # Arrange
    config.content_type = ContentConfig()
    state.SearchType = configure_search_types(config)

    configure_routes(app)
    client = TestClient(app)

    # Act
    response = client.get(f"/api/config/types")

    # Assert
    assert response.status_code == 200
    assert response.json() == ["all"]


# ----------------------------------------------------------------------------------------------------
def test_image_search(client, content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    model.image_search = image_search.setup(content_config.image, search_config.image, regenerate=False)
    query_expected_image_pairs = [
        ("kitten", "kitten_park.jpg"),
        ("a horse and dog on a leash", "horse_dog.jpg"),
        ("A guinea pig eating grass", "guineapig_grass.jpg"),
    ]

    for query, expected_image_name in query_expected_image_pairs:
        # Act
        response = client.get(f"/api/search?q={query}&n=1&t=image")

        # Assert
        assert response.status_code == 200
        actual_image = Image.open(BytesIO(client.get(response.json()[0]["entry"]).content))
        expected_image = Image.open(content_config.image.input_directories[0].joinpath(expected_image_name))

        # Assert
        assert expected_image == actual_image


# ----------------------------------------------------------------------------------------------------
def test_notes_search(client, content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    model.org_search = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False)
    user_query = quote("How to git install application?")

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org&r=true")

    # Assert
    assert response.status_code == 200
    # assert actual_data contains "Khoj via Emacs" entry
    search_result = response.json()[0]["entry"]
    assert "git clone" in search_result


# ----------------------------------------------------------------------------------------------------
def test_notes_search_with_only_filters(client, content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    filters = [WordFilter(), FileFilter()]
    model.org_search = text_search.setup(
        OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False, filters=filters
    )
    user_query = quote('+"Emacs" file:"*.org"')

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org")

    # Assert
    assert response.status_code == 200
    # assert actual_data contains word "Emacs"
    search_result = response.json()[0]["entry"]
    assert "Emacs" in search_result


# ----------------------------------------------------------------------------------------------------
def test_notes_search_with_include_filter(client, content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    filters = [WordFilter()]
    model.org_search = text_search.setup(
        OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False, filters=filters
    )
    user_query = quote('How to git install application? +"Emacs"')

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org")

    # Assert
    assert response.status_code == 200
    # assert actual_data contains word "Emacs"
    search_result = response.json()[0]["entry"]
    assert "Emacs" in search_result


# ----------------------------------------------------------------------------------------------------
def test_notes_search_with_exclude_filter(client, content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    filters = [WordFilter()]
    model.org_search = text_search.setup(
        OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False, filters=filters
    )
    user_query = quote('How to git install application? -"clone"')

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org")

    # Assert
    assert response.status_code == 200
    # assert actual_data does not contains word "Emacs"
    search_result = response.json()[0]["entry"]
    assert "clone" not in search_result
