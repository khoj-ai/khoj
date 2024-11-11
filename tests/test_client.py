# Standard Modules
import os
from urllib.parse import quote

import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient
from PIL import Image

from khoj.configure import configure_routes, configure_search_types
from khoj.database.adapters import EntryAdapters
from khoj.database.models import KhojApiUser, KhojUser
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.search_type import text_search
from khoj.utils import state
from khoj.utils.rawconfig import ContentConfig, SearchConfig


# Test
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_search_with_no_auth_key(client):
    # Arrange
    user_query = quote("How to call Khoj from Emacs?")

    # Act
    response = client.get(f"/api/search?q={user_query}")

    # Assert
    assert response.status_code == 403


@pytest.mark.django_db(transaction=True)
def test_search_with_invalid_auth_key(client):
    # Arrange
    headers = {"Authorization": "Bearer invalid-token"}
    user_query = quote("How to call Khoj from Emacs?")

    # Act
    response = client.get(f"/api/search?q={user_query}", headers=headers)

    # Assert
    assert response.status_code == 403


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_search_with_invalid_content_type(client):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    user_query = quote("How to call Khoj from Emacs?")

    # Act
    response = client.get(f"/api/search?q={user_query}&t=invalid_content_type", headers=headers)

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_search_with_valid_content_type(client):
    headers = {"Authorization": "Bearer kk-secret"}
    for content_type in ["all", "org", "markdown", "image", "pdf", "github", "notion", "plaintext", "image", "docx"]:
        # Act
        response = client.get(f"/api/search?q=random&t={content_type}", headers=headers)
        # Assert
        assert response.status_code == 200, f"Returned status: {response.status_code} for content type: {content_type}"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_with_no_auth_key(client):
    # Arrange
    files = get_sample_files_data()

    # Act
    response = client.patch("/api/content", files=files)

    # Assert
    assert response.status_code == 403


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_with_invalid_auth_key(client):
    # Arrange
    files = get_sample_files_data()
    headers = {"Authorization": "Bearer kk-invalid-token"}

    # Act
    response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert response.status_code == 403


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_update_with_invalid_content_type(client):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}

    # Act
    response = client.get(f"/api/update?t=invalid_content_type", headers=headers)

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_regenerate_with_invalid_content_type(client):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}

    # Act
    response = client.get(f"/api/update?force=true&t=invalid_content_type", headers=headers)

    # Assert
    assert response.status_code == 422


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_big_files(client):
    # Arrange
    state.billing_enabled = True
    files = get_big_size_sample_files_data()

    # Credential for the default_user, who is subscribed
    headers = {"Authorization": "Bearer kk-secret"}

    # Act
    response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert response.status_code == 429


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_medium_file_unsubscribed(client, api_user4: KhojApiUser):
    # Arrange
    api_token = api_user4.token
    state.billing_enabled = True
    files = get_medium_size_sample_files_data()
    headers = {"Authorization": f"Bearer {api_token}"}

    # Act
    response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert response.status_code == 429


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_normal_file_unsubscribed(client, api_user4: KhojApiUser):
    # Arrange
    api_token = api_user4.token
    state.billing_enabled = True
    files = get_sample_files_data()
    headers = {"Authorization": f"Bearer {api_token}"}

    # Act
    response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_big_files_no_billing(client):
    # Arrange
    state.billing_enabled = False
    files = get_big_size_sample_files_data()
    headers = {"Authorization": "Bearer kk-secret"}

    # Act
    response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update(client):
    # Arrange
    files = get_sample_files_data()
    headers = {"Authorization": "Bearer kk-secret"}

    # Act
    response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert response.status_code == 200


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_fails_if_more_than_1000_files(client, api_user4: KhojApiUser):
    # Arrange
    api_token = api_user4.token
    state.billing_enabled = True
    files = [("files", (f"path/to/filename{i}.org", f"Symphony No {i}", "text/org")) for i in range(1001)]

    headers = {"Authorization": f"Bearer {api_token}"}

    # Act
    ok_response = client.patch("/api/content", files=files[:1000], headers=headers)
    bad_response = client.patch("/api/content", files=files, headers=headers)

    # Assert
    assert ok_response.status_code == 200
    assert bad_response.status_code == 400
    assert bad_response.content.decode("utf-8") == '{"detail":"Too many files. Maximum number of files is 1000."}'


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_regenerate_with_valid_content_type(client):
    for content_type in ["all", "org", "markdown", "image", "pdf", "notion"]:
        # Arrange
        files = get_sample_files_data()
        headers = {"Authorization": "Bearer kk-secret"}

        # Act
        response = client.patch(f"/api/content?t={content_type}", files=files, headers=headers)

        # Assert
        assert response.status_code == 200, f"Returned status: {response.status_code} for content type: {content_type}"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_regenerate_with_github_fails_without_pat(client):
    # Act
    headers = {"Authorization": "Bearer kk-secret"}
    response = client.get(f"/api/update?force=true&t=github", headers=headers)

    # Arrange
    files = get_sample_files_data()

    # Act
    response = client.patch(f"/api/content?t=github", files=files, headers=headers)

    # Assert
    assert response.status_code == 200, f"Returned status: {response.status_code} for content type: github"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_get_configured_types_via_api(client, sample_org_data, default_user3: KhojUser):
    # Act
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user3)

    enabled_types = EntryAdapters.get_unique_file_types(user=default_user3).all().values_list("file_type", flat=True)

    # Assert
    assert list(enabled_types) == ["org"]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_get_api_config_types(client, sample_org_data, default_user: KhojUser):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user)

    # Act
    response = client.get(f"/api/content/types", headers=headers)

    # Assert
    assert response.status_code == 200
    assert set(response.json()) == {"all", "org", "plaintext"}


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_get_configured_types_with_no_content_config(fastapi_app: FastAPI):
    # Arrange
    state.anonymous_mode = True
    if state.config and state.config.content_type:
        state.config.content_type = None
    state.search_models = configure_search_types()

    configure_routes(fastapi_app)
    client = TestClient(fastapi_app)

    # Act
    response = client.get(f"/api/content/types")

    # Assert
    assert response.status_code == 200
    assert response.json() == ["all"]


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_notes_search(client, search_config: SearchConfig, sample_org_data, default_user: KhojUser):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user)
    user_query = quote("How to git install application?")

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org&r=true&max_distance=0.22", headers=headers)

    # Assert
    assert response.status_code == 200

    assert len(response.json()) == 1, "Expected only 1 result"
    search_result = response.json()[0]["entry"]
    assert "git clone https://github.com/khoj-ai/khoj" in search_result, "Expected 'git clone' in search result"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_notes_search_no_results(client, search_config: SearchConfig, sample_org_data, default_user: KhojUser):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user)
    user_query = quote("How to find my goat?")

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org&r=true&max_distance=0.22", headers=headers)

    # Assert
    assert response.status_code == 200
    assert response.json() == [], "Expected no results"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_notes_search_with_only_filters(
    client, content_config: ContentConfig, search_config: SearchConfig, sample_org_data, default_user: KhojUser
):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(
        OrgToEntries,
        sample_org_data,
        regenerate=False,
        user=default_user,
    )
    user_query = quote('+"Emacs" file:"*.org"')

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org", headers=headers)

    # Assert
    assert response.status_code == 200
    # assert actual_data contains word "Emacs"
    search_result = response.json()[0]["entry"]
    assert "Emacs" in search_result


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_notes_search_with_include_filter(client, sample_org_data, default_user: KhojUser):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user)
    user_query = quote('How to git install application? +"Emacs"')

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org", headers=headers)

    # Assert
    assert response.status_code == 200
    # assert actual_data contains word "Emacs"
    search_result = response.json()[0]["entry"]
    assert "emacs" in search_result


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_notes_search_with_exclude_filter(client, sample_org_data, default_user: KhojUser):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(
        OrgToEntries,
        sample_org_data,
        regenerate=False,
        user=default_user,
    )
    user_query = quote('How to git install application? -"clone"')

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org", headers=headers)

    # Assert
    assert response.status_code == 200
    # assert actual_data does not contains word "clone"
    search_result = response.json()[0]["entry"]
    assert "clone" not in search_result


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_notes_search_requires_parent_context(
    client, search_config: SearchConfig, sample_org_data, default_user: KhojUser
):
    # Arrange
    headers = {"Authorization": "Bearer kk-secret"}
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user)
    user_query = quote("Install Khoj on Emacs")

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org&r=true&max_distance=0.22", headers=headers)

    # Assert
    assert response.status_code == 200

    assert len(response.json()) == 1, "Expected only 1 result"
    search_result = response.json()[0]["entry"]
    assert "Emacs load path" in search_result, "Expected 'Emacs load path' in search result"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_different_user_data_not_accessed(client, sample_org_data, default_user: KhojUser):
    # Arrange
    headers = {"Authorization": "Bearer kk-token"}  # Token for default_user2
    text_search.setup(OrgToEntries, sample_org_data, regenerate=False, user=default_user)
    user_query = quote("How to git install application?")

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org", headers=headers)

    # Assert
    assert response.status_code == 403
    # assert actual response has no data as the default_user is different from the user making the query (anonymous)
    assert len(response.json()) == 1 and response.json()["detail"] == "Forbidden"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_user_no_data_returns_empty(client, sample_org_data, api_user3: KhojApiUser):
    # Arrange
    token = api_user3.token
    headers = {"Authorization": "Bearer " + token}
    user_query = quote("How to git install application?")

    # Act
    response = client.get(f"/api/search?q={user_query}&n=1&t=org", headers=headers)

    # Assert
    assert response.status_code == 200
    # assert actual response has no data as the default_user3, though other users have data
    assert len(response.json()) == 0
    assert response.json() == []


@pytest.mark.skipif(os.getenv("OPENAI_API_KEY") is None, reason="requires OPENAI_API_KEY")
@pytest.mark.django_db(transaction=True)
async def test_chat_with_unauthenticated_user(chat_client_with_auth, api_user2: KhojApiUser):
    # Arrange
    query = "Hello!"
    headers = {"Authorization": f"Bearer {api_user2.token}"}

    # Act
    auth_response = chat_client_with_auth.post(f"/api/chat", json={"q": query}, headers=headers)
    no_auth_response = chat_client_with_auth.post(f"/api/chat", json={"q": query})

    # Assert
    assert auth_response.status_code == 200
    assert no_auth_response.status_code == 403


def get_sample_files_data():
    return [
        ("files", ("path/to/filename.org", "* practicing piano", "text/org")),
        ("files", ("path/to/filename1.org", "** top 3 reasons why I moved to SF", "text/org")),
        ("files", ("path/to/filename2.org", "* how to build a search engine", "text/org")),
        ("files", ("path/to/filename.pdf", "Moore's law does not apply to consumer hardware", "application/pdf")),
        ("files", ("path/to/filename1.pdf", "The sun is a ball of helium", "application/pdf")),
        ("files", ("path/to/filename2.pdf", "Effect of sunshine on baseline human happiness", "application/pdf")),
        ("files", ("path/to/filename.txt", "data,column,value", "text/plain")),
        ("files", ("path/to/filename1.txt", "<html>my first web page</html>", "text/plain")),
        ("files", ("path/to/filename2.txt", "2021-02-02 Journal Entry", "text/plain")),
        ("files", ("path/to/filename.md", "# Notes from client call", "text/markdown")),
        (
            "files",
            (
                "path/to/filename.docx",
                "## Studying anthropological records from the Fatimid caliphate",
                "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
            ),
        ),
        (
            "files",
            ("path/to/filename1.md", "## Studying anthropological records from the Fatimid caliphate", "text/markdown"),
        ),
        ("files", ("path/to/filename2.md", "**Understanding science through the lens of art**", "text/markdown")),
    ]


def get_big_size_sample_files_data():
    # a string of approximately 100 MB
    big_text = "a" * (100 * 1024 * 1024)
    return [
        (
            "files",
            ("path/to/filename.org", big_text, "text/org"),
        )
    ]


def get_medium_size_sample_files_data():
    big_text = "a" * (10 * 1024 * 1024)  # a string of approximately 10 MB
    return [
        (
            "files",
            ("path/to/filename.org", big_text, "text/org"),
        )
    ]
