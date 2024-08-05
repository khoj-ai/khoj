# Standard Modules
from urllib.parse import quote

import pytest

from khoj.database.models import KhojApiUser, KhojUser
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.search_type import text_search


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_search_for_user2_returns_empty(client, api_user2: KhojApiUser):
    token = api_user2.token
    headers = {"Authorization": f"Bearer {token}"}
    for content_type in ["all", "org", "markdown", "pdf", "github", "notion", "plaintext"]:
        # Act
        response = client.get(f"/api/search?q=random&t={content_type}", headers=headers)
        # Assert
        assert response.text == "[]"
        assert response.status_code == 200, f"Returned status: {response.status_code} for content type: {content_type}"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db(transaction=True)
def test_index_update_with_user2(client, api_user2: KhojApiUser):
    # Arrange
    files = get_sample_files_data()
    source_file_symbol = set([f[1][0] for f in files])

    headers = {"Authorization": f"Bearer {api_user2.token}"}
    update_response = client.patch("/api/content", files=files, headers=headers)
    search_response = client.get("/api/search?q=hardware&t=all", headers=headers)
    results = search_response.json()

    # Assert
    assert update_response.status_code == 200
    assert len(results) == 5
    for result in results:
        assert result["additional"]["file"] in source_file_symbol


@pytest.mark.django_db(transaction=True)
def test_index_update_with_user2_inaccessible_user1(client, api_user2: KhojApiUser, api_user: KhojApiUser):
    # Arrange
    files = get_sample_files_data()
    source_file_symbol = set([f[1][0] for f in files])

    headers = {"Authorization": f"Bearer {api_user2.token}"}
    update_response = client.patch("/api/content", files=files, headers=headers)

    # Act
    headers = {"Authorization": f"Bearer {api_user.token}"}
    search_response = client.get("/api/search?q=hardware&t=all", headers=headers)
    results = search_response.json()

    # Assert
    assert update_response.status_code == 200
    assert len(results) == 3
    for result in results:
        assert result["additional"]["file"] not in source_file_symbol


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
            ("path/to/filename1.md", "## Studying anthropological records from the Fatimid caliphate", "text/markdown"),
        ),
        ("files", ("path/to/filename2.md", "**Understanding science through the lens of art**", "text/markdown")),
    ]
