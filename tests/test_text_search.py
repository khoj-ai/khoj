# System Packages
import asyncio
import logging
import os

import pytest

from khoj.database.adapters import EntryAdapters
from khoj.database.models import Entry, GithubConfig, KhojUser
from khoj.processor.content.github.github_to_entries import GithubToEntries
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.search_type import text_search
from tests.helpers import get_index_files, get_sample_data

logger = logging.getLogger(__name__)


# Test
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_search_setup_with_empty_file_creates_no_entries(search_config, default_user: KhojUser):
    # Arrange
    initial_data = {
        "test.org": "* First heading\nFirst content",
        "test2.org": "* Second heading\nSecond content",
    }
    text_search.setup(OrgToEntries, initial_data, regenerate=True, user=default_user)
    existing_entries = Entry.objects.filter(user=default_user).count()

    final_data = {"new_file.org": ""}

    # Act
    # Generate notes embeddings during asymmetric setup
    text_search.setup(OrgToEntries, final_data, regenerate=True, user=default_user)

    # Assert
    updated_entries = Entry.objects.filter(user=default_user).count()

    assert existing_entries == 2
    assert updated_entries == 0
    verify_embeddings(0, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_indexer_deletes_embedding_before_regenerate(search_config, default_user: KhojUser, caplog):
    # Arrange
    data = {
        "test1.org": "* Test heading\nTest content",
        "test2.org": "* Another heading\nAnother content",
    }
    text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)
    existing_entries = Entry.objects.filter(user=default_user).count()

    # Act
    # Generate notes embeddings during asymmetric setup
    with caplog.at_level(logging.DEBUG):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)

    # Assert
    updated_entries = Entry.objects.filter(user=default_user).count()
    assert existing_entries == 2
    assert updated_entries == 2
    assert "Deleting all entries for file type org" in caplog.text
    assert "Deleted 2 entries. Created 2 new entries for user " in caplog.records[-1].message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_index_same_if_content_unchanged(search_config, default_user: KhojUser, caplog):
    # Arrange
    existing_entries = Entry.objects.filter(user=default_user)
    data = {"test.org": "* Test heading\nTest content"}

    # Act
    # Generate initial notes embeddings during asymmetric setup
    with caplog.at_level(logging.DEBUG):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)
    initial_logs = caplog.text
    caplog.clear()  # Clear logs

    # Run asymmetric setup again with no changes to data source. Ensure index is not updated
    with caplog.at_level(logging.DEBUG):
        text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)
    final_logs = caplog.text

    # Assert
    updated_entries = Entry.objects.filter(user=default_user)
    for entry in updated_entries:
        assert entry in existing_entries
    assert len(existing_entries) == len(updated_entries)
    assert "Deleting all entries for file type org" in initial_logs
    assert "Deleting all entries for file type org" not in final_logs


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
@pytest.mark.asyncio
async def test_text_search(search_config):
    # Arrange
    default_user, _ = await KhojUser.objects.aget_or_create(
        username="test_user", password="test_password", email="test@example.com"
    )
    # Get some sample org data to index
    data = get_sample_data("org")

    loop = asyncio.get_event_loop()
    await loop.run_in_executor(
        None,
        text_search.setup,
        OrgToEntries,
        data,
        True,
        default_user,
    )

    query = "Load Khoj on Emacs?"

    # Act
    hits = await text_search.query(query, default_user)
    results = text_search.collate_results(hits)
    results = sorted(results, key=lambda x: float(x.score))[:1]

    # Assert
    search_result = results[0].entry
    assert "Emacs load path" in search_result, 'Expected "Emacs load path" in entry'


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_entry_chunking_by_max_tokens(tmp_path, search_config, default_user: KhojUser, caplog):
    # Arrange
    # Insert org-mode entry with size exceeding max token limit to new org file
    max_tokens = 256
    new_file_to_index = tmp_path / "test.org"
    content = f"* Entry more than {max_tokens} words\n"
    for index in range(max_tokens + 1):
        content += f"{index} "
    data = {str(new_file_to_index): content}

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)

    # Assert
    assert "Deleted 0 entries. Created 3 new entries for user " in caplog.records[-1].message, (
        "new entry not split by max tokens"
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_entry_chunking_by_max_tokens_not_full_corpus(tmp_path, search_config, default_user: KhojUser, caplog):
    # Arrange
    # Insert org-mode entry with size exceeding max token limit to new org file
    data = {
        "readme.org": """
* Khoj
/Allow natural language search on user content like notes, images using transformer based models/

All data is processed locally. User can interface with khoj app via [[./interface/emacs/khoj.el][Emacs]], API or Commandline

** Dependencies
- Python3
- [[https://docs.conda.io/en/latest/miniconda.html#latest-miniconda-installer-links][Miniconda]]

** Install
#+begin_src shell
git clone https://github.com/khoj-ai/khoj && cd khoj
conda env create -f environment.yml
conda activate khoj
#+end_src"""
    }
    text_search.setup(
        OrgToEntries,
        data,
        regenerate=False,
        user=default_user,
    )

    max_tokens = 256
    new_file_to_index = tmp_path / "test.org"
    content = f"* Entry more than {max_tokens} words\n"
    for index in range(max_tokens + 1):
        content += f"{index} "
    data = {str(new_file_to_index): content}

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    with caplog.at_level(logging.INFO):
        text_search.setup(
            OrgToEntries,
            data,
            regenerate=False,
            user=default_user,
        )

    # Assert
    assert "Deleted 0 entries. Created 3 new entries for user " in caplog.records[-1].message, (
        "new entry not split by max tokens"
    )


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_regenerate_index_with_new_entry(search_config, default_user: KhojUser):
    # Arrange
    # Initial indexed files
    text_search.setup(OrgToEntries, get_sample_data("org"), regenerate=True, user=default_user)
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Regenerate index with only files from test data set
    files_to_index = get_index_files()
    text_search.setup(OrgToEntries, files_to_index, regenerate=True, user=default_user)
    updated_entries1 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Act
    # Update index with the new file
    new_file = "test.org"
    new_entry = "\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    files_to_index[new_file] = new_entry

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    text_search.setup(OrgToEntries, files_to_index, regenerate=True, user=default_user)
    updated_entries2 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    for entry in updated_entries1:
        assert entry in updated_entries2

    assert not any([new_file in entry for entry in updated_entries1])
    assert not any([new_file in entry for entry in existing_entries])
    assert any([new_file in entry for entry in updated_entries2])

    assert any(
        ["Saw a super cute video of a chihuahua doing the Tango on Youtube" in entry for entry in updated_entries2]
    )
    verify_embeddings(3, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_duplicate_entries_in_stable_order(tmp_path, search_config, default_user: KhojUser):
    # Arrange
    initial_data = get_sample_data("org")
    text_search.setup(OrgToEntries, initial_data, regenerate=True, user=default_user)
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Insert org-mode entries with same compiled form into new org file
    new_file_to_index = tmp_path / "test.org"
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    # Initial data with duplicate entries
    data = {str(new_file_to_index): f"{new_entry}{new_entry}"}

    # Act
    # generate embeddings, entries, notes model from scratch after adding new org-mode file
    text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)
    updated_entries1 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # idempotent indexing when data unchanged
    text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)
    updated_entries2 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    # verify only 1 entry added even if there are multiple duplicate entries
    for entry in existing_entries:
        assert entry not in updated_entries1

    # verify the second indexing update has same entries and ordering as first
    for entry in updated_entries1:
        assert entry in updated_entries2

    assert len(existing_entries) == 2
    assert len(updated_entries1) == len(updated_entries2)
    verify_embeddings(1, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_deleted_entry(tmp_path, search_config, default_user: KhojUser):
    # Arrange
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    new_file_to_index = tmp_path / "test.org"
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"

    # Initial data with two entries
    initial_data = {str(new_file_to_index): f"{new_entry}{new_entry} -- Tatooine"}
    # Final data with only first entry, with second entry removed
    final_data = {str(new_file_to_index): f"{new_entry}"}

    # Act
    # load embeddings, entries, notes model after adding new org file with 2 entries
    text_search.setup(OrgToEntries, initial_data, regenerate=True, user=default_user)
    updated_entries1 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    text_search.setup(OrgToEntries, final_data, regenerate=False, user=default_user)
    updated_entries2 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    for entry in existing_entries:
        assert entry not in updated_entries1

    # verify the entry in updated_entries2 is a subset of updated_entries1
    for entry in updated_entries1:
        assert entry not in updated_entries2

    for entry in updated_entries2:
        assert entry in updated_entries1[0]

    verify_embeddings(1, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_new_entry(search_config, default_user: KhojUser):
    # Arrange
    # Initial indexed files
    text_search.setup(OrgToEntries, get_sample_data("org"), regenerate=True, user=default_user)
    old_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Regenerate index with only files from test data set
    files_to_index = get_index_files()
    new_entries = text_search.setup(OrgToEntries, files_to_index, regenerate=True, user=default_user)

    # Act
    # Update index with the new file
    new_file = "test.org"
    new_entry = "\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    final_data = {new_file: new_entry}

    text_search.setup(OrgToEntries, final_data, regenerate=False, user=default_user)
    updated_new_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    for old_entry in old_entries:
        assert old_entry not in updated_new_entries
    assert len(updated_new_entries) == len(new_entries) + 1
    verify_embeddings(3, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
@pytest.mark.parametrize(
    "text_to_entries",
    [
        (OrgToEntries),
    ],
)
def test_update_index_with_deleted_file(text_to_entries: TextToEntries, search_config, default_user: KhojUser):
    "Delete entries associated with new file when file path with empty content passed."
    # Arrange
    file_to_index = "test"
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    initial_data = {file_to_index: new_entry}
    final_data = {file_to_index: ""}

    # Act
    # load entries after adding file
    initial_added_entries, _ = text_search.setup(text_to_entries, initial_data, regenerate=True, user=default_user)
    initial_total_entries = EntryAdapters.get_existing_entry_hashes_by_file(default_user, file_to_index).count()

    # load entries after deleting file
    final_added_entries, final_deleted_entries = text_search.setup(
        text_to_entries, final_data, regenerate=False, user=default_user
    )
    final_total_entries = EntryAdapters.get_existing_entry_hashes_by_file(default_user, file_to_index).count()

    # Assert
    assert initial_total_entries > 0, "File entries not indexed"
    assert initial_added_entries > 0, "No entries got added"

    assert final_total_entries == 0, "File did not get deleted"
    assert final_added_entries == 0, "Entries were unexpectedly added in delete entries pass"
    assert final_deleted_entries == initial_added_entries, "All added entries were not deleted"

    verify_embeddings(0, default_user), "Embeddings still exist for user"

    # Clean up
    EntryAdapters.delete_all_entries(default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(os.getenv("GITHUB_PAT_TOKEN") is None, reason="GITHUB_PAT_TOKEN not set")
def test_text_search_setup_github(search_config, default_user: KhojUser):
    # Arrange
    github_config = GithubConfig.objects.filter(user=default_user).first()

    # Act
    # Regenerate github embeddings to test asymmetric setup without caching
    text_search.setup(
        GithubToEntries,
        {},
        regenerate=True,
        user=default_user,
        config=github_config,
    )

    # Assert
    embeddings = Entry.objects.filter(user=default_user, file_type="github").count()
    assert embeddings > 1


def verify_embeddings(expected_count, user):
    embeddings = Entry.objects.filter(user=user, file_type="org").count()
    assert embeddings == expected_count
