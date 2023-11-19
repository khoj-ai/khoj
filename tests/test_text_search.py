# System Packages
import logging
from pathlib import Path
import os
import asyncio

# External Packages
import pytest

# Internal Packages
from khoj.search_type import text_search
from khoj.utils.rawconfig import ContentConfig, SearchConfig
from khoj.processor.org_mode.org_to_entries import OrgToEntries
from khoj.processor.github.github_to_entries import GithubToEntries
from khoj.utils.fs_syncer import collect_files, get_org_files
from database.models import LocalOrgConfig, KhojUser, Entry, GithubConfig

logger = logging.getLogger(__name__)


# Test
# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_search_setup_with_missing_file_raises_error(org_config_with_only_new_file: LocalOrgConfig):
    # Arrange
    # Ensure file mentioned in org.input-files is missing
    single_new_file = Path(org_config_with_only_new_file.input_files[0])
    single_new_file.unlink()

    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(FileNotFoundError):
        get_org_files(org_config_with_only_new_file)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_get_org_files_with_org_suffixed_dir_doesnt_raise_error(tmp_path, default_user: KhojUser):
    # Arrange
    orgfile = tmp_path / "directory.org" / "file.org"
    orgfile.parent.mkdir()
    with open(orgfile, "w") as f:
        f.write("* Heading\n- List item\n")

    LocalOrgConfig.objects.create(
        input_filter=[f"{tmp_path}/**/*"],
        input_files=None,
        user=default_user,
    )

    # Act
    org_files = collect_files(user=default_user)["org"]

    # Assert
    # should return orgfile and not raise IsADirectoryError
    assert org_files == {f"{orgfile}": "* Heading\n- List item\n"}


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_search_setup_with_empty_file_creates_no_entries(
    org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser, caplog
):
    # Arrange
    data = get_org_files(org_config_with_only_new_file)

    # Act
    # Generate notes embeddings during asymmetric setup
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)

    # Assert
    assert "Deleted 8 entries. Created 0 new entries for user " in caplog.records[-1].message
    verify_embeddings(0, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_indexer_deletes_embedding_before_regenerate(
    content_config: ContentConfig, default_user: KhojUser, caplog
):
    # Arrange
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    data = get_org_files(org_config)

    # Act
    # Generate notes embeddings during asymmetric setup
    with caplog.at_level(logging.DEBUG):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)

    # Assert
    assert "Deleting all entries for file type org" in caplog.text
    assert "Deleted 8 entries. Created 13 new entries for user " in caplog.records[-1].message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_search_setup_batch_processes(content_config: ContentConfig, default_user: KhojUser, caplog):
    # Arrange
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    data = get_org_files(org_config)

    # Act
    # Generate notes embeddings during asymmetric setup
    with caplog.at_level(logging.DEBUG):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)

    # Assert
    assert "Deleted 8 entries. Created 13 new entries for user " in caplog.records[-1].message


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_index_same_if_content_unchanged(content_config: ContentConfig, default_user: KhojUser, caplog):
    # Arrange
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    data = get_org_files(org_config)

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
    assert "Deleting all entries for file type org" in initial_logs
    assert "Deleting all entries for file type org" not in final_logs


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
@pytest.mark.anyio
# @pytest.mark.asyncio
async def test_text_search(search_config: SearchConfig):
    # Arrange
    default_user = await KhojUser.objects.acreate(
        username="test_user", password="test_password", email="test@example.com"
    )
    org_config = await LocalOrgConfig.objects.acreate(
        input_files=None,
        input_filter=["tests/data/org/*.org"],
        index_heading_entries=False,
        user=default_user,
    )
    data = get_org_files(org_config)

    loop = asyncio.get_event_loop()
    await loop.run_in_executor(
        None,
        text_search.setup,
        OrgToEntries,
        data,
        True,
        True,
        default_user,
    )

    query = "Load Khoj on Emacs?"

    # Act
    hits = await text_search.query(default_user, query)
    results = text_search.collate_results(hits)
    results = sorted(results, key=lambda x: float(x.score))[:1]

    # Assert
    search_result = results[0].entry
    assert "Emacs load path" in search_result, 'Expected "Emacs load path" in entry'


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_entry_chunking_by_max_tokens(org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser, caplog):
    # Arrange
    # Insert org-mode entry with size exceeding max token limit to new org file
    max_tokens = 256
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])
    with open(new_file_to_index, "w") as f:
        f.write(f"* Entry more than {max_tokens} words\n")
        for index in range(max_tokens + 1):
            f.write(f"{index} ")

    data = get_org_files(org_config_with_only_new_file)

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)

    # Assert
    assert (
        "Deleted 0 entries. Created 2 new entries for user " in caplog.records[-1].message
    ), "new entry not split by max tokens"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_entry_chunking_by_max_tokens_not_full_corpus(
    org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser, caplog
):
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
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])
    with open(new_file_to_index, "w") as f:
        f.write(f"* Entry more than {max_tokens} words\n")
        for index in range(max_tokens + 1):
            f.write(f"{index} ")

    data = get_org_files(org_config_with_only_new_file)

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    with caplog.at_level(logging.INFO):
        text_search.setup(
            OrgToEntries,
            data,
            regenerate=False,
            full_corpus=False,
            user=default_user,
        )

    # Assert
    assert (
        "Deleted 0 entries. Created 2 new entries for user " in caplog.records[-1].message
    ), "new entry not split by max tokens"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_regenerate_index_with_new_entry(
    content_config: ContentConfig, new_org_file: Path, default_user: KhojUser, caplog
):
    # Arrange
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    initial_data = get_org_files(org_config)

    # append org-mode entry to first org input file in config
    org_config.input_files = [f"{new_org_file}"]
    with open(new_org_file, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    final_data = get_org_files(org_config)

    # Act
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, initial_data, regenerate=True, user=default_user)
    initial_logs = caplog.text
    caplog.clear()  # Clear logs

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, final_data, regenerate=True, user=default_user)
    final_logs = caplog.text

    # Assert
    assert "Deleted 8 entries. Created 13 new entries for user " in initial_logs
    assert "Deleted 13 entries. Created 14 new entries for user " in final_logs
    verify_embeddings(14, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_duplicate_entries_in_stable_order(
    org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser, caplog
):
    # Arrange
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])

    # Insert org-mode entries with same compiled form into new org file
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    with open(new_file_to_index, "w") as f:
        f.write(f"{new_entry}{new_entry}")

    data = get_org_files(org_config_with_only_new_file)

    # Act
    # generate embeddings, entries, notes model from scratch after adding new org-mode file
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)
    initial_logs = caplog.text
    caplog.clear()  # Clear logs

    data = get_org_files(org_config_with_only_new_file)

    # update embeddings, entries, notes model with no new changes
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)
    final_logs = caplog.text

    # Assert
    # verify only 1 entry added even if there are multiple duplicate entries
    assert "Deleted 8 entries. Created 1 new entries for user " in initial_logs
    assert "Deleted 0 entries. Created 0 new entries for user " in final_logs

    verify_embeddings(1, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_deleted_entry(org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser, caplog):
    # Arrange
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])

    # Insert org-mode entries with same compiled form into new org file
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    with open(new_file_to_index, "w") as f:
        f.write(f"{new_entry}{new_entry} -- Tatooine")
    initial_data = get_org_files(org_config_with_only_new_file)

    # update embeddings, entries, notes model after removing an entry from the org file
    with open(new_file_to_index, "w") as f:
        f.write(f"{new_entry}")

    final_data = get_org_files(org_config_with_only_new_file)

    # Act
    # load embeddings, entries, notes model after adding new org file with 2 entries
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, initial_data, regenerate=True, user=default_user)
    initial_logs = caplog.text
    caplog.clear()  # Clear logs

    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, final_data, regenerate=False, user=default_user)
    final_logs = caplog.text

    # Assert
    # verify only 1 entry added even if there are multiple duplicate entries
    assert "Deleted 8 entries. Created 2 new entries for user " in initial_logs
    assert "Deleted 1 entries. Created 0 new entries for user " in final_logs

    verify_embeddings(1, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_new_entry(content_config: ContentConfig, new_org_file: Path, default_user: KhojUser, caplog):
    # Arrange
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    data = get_org_files(org_config)
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)
    initial_logs = caplog.text
    caplog.clear()  # Clear logs

    # append org-mode entry to first org input file in config
    with open(new_org_file, "w") as f:
        new_entry = "\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
        f.write(new_entry)

    data = get_org_files(org_config)

    # Act
    # update embeddings, entries with the newly added note
    with caplog.at_level(logging.INFO):
        text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)
    final_logs = caplog.text

    # Assert
    assert "Deleted 8 entries. Created 13 new entries for user " in initial_logs
    assert "Deleted 0 entries. Created 1 new entries for user " in final_logs
    verify_embeddings(14, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(os.getenv("GITHUB_PAT_TOKEN") is None, reason="GITHUB_PAT_TOKEN not set")
def test_text_search_setup_github(content_config: ContentConfig, default_user: KhojUser):
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
