# System Packages
import asyncio
import logging
import os
from pathlib import Path

import pytest

from khoj.database.adapters import EntryAdapters
from khoj.database.models import Entry, GithubConfig, KhojUser, LocalOrgConfig
from khoj.processor.content.docx.docx_to_entries import DocxToEntries
from khoj.processor.content.github.github_to_entries import GithubToEntries
from khoj.processor.content.images.image_to_entries import ImageToEntries
from khoj.processor.content.markdown.markdown_to_entries import MarkdownToEntries
from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.pdf.pdf_to_entries import PdfToEntries
from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.search_type import text_search
from khoj.utils.fs_syncer import collect_files, get_org_files
from khoj.utils.rawconfig import ContentConfig, SearchConfig

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
    org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser
):
    # Arrange
    existing_entries = Entry.objects.filter(user=default_user).count()
    data = get_org_files(org_config_with_only_new_file)

    # Act
    # Generate notes embeddings during asymmetric setup
    text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)

    # Assert
    updated_entries = Entry.objects.filter(user=default_user).count()

    assert existing_entries == 2
    assert updated_entries == 0
    verify_embeddings(0, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_text_indexer_deletes_embedding_before_regenerate(
    content_config: ContentConfig, default_user: KhojUser, caplog
):
    # Arrange
    existing_entries = Entry.objects.filter(user=default_user).count()
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    data = get_org_files(org_config)

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
def test_text_index_same_if_content_unchanged(content_config: ContentConfig, default_user: KhojUser, caplog):
    # Arrange
    existing_entries = Entry.objects.filter(user=default_user)
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
    updated_entries = Entry.objects.filter(user=default_user)
    for entry in updated_entries:
        assert entry in existing_entries
    assert len(existing_entries) == len(updated_entries)
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
        "Deleted 0 entries. Created 3 new entries for user " in caplog.records[-1].message
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
            user=default_user,
        )

    # Assert
    assert (
        "Deleted 0 entries. Created 3 new entries for user " in caplog.records[-1].message
    ), "new entry not split by max tokens"


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_regenerate_index_with_new_entry(content_config: ContentConfig, new_org_file: Path, default_user: KhojUser):
    # Arrange
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    initial_data = get_org_files(org_config)

    # append org-mode entry to first org input file in config
    org_config.input_files = [f"{new_org_file}"]
    with open(new_org_file, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    final_data = get_org_files(org_config)

    # Act
    text_search.setup(OrgToEntries, initial_data, regenerate=True, user=default_user)
    updated_entries1 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    text_search.setup(OrgToEntries, final_data, regenerate=True, user=default_user)
    updated_entries2 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    for entry in updated_entries1:
        assert entry in updated_entries2

    assert not any([new_org_file.name in entry for entry in updated_entries1])
    assert not any([new_org_file.name in entry for entry in existing_entries])
    assert any([new_org_file.name in entry for entry in updated_entries2])

    assert any(
        ["Saw a super cute video of a chihuahua doing the Tango on Youtube" in entry for entry in updated_entries2]
    )
    verify_embeddings(3, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_duplicate_entries_in_stable_order(
    org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser
):
    # Arrange
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])

    # Insert org-mode entries with same compiled form into new org file
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    with open(new_file_to_index, "w") as f:
        f.write(f"{new_entry}{new_entry}")

    data = get_org_files(org_config_with_only_new_file)

    # Act
    # generate embeddings, entries, notes model from scratch after adding new org-mode file
    text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)
    updated_entries1 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    data = get_org_files(org_config_with_only_new_file)

    # update embeddings, entries, notes model with no new changes
    text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)
    updated_entries2 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    # verify only 1 entry added even if there are multiple duplicate entries
    for entry in existing_entries:
        assert entry not in updated_entries1

    for entry in updated_entries1:
        assert entry in updated_entries2

    assert len(existing_entries) == 2
    assert len(updated_entries1) == len(updated_entries2)
    verify_embeddings(1, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
def test_update_index_with_deleted_entry(org_config_with_only_new_file: LocalOrgConfig, default_user: KhojUser):
    # Arrange
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))
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
def test_update_index_with_new_entry(content_config: ContentConfig, new_org_file: Path, default_user: KhojUser):
    # Arrange
    existing_entries = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))
    org_config = LocalOrgConfig.objects.filter(user=default_user).first()
    data = get_org_files(org_config)
    text_search.setup(OrgToEntries, data, regenerate=True, user=default_user)

    # append org-mode entry to first org input file in config
    with open(new_org_file, "w") as f:
        new_entry = "\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
        f.write(new_entry)

    data = get_org_files(org_config)

    # Act
    # update embeddings, entries with the newly added note
    text_search.setup(OrgToEntries, data, regenerate=False, user=default_user)
    updated_entries1 = list(Entry.objects.filter(user=default_user).values_list("compiled", flat=True))

    # Assert
    for entry in existing_entries:
        assert entry not in updated_entries1
    assert len(updated_entries1) == len(existing_entries) + 1
    verify_embeddings(3, default_user)


# ----------------------------------------------------------------------------------------------------
@pytest.mark.django_db
@pytest.mark.parametrize(
    "text_to_entries",
    [
        (OrgToEntries),
    ],
)
def test_update_index_with_deleted_file(
    org_config_with_only_new_file: LocalOrgConfig, text_to_entries: TextToEntries, default_user: KhojUser
):
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
