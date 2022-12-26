# System Packages
from copy import deepcopy
from pathlib import Path

# External Packages
import pytest

# Internal Packages
from src.utils.state import model
from src.search_type import text_search
from src.utils.rawconfig import ContentConfig, SearchConfig
from src.processor.org_mode.org_to_jsonl import OrgToJsonl


# Test
# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup_with_missing_file_raises_error(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    file_to_index = Path(content_config.org.input_filter[0]).parent / "new_file_to_index.org"
    new_org_content_config = deepcopy(content_config.org)
    new_org_content_config.input_files = [f'{file_to_index}']
    new_org_content_config.input_filter = None

    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(FileNotFoundError):
        text_search.setup(OrgToJsonl, new_org_content_config, search_config.asymmetric, regenerate=True)


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup_with_empty_file_raises_error(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    file_to_index = Path(content_config.org.input_filter[0]).parent / "new_file_to_index.org"
    file_to_index.touch()
    new_org_content_config = deepcopy(content_config.org)
    new_org_content_config.input_files = [f'{file_to_index}']
    new_org_content_config.input_filter = None

    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(ValueError, match=r'^No valid entries found*'):
        text_search.setup(OrgToJsonl, new_org_content_config, search_config.asymmetric, regenerate=True)

    # Cleanup
    # delete created test file
    file_to_index.unlink()


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup(content_config: ContentConfig, search_config: SearchConfig):
    # Act
    # Regenerate notes embeddings during asymmetric setup
    notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    # Assert
    assert len(notes_model.entries) == 10
    assert len(notes_model.corpus_embeddings) == 10


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_search(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    model.notes_search = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)
    query = "How to git install application?"

    # Act
    hits, entries = text_search.query(
        query,
        model = model.notes_search,
        rank_results=True)

    results = text_search.collate_results(
        hits,
        entries,
        count=1)

    # Assert
    # Actual_data should contain "Khoj via Emacs" entry
    search_result = results[0].entry
    assert "git clone" in search_result


# ----------------------------------------------------------------------------------------------------
def test_entry_chunking_by_max_tokens(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    initial_notes_model= text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    file_to_add_on_reload = Path(content_config.org.input_filter[0]).parent / "entry_exceeding_max_tokens.org"
    content_config.org.input_files = [f'{file_to_add_on_reload}']

    # Insert org-mode entry with size exceeding max token limit to new org file
    max_tokens = 256
    with open(file_to_add_on_reload, "w") as f:
        f.write(f"* Entry more than {max_tokens} words\n")
        for index in range(max_tokens+1):
            f.write(f"{index} ")

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False)

    # Assert
    # verify newly added org-mode entry is split by max tokens
    assert len(initial_notes_model.entries) == 12
    assert len(initial_notes_model.corpus_embeddings) == 12

    # Cleanup
    # delete reload test file added
    content_config.org.input_files = []
    file_to_add_on_reload.unlink()


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_reload(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    initial_notes_model= text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    file_to_add_on_reload = Path(content_config.org.input_filter[0]).parent / "reload.org"
    content_config.org.input_files = [f'{file_to_add_on_reload}']

    # append Org-Mode Entry to first Org Input File in Config
    with open(file_to_add_on_reload, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    regenerated_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    # Act
    # reload embeddings, entries, notes model from previously generated notes jsonl and model embeddings files
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False)

    # Assert
    assert len(regenerated_notes_model.entries) == 11
    assert len(regenerated_notes_model.corpus_embeddings) == 11

    # verify new entry loaded from updated embeddings, entries
    assert len(initial_notes_model.entries) == 11
    assert len(initial_notes_model.corpus_embeddings) == 11

    # Cleanup
    # delete reload test file added
    content_config.org.input_files = []
    file_to_add_on_reload.unlink()


# ----------------------------------------------------------------------------------------------------
def test_incremental_update(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    file_to_add_on_update = Path(content_config.org.input_filter[0]).parent / "update.org"
    content_config.org.input_files = [f'{file_to_add_on_update}']

    # append Org-Mode Entry to first Org Input File in Config
    with open(file_to_add_on_update, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # Act
    # update embeddings, entries with the newly added note
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False)

    # verify new entry added in updated embeddings, entries
    assert len(initial_notes_model.entries) == 11
    assert len(initial_notes_model.corpus_embeddings) == 11

    # Cleanup
    # delete file added for update testing
    content_config.org.input_files = []
    file_to_add_on_update.unlink()
