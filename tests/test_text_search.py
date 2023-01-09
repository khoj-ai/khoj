# System Packages
from copy import deepcopy
from pathlib import Path

# External Packages
import pytest

# Internal Packages
from src.utils.state import model
from src.search_type import text_search
from src.utils.rawconfig import ContentConfig, SearchConfig, TextContentConfig
from src.processor.org_mode.org_to_jsonl import OrgToJsonl


# Test
# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup_with_missing_file_raises_error(org_config_with_only_new_file: TextContentConfig, search_config: SearchConfig):
    # Arrange
    # Ensure file mentioned in org.input-files is missing
    single_new_file = Path(org_config_with_only_new_file.input_files[0])
    single_new_file.unlink()

    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(FileNotFoundError):
        text_search.setup(OrgToJsonl, org_config_with_only_new_file, search_config.asymmetric, regenerate=True)


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup_with_empty_file_raises_error(org_config_with_only_new_file: TextContentConfig, search_config: SearchConfig):
    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(ValueError, match=r'^No valid entries found*'):
        text_search.setup(OrgToJsonl, org_config_with_only_new_file, search_config.asymmetric, regenerate=True)


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
def test_entry_chunking_by_max_tokens(org_config_with_only_new_file: TextContentConfig, search_config: SearchConfig):
    # Arrange
    # Insert org-mode entry with size exceeding max token limit to new org file
    max_tokens = 256
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])
    with open(new_file_to_index, "w") as f:
        f.write(f"* Entry more than {max_tokens} words\n")
        for index in range(max_tokens+1):
            f.write(f"{index} ")

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    initial_notes_model = text_search.setup(OrgToJsonl, org_config_with_only_new_file, search_config.asymmetric, regenerate=False)

    # Assert
    # verify newly added org-mode entry is split by max tokens
    assert len(initial_notes_model.entries) == 2
    assert len(initial_notes_model.corpus_embeddings) == 2


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_reload(content_config: ContentConfig, search_config: SearchConfig, new_org_file: Path):
    # Arrange
    initial_notes_model= text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    # append org-mode entry to first org input file in config
    content_config.org.input_files = [f'{new_org_file}']
    with open(new_org_file, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    regenerated_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    # Act
    # reload embeddings, entries, notes model from previously generated notes jsonl and model embeddings files
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False)

    # Assert
    assert len(regenerated_notes_model.entries) == 11
    assert len(regenerated_notes_model.corpus_embeddings) == 11

    # Assert
    # verify new entry loaded from updated embeddings, entries
    assert len(initial_notes_model.entries) == 11
    assert len(initial_notes_model.corpus_embeddings) == 11

    # Cleanup
    # reset input_files in config to empty list
    content_config.org.input_files = []


# ----------------------------------------------------------------------------------------------------
def test_incremental_update(content_config: ContentConfig, search_config: SearchConfig, new_org_file: Path):
    # Arrange
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=True)

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    # append org-mode entry to first org input file in config
    with open(new_org_file, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # Act
    # update embeddings, entries with the newly added note
    content_config.org.input_files = [f'{new_org_file}']
    initial_notes_model = text_search.setup(OrgToJsonl, content_config.org, search_config.asymmetric, regenerate=False)

    # Assert
    # verify new entry added in updated embeddings, entries
    assert len(initial_notes_model.entries) == 11
    assert len(initial_notes_model.corpus_embeddings) == 11

    # Cleanup
    # reset input_files in config to empty list
    content_config.org.input_files = []
