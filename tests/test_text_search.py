# System Packages
import logging
from pathlib import Path
import os

# External Packages
import pytest
import torch
from khoj.utils.config import SearchModels

# Internal Packages
from khoj.utils.state import content_index, search_models
from khoj.search_type import text_search
from khoj.utils.rawconfig import ContentConfig, SearchConfig, TextContentConfig
from khoj.processor.org_mode.org_to_jsonl import OrgToJsonl
from khoj.processor.github.github_to_jsonl import GithubToJsonl


# Test
# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup_with_missing_file_raises_error(
    org_config_with_only_new_file: TextContentConfig, search_config: SearchConfig
):
    # Arrange
    # Ensure file mentioned in org.input-files is missing
    single_new_file = Path(org_config_with_only_new_file.input_files[0])
    single_new_file.unlink()

    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(FileNotFoundError):
        text_search.setup(OrgToJsonl, org_config_with_only_new_file, search_config.asymmetric, regenerate=True)


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup_with_empty_file_raises_error(
    org_config_with_only_new_file: TextContentConfig, search_config: SearchConfig
):
    # Act
    # Generate notes embeddings during asymmetric setup
    with pytest.raises(ValueError, match=r"^No valid entries found*"):
        text_search.setup(OrgToJsonl, org_config_with_only_new_file, search_config.asymmetric, regenerate=True)


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup(content_config: ContentConfig, search_models: SearchModels):
    # Act
    # Regenerate notes embeddings during asymmetric setup
    notes_model = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=True
    )

    # Assert
    assert len(notes_model.entries) == 10
    assert len(notes_model.corpus_embeddings) == 10


# ----------------------------------------------------------------------------------------------------
def test_text_content_index_only_updates_on_changes(content_config: ContentConfig, search_models: SearchModels, caplog):
    # Arrange
    caplog.set_level(logging.INFO, logger="khoj")

    # Act
    # Generate initial notes embeddings during asymmetric setup
    text_search.setup(OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=True)
    initial_logs = caplog.text
    caplog.clear()  # Clear logs

    # Run asymmetric setup again with no changes to data source. Ensure index is not updated
    text_search.setup(OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=False)
    final_logs = caplog.text

    # Assert
    assert "📩 Saved computed text embeddings to" in initial_logs
    assert "📩 Saved computed text embeddings to" not in final_logs


# ----------------------------------------------------------------------------------------------------
@pytest.mark.anyio
async def test_asymmetric_search(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    search_models.text_search = text_search.initialize_model(search_config.asymmetric)
    content_index.org = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=True
    )
    query = "How to git install application?"

    # Act
    hits, entries = await text_search.query(
        query, search_model=search_models.text_search, content=content_index.org, rank_results=True
    )

    results = text_search.collate_results(hits, entries, count=1)

    # Assert
    # Actual_data should contain "Khoj via Emacs" entry
    search_result = results[0].entry
    assert "git clone" in search_result


# ----------------------------------------------------------------------------------------------------
def test_entry_chunking_by_max_tokens(org_config_with_only_new_file: TextContentConfig, search_models: SearchModels):
    # Arrange
    # Insert org-mode entry with size exceeding max token limit to new org file
    max_tokens = 256
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])
    with open(new_file_to_index, "w") as f:
        f.write(f"* Entry more than {max_tokens} words\n")
        for index in range(max_tokens + 1):
            f.write(f"{index} ")

    # Act
    # reload embeddings, entries, notes model after adding new org-mode file
    initial_notes_model = text_search.setup(
        OrgToJsonl, org_config_with_only_new_file, search_models.text_search.bi_encoder, regenerate=False
    )

    # Assert
    # verify newly added org-mode entry is split by max tokens
    assert len(initial_notes_model.entries) == 2
    assert len(initial_notes_model.corpus_embeddings) == 2


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_reload(content_config: ContentConfig, search_models: SearchModels, new_org_file: Path):
    # Arrange
    initial_notes_model = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=True
    )

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    # append org-mode entry to first org input file in config
    content_config.org.input_files = [f"{new_org_file}"]
    with open(new_org_file, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    regenerated_notes_model = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=True
    )

    # Act
    # reload embeddings, entries, notes model from previously generated notes jsonl and model embeddings files
    initial_notes_model = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=False
    )

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
def test_update_index_with_duplicate_entries_in_stable_order(
    org_config_with_only_new_file: TextContentConfig, search_models: SearchModels
):
    # Arrange
    new_file_to_index = Path(org_config_with_only_new_file.input_files[0])

    # Insert org-mode entries with same compiled form into new org file
    new_entry = "* TODO A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n"
    with open(new_file_to_index, "w") as f:
        f.write(f"{new_entry}{new_entry}")

    # Act
    # load embeddings, entries, notes model after adding new org-mode file
    initial_index = text_search.setup(
        OrgToJsonl, org_config_with_only_new_file, search_models.text_search.bi_encoder, regenerate=True
    )

    # update embeddings, entries, notes model after adding new org-mode file
    updated_index = text_search.setup(
        OrgToJsonl, org_config_with_only_new_file, search_models.text_search.bi_encoder, regenerate=False
    )

    # Assert
    # verify only 1 entry added even if there are multiple duplicate entries
    assert len(initial_index.entries) == len(updated_index.entries) == 1
    assert len(initial_index.corpus_embeddings) == len(updated_index.corpus_embeddings) == 1

    # verify the same entry is added even when there are multiple duplicate entries
    error_details = compare_index(initial_index, updated_index)
    if error_details:
        pytest.fail(error_details)


# ----------------------------------------------------------------------------------------------------
def test_incremental_update(content_config: ContentConfig, search_models: SearchModels, new_org_file: Path):
    # Arrange
    initial_notes_model = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=True
    )

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    # append org-mode entry to first org input file in config
    with open(new_org_file, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # Act
    # update embeddings, entries with the newly added note
    content_config.org.input_files = [f"{new_org_file}"]
    initial_notes_model = text_search.setup(
        OrgToJsonl, content_config.org, search_models.text_search.bi_encoder, regenerate=False
    )

    # Assert
    # verify new entry added in updated embeddings, entries
    assert len(initial_notes_model.entries) == 11
    assert len(initial_notes_model.corpus_embeddings) == 11

    # Cleanup
    # reset input_files in config to empty list
    content_config.org.input_files = []


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(os.getenv("GITHUB_PAT_TOKEN") is None, reason="GITHUB_PAT_TOKEN not set")
def test_asymmetric_setup_github(content_config: ContentConfig, search_models: SearchModels):
    # Act
    # Regenerate github embeddings to test asymmetric setup without caching
    github_model = text_search.setup(
        GithubToJsonl, content_config.github, search_models.text_search.bi_encoder, regenerate=True
    )

    # Assert
    assert len(github_model.entries) > 1


def compare_index(initial_notes_model, final_notes_model):
    mismatched_entries, mismatched_embeddings = [], []
    for index in range(len(initial_notes_model.entries)):
        if initial_notes_model.entries[index].to_json() != final_notes_model.entries[index].to_json():
            mismatched_entries.append(index)

    # verify new entry embedding appended to embeddings tensor, without disrupting order or content of existing embeddings
    for index in range(len(initial_notes_model.corpus_embeddings)):
        if not torch.equal(final_notes_model.corpus_embeddings[index], initial_notes_model.corpus_embeddings[index]):
            mismatched_embeddings.append(index)

    error_details = ""
    if mismatched_entries:
        mismatched_entries_str = ",".join(map(str, mismatched_entries))
        error_details += f"Entries at {mismatched_entries_str} not equal\n"
    if mismatched_embeddings:
        mismatched_embeddings_str = ", ".join(map(str, mismatched_embeddings))
        error_details += f"Embeddings at {mismatched_embeddings_str} not equal\n"

    return error_details
