# System Packages
from pathlib import Path

# Internal Packages
from src.main import model
from src.search_type import asymmetric
from src.utils.rawconfig import ContentConfig, SearchConfig


# Test
# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup(content_config: ContentConfig, search_config: SearchConfig):
    # Act
    # Regenerate notes embeddings during asymmetric setup
    notes_model = asymmetric.setup(content_config.org, search_config.asymmetric, regenerate=True)

    # Assert
    assert len(notes_model.entries) == 10
    assert len(notes_model.corpus_embeddings) == 10


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_search(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    model.notes_search = asymmetric.setup(content_config.org, search_config.asymmetric, regenerate=False)
    query = "How to git install application?"

    # Act
    hits, entries = asymmetric.query(
        query,
        model = model.notes_search)

    results = asymmetric.collate_results(
        hits,
        entries,
        count=1)

    # Assert
    # Actual_data should contain "Semantic Search via Emacs" entry
    search_result = results[0]["Entry"]
    assert "git clone" in search_result


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_reload(content_config: ContentConfig, search_config: SearchConfig):
    # Arrange
    initial_notes_model= asymmetric.setup(content_config.org, search_config.asymmetric, regenerate=False)

    assert len(initial_notes_model.entries) == 10
    assert len(initial_notes_model.corpus_embeddings) == 10

    file_to_add_on_reload = Path(content_config.org.input_filter).parent / "reload.org"
    content_config.org.input_files = [f'{file_to_add_on_reload}']

    # append Org-Mode Entry to first Org Input File in Config
    with open(file_to_add_on_reload, "w") as f:
        f.write("\n* A Chihuahua doing Tango\n- Saw a super cute video of a chihuahua doing the Tango on Youtube\n")

    # regenerate notes jsonl, model embeddings and model to include entry from new file
    regenerated_notes_model = asymmetric.setup(content_config.org, search_config.asymmetric, regenerate=True)

    # Act
    # reload embeddings, entries, notes model from previously generated notes jsonl and model embeddings files
    initial_notes_model = asymmetric.setup(content_config.org, search_config.asymmetric, regenerate=False)

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
