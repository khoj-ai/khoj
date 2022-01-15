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
    hits = asymmetric.query(
        query,
        model = model.notes_search)

    results = asymmetric.collate_results(
        hits,
        model.notes_search.entries,
        count=1)

    # Assert
    # Actual_data should contain "Semantic Search via Emacs" entry
    search_result = results[0]["Entry"]
    assert "git clone" in search_result
