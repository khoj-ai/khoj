# External Packages
import torch

# Application Packages
from src.search_filter.explicit_filter import ExplicitFilter
from src.utils.config import SearchType


def test_no_explicit_filter(tmp_path):
    # Arrange
    explicit_filter = ExplicitFilter(tmp_path, SearchType.Org)
    embeddings, entries = arrange_content()
    q_with_no_filter = 'head tail'

    # Act
    ret_query, ret_entries, ret_emb = explicit_filter.apply(q_with_no_filter, entries.copy(), embeddings)

    # Assert
    assert ret_query == 'head tail'
    assert len(ret_emb) == 4
    assert ret_entries == entries


def test_explicit_exclude_filter(tmp_path):
    # Arrange
    explicit_filter = ExplicitFilter(tmp_path, SearchType.Org)
    embeddings, entries = arrange_content()
    q_with_exclude_filter = 'head -exclude_word tail'

    # Act
    ret_query, ret_entries, ret_emb = explicit_filter.apply(q_with_exclude_filter, entries.copy(), embeddings)

    # Assert
    assert ret_query == 'head tail'
    assert len(ret_emb) == 2
    assert ret_entries == [entries[0], entries[2]]


def test_explicit_include_filter(tmp_path):
    # Arrange
    explicit_filter = ExplicitFilter(tmp_path, SearchType.Org)
    embeddings, entries = arrange_content()
    query_with_include_filter = 'head +include_word tail'

    # Act
    ret_query, ret_entries, ret_emb = explicit_filter.apply(query_with_include_filter, entries.copy(), embeddings)

    # Assert
    assert ret_query == 'head tail'
    assert len(ret_emb) == 2
    assert ret_entries == [entries[2], entries[3]]


def test_explicit_include_and_exclude_filter(tmp_path):
    # Arrange
    explicit_filter = ExplicitFilter(tmp_path, SearchType.Org)
    embeddings, entries = arrange_content()
    query_with_include_and_exclude_filter = 'head +include_word -exclude_word tail'

    # Act
    ret_query, ret_entries, ret_emb = explicit_filter.apply(query_with_include_and_exclude_filter, entries.copy(), embeddings)

    # Assert
    assert ret_query == 'head tail'
    assert len(ret_emb) == 1
    assert ret_entries == [entries[2]]


def arrange_content():
    embeddings = torch.randn(4, 10)
    entries = [
        {'compiled': '', 'raw': 'Minimal Entry'},
        {'compiled': '', 'raw': 'Entry with exclude_word'},
        {'compiled': '', 'raw': 'Entry with include_word'},
        {'compiled': '', 'raw': 'Entry with include_word and exclude_word'}]

    return embeddings, entries
