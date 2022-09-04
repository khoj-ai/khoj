# External Packages
import torch

# Application Packages
from src.search_filter.file_filter import FileFilter


def test_no_file_filter():
    # Arrange
    file_filter = FileFilter()
    embeddings, entries = arrange_content()
    q_with_no_filter = 'head tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, ret_entries, ret_emb = file_filter.apply(q_with_no_filter, entries.copy(), embeddings)

    # Assert
    assert can_filter == False
    assert ret_query == 'head tail'
    assert len(ret_emb) == 4
    assert ret_entries == entries


def test_file_filter_with_partial_match():
    # Arrange
    file_filter = FileFilter()
    embeddings, entries = arrange_content()
    q_with_no_filter = 'head file:"*.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, ret_entries, ret_emb = file_filter.apply(q_with_no_filter, entries.copy(), embeddings)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert len(ret_emb) == 4
    assert ret_entries == entries


def test_file_filter_with_non_existent_file():
    # Arrange
    file_filter = FileFilter()
    embeddings, entries = arrange_content()
    q_with_no_filter = 'head file:"nonexistent.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, ret_entries, ret_emb = file_filter.apply(q_with_no_filter, entries.copy(), embeddings)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert len(ret_emb) == 0
    assert ret_entries == []


def test_single_file_filter():
    # Arrange
    file_filter = FileFilter()
    embeddings, entries = arrange_content()
    q_with_no_filter = 'head file:"file1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, ret_entries, ret_emb = file_filter.apply(q_with_no_filter, entries.copy(), embeddings)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert len(ret_emb) == 2
    assert ret_entries == [entries[0], entries[2]]


def test_multiple_file_filter():
    # Arrange
    file_filter = FileFilter()
    embeddings, entries = arrange_content()
    q_with_no_filter = 'head tail file:"file1.org" file:"file2.org"'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, ret_entries, ret_emb = file_filter.apply(q_with_no_filter, entries.copy(), embeddings)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert len(ret_emb) == 4
    assert ret_entries == entries


def arrange_content():
    embeddings = torch.randn(4, 10)
    entries = [
        {'compiled': '', 'raw': 'First Entry', 'file': 'file1.org'},
        {'compiled': '', 'raw': 'Second Entry', 'file': 'file2.org'},
        {'compiled': '', 'raw': 'Third Entry', 'file': 'file1.org'},
        {'compiled': '', 'raw': 'Fourth Entry', 'file': 'file2.org'}]

    return embeddings, entries
