# Application Packages
from src.search_filter.file_filter import FileFilter
from src.utils.rawconfig import Entry


def test_no_file_filter():
    # Arrange
    file_filter = FileFilter()
    entries = arrange_content()
    q_with_no_filter = 'head tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, entry_indices = file_filter.apply(q_with_no_filter, entries)

    # Assert
    assert can_filter == False
    assert ret_query == 'head tail'
    assert entry_indices == {0, 1, 2, 3}


def test_file_filter_with_non_existent_file():
    # Arrange
    file_filter = FileFilter()
    entries = arrange_content()
    q_with_no_filter = 'head file:"nonexistent.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, entry_indices = file_filter.apply(q_with_no_filter, entries)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert entry_indices == {}


def test_single_file_filter():
    # Arrange
    file_filter = FileFilter()
    entries = arrange_content()
    q_with_no_filter = 'head file:"file 1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, entry_indices = file_filter.apply(q_with_no_filter, entries)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert entry_indices == {0, 2}


def test_file_filter_with_partial_match():
    # Arrange
    file_filter = FileFilter()
    entries = arrange_content()
    q_with_no_filter = 'head file:"1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, entry_indices = file_filter.apply(q_with_no_filter, entries)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert entry_indices == {0, 2}


def test_file_filter_with_regex_match():
    # Arrange
    file_filter = FileFilter()
    entries = arrange_content()
    q_with_no_filter = 'head file:"*.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, entry_indices = file_filter.apply(q_with_no_filter, entries)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert entry_indices == {0, 1, 2, 3}


def test_multiple_file_filter():
    # Arrange
    file_filter = FileFilter()
    entries = arrange_content()
    q_with_no_filter = 'head tail file:"file 1.org" file:"file2.org"'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)
    ret_query, entry_indices = file_filter.apply(q_with_no_filter, entries)

    # Assert
    assert can_filter == True
    assert ret_query == 'head tail'
    assert entry_indices == {0, 1, 2, 3}


def arrange_content():
    entries = [
        Entry(compiled='', raw='First Entry', file= 'file 1.org'),
        Entry(compiled='', raw='Second Entry', file= 'file2.org'),
        Entry(compiled='', raw='Third Entry', file= 'file 1.org'),
        Entry(compiled='', raw='Fourth Entry', file= 'file2.org')
    ]

    return entries
