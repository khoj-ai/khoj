# Application Packages
from khoj.search_filter.file_filter import FileFilter
from khoj.utils.rawconfig import Entry


def test_no_file_filter():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = "head tail"

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == False


def test_file_filter_with_non_existent_file():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = 'head file:"nonexistent.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == True


def test_single_file_filter():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = 'head file:"file 1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == True


def test_file_filter_with_partial_match():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = 'head file:"1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == True


def test_file_filter_with_regex_match():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = 'head file:"*.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == True


def test_multiple_file_filter():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = 'head tail file:"file 1.org" file:"file2.org"'

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == True


def test_get_file_filter_terms():
    # Arrange
    file_filter = FileFilter()
    q_with_filter_terms = 'head tail file:"file 1.org" file:"/path/to/dir/*.org"'

    # Act
    filter_terms = file_filter.get_filter_terms(q_with_filter_terms)

    # Assert
    assert filter_terms == ["file 1\\.org", "/path/to/dir/.*\\.org"]


def arrange_content():
    entries = [
        Entry(compiled="", raw="First Entry", file="file 1.org"),
        Entry(compiled="", raw="Second Entry", file="file2.org"),
        Entry(compiled="", raw="Third Entry", file="file 1.org"),
        Entry(compiled="", raw="Fourth Entry", file="file2.org"),
    ]

    return entries
