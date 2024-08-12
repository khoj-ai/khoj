# Application Packages
from khoj.search_filter.file_filter import FileFilter
from khoj.utils.rawconfig import Entry


def test_can_filter_no_file_filter():
    # Arrange
    file_filter = FileFilter()
    q_with_no_filter = "head tail"

    # Act
    can_filter = file_filter.can_filter(q_with_no_filter)

    # Assert
    assert can_filter == False


def test_can_filter_non_existent_file():
    # Arrange
    file_filter = FileFilter()
    q_with_filter = 'head file:"nonexistent.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_filter)

    # Assert
    assert can_filter == True


def test_can_filter_single_file_include():
    # Arrange
    file_filter = FileFilter()
    q_with_filter = 'head file:"file 1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_filter)

    # Assert
    assert can_filter == True


def test_can_filter_single_file_exclude():
    # Arrange
    file_filter = FileFilter()
    q_with_filter = 'head -file:"1.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_filter)

    # Assert
    assert can_filter == True


def test_can_filter_file_with_regex_match():
    # Arrange
    file_filter = FileFilter()
    q_with_filter = 'head file:"*.org" tail'

    # Act
    can_filter = file_filter.can_filter(q_with_filter)

    # Assert
    assert can_filter == True


def test_can_filter_multiple_file_includes():
    # Arrange
    file_filter = FileFilter()
    q_with_filter = 'head tail file:"file 1.org" file:"file2.org"'

    # Act
    can_filter = file_filter.can_filter(q_with_filter)

    # Assert
    assert can_filter == True


def test_get_single_include_file_filter_terms():
    # Arrange
    file_filter = FileFilter()
    q_with_filter_terms = 'head tail file:"/path/to/dir/*.org"'

    # Act
    filter_terms = file_filter.get_filter_terms(q_with_filter_terms)

    # Assert
    assert filter_terms == ["/path/to/dir/*.org"]


def test_get_single_exclude_file_filter_terms():
    # Arrange
    file_filter = FileFilter()
    q_with_filter_terms = 'head tail -file:"file 1.org"'

    # Act
    filter_terms = file_filter.get_filter_terms(q_with_filter_terms)

    # Assert
    assert filter_terms == ["-file 1.org"]


def test_get_single_include_exclude_file_filter_terms():
    # Arrange
    file_filter = FileFilter()
    q_with_filter_terms = 'head tail -file:"file 1.org" file:"/path/to/dir/*.org"'

    # Act
    filter_terms = file_filter.get_filter_terms(q_with_filter_terms)

    # Assert
    assert filter_terms == ["/path/to/dir/*.org", "-file 1.org"]


def test_get_multiple_include_exclude_file_filter_terms():
    # Arrange
    file_filter = FileFilter()
    q_with_filter_terms = (
        'head -file:"file 1.org" file:"file 1.org" file:"/path/to/dir/.*.org" -file:"/path/to/dir/*.org" tail'
    )

    # Act
    filter_terms = file_filter.get_filter_terms(q_with_filter_terms)

    # Assert
    assert filter_terms == ["file 1.org", "/path/to/dir/.*.org", "-file 1.org", "-/path/to/dir/*.org"]


def arrange_content():
    entries = [
        Entry(compiled="", raw="First Entry", file="file 1.org"),
        Entry(compiled="", raw="Second Entry", file="file2.org"),
        Entry(compiled="", raw="Third Entry", file="file 1.org"),
        Entry(compiled="", raw="Fourth Entry", file="file2.org"),
    ]

    return entries
