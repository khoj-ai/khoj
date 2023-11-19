# Application Packages
from khoj.search_filter.word_filter import WordFilter
from khoj.utils.rawconfig import Entry


# Test
# ----------------------------------------------------------------------------------------------------
def test_no_word_filter():
    # Arrange
    word_filter = WordFilter()
    q_with_no_filter = "head tail"

    # Act
    can_filter = word_filter.can_filter(q_with_no_filter)
    filter_terms = word_filter.get_filter_terms(q_with_no_filter)

    # Assert
    assert can_filter == False
    assert filter_terms == []


# ----------------------------------------------------------------------------------------------------


def test_word_exclude_filter():
    # Arrange
    word_filter = WordFilter()
    q_with_exclude_filter = 'head -"exclude_word" tail'

    # Act
    can_filter = word_filter.can_filter(q_with_exclude_filter)

    # Assert
    assert can_filter == True


# ----------------------------------------------------------------------------------------------------
def test_word_include_filter():
    # Arrange
    word_filter = WordFilter()
    query_with_include_filter = 'head +"include_word" tail'

    # Act
    can_filter = word_filter.can_filter(query_with_include_filter)

    # Assert
    assert can_filter == True


# ----------------------------------------------------------------------------------------------------
def test_word_include_and_exclude_filter():
    # Arrange
    word_filter = WordFilter()
    query_with_include_and_exclude_filter = 'head +"include_word" -"exclude_word" tail'

    # Act
    can_filter = word_filter.can_filter(query_with_include_and_exclude_filter)

    # Assert
    assert can_filter == True


# ----------------------------------------------------------------------------------------------------
def test_get_word_filter_terms():
    # Arrange
    word_filter = WordFilter()
    query_with_include_and_exclude_filter = 'head +"include_word" -"exclude_word" tail'

    # Act
    filter_terms = word_filter.get_filter_terms(query_with_include_and_exclude_filter)

    # Assert
    assert filter_terms == ["+include_word", "-exclude_word"]


def arrange_content():
    entries = [
        Entry(compiled="", raw="Minimal Entry"),
        Entry(compiled="", raw="Entry with exclude_word"),
        Entry(compiled="", raw="Entry with include_word"),
        Entry(compiled="", raw="Entry with include_word and exclude_word"),
    ]

    return entries
