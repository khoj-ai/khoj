# Standard Packages
import re
from datetime import datetime
from math import inf

# External Packages
import torch

# Application Packages
from src.search_filter import date_filter


def test_date_filter():
    embeddings = torch.randn(3, 10)
    entries = [
        ['', 'Entry with no date'],
        ['', 'April Fools entry: 1984-04-01'],
        ['', 'Entry with date:1984-04-02']]

    q_with_no_date_filter = 'head tail'
    ret_query, ret_entries, ret_emb = date_filter.date_filter(q_with_no_date_filter, entries.copy(), embeddings)
    assert ret_query == 'head tail'
    assert len(ret_emb) == 3
    assert ret_entries == entries

    q_with_dtrange_non_overlapping_at_boundary = 'head dt>"1984-04-01" dt<"1984-04-02" tail'
    ret_query, ret_entries, ret_emb = date_filter.date_filter(q_with_dtrange_non_overlapping_at_boundary, entries.copy(), embeddings)
    assert ret_query == 'head tail'
    assert len(ret_emb) == 0
    assert ret_entries == []

    query_with_overlapping_dtrange = 'head dt>"1984-04-01" dt<"1984-04-03" tail'
    ret_query, ret_entries, ret_emb = date_filter.date_filter(query_with_overlapping_dtrange, entries.copy(), embeddings)
    assert ret_query == 'head tail'
    assert ret_entries == [entries[2]]
    assert len(ret_emb) == 1

    query_with_overlapping_dtrange = 'head dt>="1984-04-01" dt<"1984-04-02" tail'
    ret_query, ret_entries, ret_emb = date_filter.date_filter(query_with_overlapping_dtrange, entries.copy(), embeddings)
    assert ret_query == 'head tail'
    assert ret_entries == [entries[1]]
    assert len(ret_emb) == 1

    query_with_overlapping_dtrange = 'head dt>"1984-04-01" dt<="1984-04-02" tail'
    ret_query, ret_entries, ret_emb = date_filter.date_filter(query_with_overlapping_dtrange, entries.copy(), embeddings)
    assert ret_query == 'head tail'
    assert ret_entries == [entries[2]]
    assert len(ret_emb) == 1

    query_with_overlapping_dtrange = 'head dt>="1984-04-01" dt<="1984-04-02" tail'
    ret_query, ret_entries, ret_emb = date_filter.date_filter(query_with_overlapping_dtrange, entries.copy(), embeddings)
    assert ret_query == 'head tail'
    assert ret_entries == [entries[1], entries[2]]
    assert len(ret_emb) == 2


def test_extract_date_range():
    assert date_filter.extract_date_range('head dt>"1984-01-04" dt<"1984-01-07" tail') == [datetime(1984, 1, 5, 0, 0, 0).timestamp(), datetime(1984, 1, 7, 0, 0, 0).timestamp()]
    assert date_filter.extract_date_range('head dt<="1984-01-01"') == [0, datetime(1984, 1, 2, 0, 0, 0).timestamp()]
    assert date_filter.extract_date_range('head dt>="1984-01-01"') == [datetime(1984, 1, 1, 0, 0, 0).timestamp(), inf]
    assert date_filter.extract_date_range('head dt:"1984-01-01"') == [datetime(1984, 1, 1, 0, 0, 0).timestamp(), datetime(1984, 1, 2, 0, 0, 0).timestamp()]

    # No date filter specified in query
    assert date_filter.extract_date_range('head tail') == None

    # Non intersecting date ranges
    assert date_filter.extract_date_range('head dt>"1984-01-01" dt<"1984-01-01" tail') == None


def test_parse():
    test_now = datetime(1984, 4, 1, 21, 21, 21)

    # day variations
    assert date_filter.parse('today', relative_base=test_now) == (datetime(1984, 4, 1, 0, 0, 0), datetime(1984, 4, 2, 0, 0, 0))
    assert date_filter.parse('tomorrow', relative_base=test_now) == (datetime(1984, 4, 2, 0, 0, 0), datetime(1984, 4, 3, 0, 0, 0))
    assert date_filter.parse('yesterday', relative_base=test_now) == (datetime(1984, 3, 31, 0, 0, 0), datetime(1984, 4, 1, 0, 0, 0))
    assert date_filter.parse('5 days ago', relative_base=test_now) == (datetime(1984, 3, 27, 0, 0, 0), datetime(1984, 3, 28, 0, 0, 0))

    # week variations
    assert date_filter.parse('last week', relative_base=test_now) == (datetime(1984, 3, 18, 0, 0, 0), datetime(1984, 3, 25, 0, 0, 0))
    assert date_filter.parse('2 weeks ago', relative_base=test_now) == (datetime(1984, 3, 11, 0, 0, 0), datetime(1984, 3, 18, 0, 0, 0))

    # month variations
    assert date_filter.parse('next month', relative_base=test_now) == (datetime(1984, 5, 1, 0, 0, 0), datetime(1984, 6, 1, 0, 0, 0))
    assert date_filter.parse('2 months ago', relative_base=test_now) == (datetime(1984, 2, 1, 0, 0, 0), datetime(1984, 3, 1, 0, 0, 0))

    # year variations
    assert date_filter.parse('this year', relative_base=test_now) == (datetime(1984, 1, 1, 0, 0, 0), datetime(1985, 1, 1, 0, 0, 0))
    assert date_filter.parse('20 years later', relative_base=test_now) == (datetime(2004, 1, 1, 0, 0, 0), datetime(2005, 1, 1, 0, 0, 0))

    # specific month/date variation
    assert date_filter.parse('in august', relative_base=test_now) == (datetime(1983, 8, 1, 0, 0, 0), datetime(1983, 8, 2, 0, 0, 0))
    assert date_filter.parse('on 1983-08-01', relative_base=test_now) == (datetime(1983, 8, 1, 0, 0, 0), datetime(1983, 8, 2, 0, 0, 0))


def test_date_filter_regex():
    dtrange_match = re.search(date_filter.date_range_regex, 'head dt>"today" dt:"1984-01-01" tail')
    assert dtrange_match.groups() == ('>', 'today', ':', '1984-01-01')

    dtrange_match = re.search(date_filter.date_range_regex, 'head dt>="today" dt="1984-01-01"')
    assert dtrange_match.groups() == ('>=', 'today', '=', '1984-01-01')

    dtrange_match = re.search(date_filter.date_range_regex, 'head dt<"today" tail')
    assert dtrange_match.groups() == ('<', 'today', None, None)

    dtrange_match = re.search(date_filter.date_range_regex, 'head dt<="today"')
    assert dtrange_match.groups() == ('<=', 'today', None, None)

    dtrange_match = re.search(date_filter.date_range_regex, 'head tail')
    assert dtrange_match.groups() == (None, None, None, None)
