# Standard Packages
import re
from datetime import datetime
from math import inf

# Application Packages
from src.search_filter.date_filter import DateFilter
from src.utils.rawconfig import Entry


def test_date_filter():
    entries = [
        Entry(compiled='', raw='Entry with no date'),
        Entry(compiled='', raw='April Fools entry: 1984-04-01'),
        Entry(compiled='', raw='Entry with date:1984-04-02')
    ]

    q_with_no_date_filter = 'head tail'
    ret_query, entry_indices = DateFilter().apply(q_with_no_date_filter, entries)
    assert ret_query == 'head tail'
    assert entry_indices == {0, 1, 2}

    q_with_dtrange_non_overlapping_at_boundary = 'head dt>"1984-04-01" dt<"1984-04-02" tail'
    ret_query, entry_indices = DateFilter().apply(q_with_dtrange_non_overlapping_at_boundary, entries)
    assert ret_query == 'head tail'
    assert entry_indices == set()

    query_with_overlapping_dtrange = 'head dt>"1984-04-01" dt<"1984-04-03" tail'
    ret_query, entry_indices = DateFilter().apply(query_with_overlapping_dtrange, entries)
    assert ret_query == 'head tail'
    assert entry_indices == {2}

    query_with_overlapping_dtrange = 'head dt>="1984-04-01" dt<"1984-04-02" tail'
    ret_query, entry_indices = DateFilter().apply(query_with_overlapping_dtrange, entries)
    assert ret_query == 'head tail'
    assert entry_indices == {1}

    query_with_overlapping_dtrange = 'head dt>"1984-04-01" dt<="1984-04-02" tail'
    ret_query, entry_indices = DateFilter().apply(query_with_overlapping_dtrange, entries)
    assert ret_query == 'head tail'
    assert entry_indices == {2}

    query_with_overlapping_dtrange = 'head dt>="1984-04-01" dt<="1984-04-02" tail'
    ret_query, entry_indices = DateFilter().apply(query_with_overlapping_dtrange, entries)
    assert ret_query == 'head tail'
    assert entry_indices == {1, 2}


def test_extract_date_range():
    assert DateFilter().extract_date_range('head dt>"1984-01-04" dt<"1984-01-07" tail') == [datetime(1984, 1, 5, 0, 0, 0).timestamp(), datetime(1984, 1, 7, 0, 0, 0).timestamp()]
    assert DateFilter().extract_date_range('head dt<="1984-01-01"') == [0, datetime(1984, 1, 2, 0, 0, 0).timestamp()]
    assert DateFilter().extract_date_range('head dt>="1984-01-01"') == [datetime(1984, 1, 1, 0, 0, 0).timestamp(), inf]
    assert DateFilter().extract_date_range('head dt:"1984-01-01"') == [datetime(1984, 1, 1, 0, 0, 0).timestamp(), datetime(1984, 1, 2, 0, 0, 0).timestamp()]

    # Unparseable date filter specified in query
    assert DateFilter().extract_date_range('head dt:"Summer of 69" tail') == None

    # No date filter specified in query
    assert DateFilter().extract_date_range('head tail') == None

    # Non intersecting date ranges
    assert DateFilter().extract_date_range('head dt>"1984-01-01" dt<"1984-01-01" tail') == None


def test_parse():
    test_now = datetime(1984, 4, 1, 21, 21, 21)

    # day variations
    assert DateFilter().parse('today', relative_base=test_now) == (datetime(1984, 4, 1, 0, 0, 0), datetime(1984, 4, 2, 0, 0, 0))
    assert DateFilter().parse('tomorrow', relative_base=test_now) == (datetime(1984, 4, 2, 0, 0, 0), datetime(1984, 4, 3, 0, 0, 0))
    assert DateFilter().parse('yesterday', relative_base=test_now) == (datetime(1984, 3, 31, 0, 0, 0), datetime(1984, 4, 1, 0, 0, 0))
    assert DateFilter().parse('5 days ago', relative_base=test_now) == (datetime(1984, 3, 27, 0, 0, 0), datetime(1984, 3, 28, 0, 0, 0))

    # week variations
    assert DateFilter().parse('last week', relative_base=test_now) == (datetime(1984, 3, 18, 0, 0, 0), datetime(1984, 3, 25, 0, 0, 0))
    assert DateFilter().parse('2 weeks ago', relative_base=test_now) == (datetime(1984, 3, 11, 0, 0, 0), datetime(1984, 3, 18, 0, 0, 0))

    # month variations
    assert DateFilter().parse('next month', relative_base=test_now) == (datetime(1984, 5, 1, 0, 0, 0), datetime(1984, 6, 1, 0, 0, 0))
    assert DateFilter().parse('2 months ago', relative_base=test_now) == (datetime(1984, 2, 1, 0, 0, 0), datetime(1984, 3, 1, 0, 0, 0))

    # year variations
    assert DateFilter().parse('this year', relative_base=test_now) == (datetime(1984, 1, 1, 0, 0, 0), datetime(1985, 1, 1, 0, 0, 0))
    assert DateFilter().parse('20 years later', relative_base=test_now) == (datetime(2004, 1, 1, 0, 0, 0), datetime(2005, 1, 1, 0, 0, 0))

    # specific month/date variation
    assert DateFilter().parse('in august', relative_base=test_now) == (datetime(1983, 8, 1, 0, 0, 0), datetime(1983, 8, 2, 0, 0, 0))
    assert DateFilter().parse('on 1983-08-01', relative_base=test_now) == (datetime(1983, 8, 1, 0, 0, 0), datetime(1983, 8, 2, 0, 0, 0))


def test_date_filter_regex():
    dtrange_match = re.findall(DateFilter().date_regex, 'multi word head dt>"today" dt:"1984-01-01"')
    assert dtrange_match == [('>', 'today'), (':', '1984-01-01')]

    dtrange_match = re.findall(DateFilter().date_regex, 'head dt>"today" dt:"1984-01-01" multi word tail')
    assert dtrange_match == [('>', 'today'), (':', '1984-01-01')]

    dtrange_match = re.findall(DateFilter().date_regex, 'multi word head dt>="today" dt="1984-01-01"')
    assert dtrange_match == [('>=', 'today'), ('=', '1984-01-01')]

    dtrange_match = re.findall(DateFilter().date_regex, 'dt<"multi word date" multi word tail')
    assert dtrange_match == [('<', 'multi word date')]

    dtrange_match = re.findall(DateFilter().date_regex, 'head dt<="multi word date"')
    assert dtrange_match == [('<=', 'multi word date')]

    dtrange_match = re.findall(DateFilter().date_regex, 'head tail')
    assert dtrange_match == []