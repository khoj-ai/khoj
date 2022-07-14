# Standard Packages
import re
from datetime import timedelta, datetime

# Application Packages
from src.search_filter import date_filter

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
    dtrange_match = re.search(date_filter.date_range_regex, 'head dt>"today" dt:"2020-01-01" tail')
    assert dtrange_match.groups() == ('>', 'today', ':', '2020-01-01')

    dtrange_match = re.search(date_filter.date_range_regex, 'head dt>="today" dt="2020-01-01"')
    assert dtrange_match.groups() == ('>=', 'today', '=', '2020-01-01')

    dtrange_match = re.search(date_filter.date_range_regex, 'head dt<"today" tail')
    assert dtrange_match.groups() == ('<', 'today', None, None)

    dtrange_match = re.search(date_filter.date_range_regex, 'head dt<="today"')
    assert dtrange_match.groups() == ('<=', 'today', None, None)

    dtrange_match = re.search(date_filter.date_range_regex, 'head tail')
    assert dtrange_match.groups() == (None, None, None, None)
