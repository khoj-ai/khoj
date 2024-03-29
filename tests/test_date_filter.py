import re
from datetime import datetime

import pytest

from khoj.search_filter.date_filter import DateFilter


@pytest.mark.filterwarnings("ignore:The localize method is no longer necessary.")
def test_extract_date_range():
    assert DateFilter().extract_date_range('head dt>"1984-01-04" dt<"1984-01-07" tail') == [
        datetime(1984, 1, 5, 0, 0, 0).timestamp(),
        datetime(1984, 1, 7, 0, 0, 0).timestamp(),
    ]
    assert DateFilter().extract_date_range('head dt<="1984-01-01"') == [None, datetime(1984, 1, 2, 0, 0, 0).timestamp()]
    assert DateFilter().extract_date_range('head dt>="1984-01-01"') == [datetime(1984, 1, 1, 0, 0, 0).timestamp(), None]
    assert DateFilter().extract_date_range('head dt:"1984-01-01"') == [
        datetime(1984, 1, 1, 0, 0, 0).timestamp(),
        datetime(1984, 1, 2, 0, 0, 0).timestamp(),
    ]
    assert DateFilter().extract_date_range('head dt="1984-01-01"') == [
        datetime(1984, 1, 1, 0, 0, 0).timestamp(),
        datetime(1984, 1, 2, 0, 0, 0).timestamp(),
    ]

    # Unparseable date filter specified in query
    assert DateFilter().extract_date_range('head dt:"Summer of 69" tail') == []

    # No date filter specified in query
    assert DateFilter().extract_date_range("head tail") == []

    # Non intersecting date ranges
    assert DateFilter().extract_date_range('head dt>"1984-01-01" dt<"1984-01-01" tail') == []


@pytest.mark.filterwarnings("ignore:The localize method is no longer necessary.")
def test_parse():
    test_now = datetime(1984, 4, 1, 21, 21, 21)

    # day variations
    assert DateFilter().parse("today", relative_base=test_now) == (
        datetime(1984, 4, 1, 0, 0, 0),
        datetime(1984, 4, 2, 0, 0, 0),
    )
    assert DateFilter().parse("tomorrow", relative_base=test_now) == (
        datetime(1984, 4, 2, 0, 0, 0),
        datetime(1984, 4, 3, 0, 0, 0),
    )
    assert DateFilter().parse("yesterday", relative_base=test_now) == (
        datetime(1984, 3, 31, 0, 0, 0),
        datetime(1984, 4, 1, 0, 0, 0),
    )
    assert DateFilter().parse("5 days ago", relative_base=test_now) == (
        datetime(1984, 3, 27, 0, 0, 0),
        datetime(1984, 3, 28, 0, 0, 0),
    )

    # week variations
    assert DateFilter().parse("last week", relative_base=test_now) == (
        datetime(1984, 3, 18, 0, 0, 0),
        datetime(1984, 3, 25, 0, 0, 0),
    )
    assert DateFilter().parse("2 weeks ago", relative_base=test_now) == (
        datetime(1984, 3, 11, 0, 0, 0),
        datetime(1984, 3, 18, 0, 0, 0),
    )

    # month variations
    assert DateFilter().parse("next month", relative_base=test_now) == (
        datetime(1984, 5, 1, 0, 0, 0),
        datetime(1984, 6, 1, 0, 0, 0),
    )
    assert DateFilter().parse("2 months ago", relative_base=test_now) == (
        datetime(1984, 2, 1, 0, 0, 0),
        datetime(1984, 3, 1, 0, 0, 0),
    )

    # year variations
    assert DateFilter().parse("this year", relative_base=test_now) == (
        datetime(1984, 1, 1, 0, 0, 0),
        datetime(1985, 1, 1, 0, 0, 0),
    )
    assert DateFilter().parse("20 years later", relative_base=test_now) == (
        datetime(2004, 1, 1, 0, 0, 0),
        datetime(2005, 1, 1, 0, 0, 0),
    )

    # specific month/date variation
    assert DateFilter().parse("in august", relative_base=test_now) == (
        datetime(1983, 8, 1, 0, 0, 0),
        datetime(1983, 8, 2, 0, 0, 0),
    )
    assert DateFilter().parse("on 1983-08-01", relative_base=test_now) == (
        datetime(1983, 8, 1, 0, 0, 0),
        datetime(1983, 8, 2, 0, 0, 0),
    )


def test_date_filter_regex():
    dtrange_match = re.findall(DateFilter().date_regex, 'multi word head dt>"today" dt:"1984-01-01"')
    assert dtrange_match == [(">", "today"), (":", "1984-01-01")]

    dtrange_match = re.findall(DateFilter().date_regex, 'head dt>"today" dt:"1984-01-01" multi word tail')
    assert dtrange_match == [(">", "today"), (":", "1984-01-01")]

    dtrange_match = re.findall(DateFilter().date_regex, 'multi word head dt>="today" dt="1984-01-01"')
    assert dtrange_match == [(">=", "today"), ("=", "1984-01-01")]

    dtrange_match = re.findall(DateFilter().date_regex, 'dt<"multi word date" multi word tail')
    assert dtrange_match == [("<", "multi word date")]

    dtrange_match = re.findall(DateFilter().date_regex, 'head dt<="multi word date"')
    assert dtrange_match == [("<=", "multi word date")]

    dtrange_match = re.findall(DateFilter().date_regex, "head tail")
    assert dtrange_match == []


def test_get_date_filter_terms():
    dtrange_match = DateFilter().get_filter_terms('multi word head dt>"today" dt:"1984-01-01"')
    assert dtrange_match == ["dt>'today'", "dt:'1984-01-01'"]

    dtrange_match = DateFilter().get_filter_terms('head dt>"today" dt:"1984-01-01" multi word tail')
    assert dtrange_match == ["dt>'today'", "dt:'1984-01-01'"]

    dtrange_match = DateFilter().get_filter_terms('multi word head dt>="today" dt="1984-01-01"')
    assert dtrange_match == ["dt>='today'", "dt='1984-01-01'"]

    dtrange_match = DateFilter().get_filter_terms('dt<"multi word date" multi word tail')
    assert dtrange_match == ["dt<'multi word date'"]

    dtrange_match = DateFilter().get_filter_terms('head dt<="multi word date"')
    assert dtrange_match == ["dt<='multi word date'"]

    dtrange_match = DateFilter().get_filter_terms("head tail")
    assert dtrange_match == []


def test_date_extraction():
    extracted_dates = DateFilter().extract_dates("")
    assert extracted_dates == [], "Expected to handle empty string"

    extracted_dates = DateFilter().extract_dates("head tail")
    assert extracted_dates == [], "Expected to handle no dates"

    extracted_dates = DateFilter().extract_dates("head CREATED: today tail")
    assert extracted_dates == [], "Expected relative date to be ignored"

    extracted_dates = DateFilter().extract_dates("head CREATED: today SCHEDULED: 1984-04-01 tail")
    assert extracted_dates == [datetime(1984, 4, 1, 0, 0, 0)], "Expected only Y-m-d structured date to be extracted"

    extracted_dates = DateFilter().extract_dates("head SCHEDULED: 01-04-1984 tail")
    assert extracted_dates == [datetime(1984, 4, 1, 0, 0, 0)], "Expected d-m-Y structured date to be extracted"

    extracted_dates = DateFilter().extract_dates("head CREATED: today SCHEDULED: 1984/04/01 tail")
    assert extracted_dates == [datetime(1984, 4, 1, 0, 0, 0)], "Expected only Y/m/d structured date to be extracted"

    extracted_dates = DateFilter().extract_dates("head SCHEDULED: 01/04/1984 tail")
    assert extracted_dates == [datetime(1984, 4, 1, 0, 0, 0)], "Expected d/m/Y structured date to be extracted"

    extracted_dates = DateFilter().extract_dates("head DEADLINE: 01.04.1984 tail")
    assert extracted_dates == [datetime(1984, 4, 1, 0, 0, 0)], "Expected d.m.Y structured date to be extracted"

    extracted_dates = DateFilter().extract_dates("CLOCK: [1984-04-01 Sun 09:50]--[1984-04-01 Sun 10:10] => 24:20")
    assert extracted_dates == [
        datetime(1984, 4, 1, 0, 0, 0)
    ], "Expected single deduplicated date extracted from logbook entry"

    extracted_dates = DateFilter().extract_dates("CLOCK: [1984/03/31 mer 09:50]--[1984/04/01 mer 10:10] => 24:20")
    expected_dates = [datetime(1984, 4, 1, 0, 0, 0), datetime(1984, 3, 31, 0, 0, 0)]
    assert all(
        [dt in extracted_dates for dt in expected_dates]
    ), "Expected multiple different dates extracted from logbook entry"


def test_natual_date_extraction():
    extracted_dates = DateFilter().extract_dates("head 1 April 1984 tail")
    assert datetime(1984, 4, 1, 0, 0, 0) in extracted_dates, "Expected natural date to be extracted"

    extracted_dates = DateFilter().extract_dates("head 1st April 1984 tail")
    assert datetime(1984, 4, 1, 0, 0, 0) in extracted_dates, "Expected natural date to be extracted"

    extracted_dates = DateFilter().extract_dates("head 2nd Apr 1984 tail")
    assert datetime(1984, 4, 2, 0, 0, 0) in extracted_dates, "Expected natural date with short month to be extracted"

    extracted_dates = DateFilter().extract_dates("head 4th Apr 1984 tail")
    assert datetime(1984, 4, 4, 0, 0, 0) in extracted_dates, "Expected natural date to be extracted"

    extracted_dates = DateFilter().extract_dates("head 11th april 1984 tail")
    assert (
        datetime(1984, 4, 11, 0, 0, 0) in extracted_dates
    ), "Expected natural date with lowercase month to be extracted"

    extracted_dates = DateFilter().extract_dates("head 23rd april 84 tail")
    assert datetime(1984, 4, 23, 0, 0, 0) in extracted_dates, "Expected natural date with 2-digit year to be extracted"

    extracted_dates = DateFilter().extract_dates("head 31st march 84 tail")
    assert datetime(1984, 3, 31, 0, 0, 0) in extracted_dates, "Expected natural date with 2-digit year to be extracted"

    extracted_dates = DateFilter().extract_dates("head April 1984 tail")
    assert extracted_dates == [datetime(1984, 4, 1, 0, 0, 0)], "Expected partial natural date to be extracted"

    extracted_dates = DateFilter().extract_dates("head Apr 1984 tail")
    assert extracted_dates == [
        datetime(1984, 4, 1, 0, 0, 0)
    ], "Expected partial natural date with short month to be extracted"

    extracted_dates = DateFilter().extract_dates("head apr 1984 tail")
    assert extracted_dates == [
        datetime(1984, 4, 1, 0, 0, 0)
    ], "Expected partial natural date with lowercase month to be extracted"

    extracted_dates = DateFilter().extract_dates("head apr 84 tail")
    assert extracted_dates == [
        datetime(1984, 4, 1, 0, 0, 0)
    ], "Expected partial natural date with 2-digit year to be extracted"
