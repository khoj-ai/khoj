import calendar
import logging
import re
from collections import defaultdict
from datetime import datetime, timedelta
from math import inf
from typing import List, Tuple

import dateparser as dtparse
from dateparser.search import search_dates
from dateparser_data.settings import default_parsers
from dateutil.relativedelta import relativedelta

from khoj.search_filter.base_filter import BaseFilter
from khoj.utils.helpers import LRU, merge_dicts, timer

logger = logging.getLogger(__name__)


class DateFilter(BaseFilter):
    # Date Range Filter Regexes
    # Example filter queries:
    # - dt>="yesterday" dt<"tomorrow"
    # - dt>="last week"
    # - dt:"2 years ago"
    date_regex = r"dt([:><=]{1,2})[\"'](.*?)[\"']"

    def __init__(self, entry_key="compiled"):
        self.entry_key = entry_key
        self.date_to_entry_ids = defaultdict(set)
        self.cache = LRU()
        self.dtparser_regexes = self.compile_date_regexes()
        self.dtparser_ordinal_suffixes = re.compile(r"(st|nd|rd|th)")
        self.dtparser_settings = {
            "PREFER_DAY_OF_MONTH": "first",
            "DATE_ORDER": "YMD",  # Prefer YMD and DMY over MDY when parsing ambiguous dates
        }

    def compile_date_regexes(self):
        months = calendar.month_name[1:]
        abbr_months = calendar.month_abbr[1:]
        # Extract natural dates from content like 1st April 1984, 31 April 84, Apr 4th 1984, 13 Apr 84
        dBY_regex = re.compile(r"\b\d{1,2}(?:st|nd|rd|th)? (?:" + "|".join(months) + r") \d{4}\b", re.IGNORECASE)
        dBy_regex = re.compile(r"\b\d{1,2}(?:st|nd|rd|th)? (?:" + "|".join(months) + r") \d{2}\b", re.IGNORECASE)
        BdY_regex = re.compile(r"\b(?:" + "|".join(months) + r") \d{1,2}(?:st|nd|rd|th)? \d{4}\b", re.IGNORECASE)
        Bdy_regex = re.compile(r"\b(?:" + "|".join(months) + r") \d{1,2}(?:st|nd|rd|th)? \d{2}\b", re.IGNORECASE)
        dbY_regex = re.compile(r"\b\d{1,2}(?:st|nd|rd|th)? (?:" + "|".join(abbr_months) + r") \d{4}\b", re.IGNORECASE)
        dby_regex = re.compile(r"\b\d{1,2}(?:st|nd|rd|th)? (?:" + "|".join(abbr_months) + r") \d{2}\b", re.IGNORECASE)
        bdY_regex = re.compile(r"\b(?:" + "|".join(abbr_months) + r") \d{1,2}(?:st|nd|rd|th)? \d{4}\b", re.IGNORECASE)
        bdy_regex = re.compile(r"\b(?:" + "|".join(abbr_months) + r") \d{1,2}(?:st|nd|rd|th)? \d{2}\b", re.IGNORECASE)
        # Extract natural of form Month, Year like January 2021, Jan 2021, Jan 21
        BY_regex = re.compile(r"\b(?:" + "|".join(months) + r") \d{4}\b", re.IGNORECASE)
        By_regex = re.compile(r"\b(?:" + "|".join(months) + r") \d{2}\b", re.IGNORECASE)
        bY_regex = re.compile(r"\b(?:" + "|".join(abbr_months) + r") \d{4}\b", re.IGNORECASE)
        by_regex = re.compile(r"\b(?:" + "|".join(abbr_months) + r") \d{2}\b", re.IGNORECASE)
        # Extract structured dates from content like 1984-04-01, 1984/04/01, 01-04-1984, 01/04/1984, 01.04.1984, 01-04-84, 01/04/84
        Ymd_date_regex = re.compile(r"\b\d{4}[-\/]\d{2}[-\/]\d{2}\b", re.IGNORECASE)
        dmY_date_regex = re.compile(r"\b\d{2}[-\/]\d{2}[-\/]\d{4}\b", re.IGNORECASE)
        dmy_date_regex = re.compile(r"\b\d{2}[-\/]\d{2}[-\/]\d{2}\b", re.IGNORECASE)
        dmY_dot_date_regex = re.compile(r"\b\d{2}[\.]\d{2}[\.]\d{4}\b", re.IGNORECASE)

        # Combine date formatter and date identifier regex pairs
        dtparser_regexes: List[Tuple[str, re.Pattern[str]]] = [
            # Structured dates
            ("%Y-%m-%d", Ymd_date_regex),
            ("%Y/%m/%d", Ymd_date_regex),
            ("%d-%m-%Y", dmY_date_regex),
            ("%d/%m/%Y", dmY_date_regex),
            ("%d.%m.%Y", dmY_dot_date_regex),
            ("%d-%m-%y", dmy_date_regex),
            ("%d/%m/%y", dmy_date_regex),
            # Natural dates
            ("%d %B %Y", dBY_regex),
            ("%d %B %y", dBy_regex),
            ("%B %d %Y", BdY_regex),
            ("%B %d %y", Bdy_regex),
            ("%d %b %Y", dbY_regex),
            ("%d %b %y", dby_regex),
            ("%b %d %Y", bdY_regex),
            ("%b %d %y", bdy_regex),
            # Partial natural dates
            ("%B %Y", BY_regex),
            ("%B %y", By_regex),
            ("%b %Y", bY_regex),
            ("%b %y", by_regex),
        ]
        return dtparser_regexes

    def extract_dates(self, content):
        "Extract natural and structured dates from content"
        valid_dates = set()
        for date_format, date_regex in self.dtparser_regexes:
            matched_dates = date_regex.findall(content)
            for date_str in matched_dates:
                # Remove ordinal suffixes to parse date
                date_str = self.dtparser_ordinal_suffixes.sub("", date_str)
                try:
                    valid_dates.add(datetime.strptime(date_str, date_format))
                except ValueError:
                    continue

        return list(valid_dates)

    def get_filter_terms(self, query: str) -> List[str]:
        "Get all filter terms in query"
        return [f"dt{item[0]}'{item[1]}'" for item in re.findall(self.date_regex, query)]

    def get_query_date_range(self, query) -> List:
        with timer("Extract date range to filter from query", logger):
            query_daterange = self.extract_date_range(query)

        return query_daterange

    def defilter(self, query):
        # remove date range filter from query
        query = re.sub(rf"\s+{self.date_regex}", " ", query)
        query = re.sub(r"\s{2,}", " ", query).strip()  # remove multiple spaces
        return query

    def extract_date_range(self, query):
        # find date range filter in query
        date_range_matches = re.findall(self.date_regex, query)

        if len(date_range_matches) == 0:
            return []

        # extract, parse natural dates ranges from date range filter passed in query
        # e.g. today maps to (start_of_day, start_of_tomorrow)
        date_ranges_from_filter = []
        for cmp, date_str in date_range_matches:
            if self.parse(date_str):
                dt_start, dt_end = self.parse(date_str)
                date_ranges_from_filter += [[cmp, (dt_start.timestamp(), dt_end.timestamp())]]

        # Combine dates with their comparators to form date range intervals
        # For e.g.
        #   >=yesterday maps to [start_of_yesterday, inf)
        #   <tomorrow maps to [0, start_of_tomorrow)
        # ---
        effective_date_range: List = [0, inf]
        date_range_considering_comparator = []
        for cmp, (dtrange_start, dtrange_end) in date_ranges_from_filter:
            if cmp == ">":
                date_range_considering_comparator += [[dtrange_end, inf]]
            elif cmp == ">=":
                date_range_considering_comparator += [[dtrange_start, inf]]
            elif cmp == "<":
                date_range_considering_comparator += [[0, dtrange_start]]
            elif cmp == "<=":
                date_range_considering_comparator += [[0, dtrange_end]]
            elif cmp == "=" or cmp == ":" or cmp == "==":
                date_range_considering_comparator += [[dtrange_start, dtrange_end]]

        # Combine above intervals (via AND/intersect)
        # In the above example, this gives us [start_of_yesterday, start_of_tomorrow)
        # This is the effective date range to filter entries by
        # ---
        for date_range in date_range_considering_comparator:
            effective_date_range = [
                max(effective_date_range[0], date_range[0]),
                min(effective_date_range[1], date_range[1]),
            ]

        if effective_date_range == [0, inf] or effective_date_range[0] > effective_date_range[1]:
            return []
        else:
            # If the first element is 0, replace it with None

            if effective_date_range[0] == 0:
                effective_date_range[0] = None

            # If the second element is inf, replace it with None
            if effective_date_range[1] == inf:
                effective_date_range[1] = None

            return effective_date_range

    def parse(self, date_str, relative_base=None):
        "Parse date string passed in date filter of query to datetime object"
        # clean date string to handle future date parsing by date parser
        future_strings = ["later", "from now", "from today"]
        prefer_dates_from = {True: "future", False: "past"}[any([True for fstr in future_strings if fstr in date_str])]
        dtquery_settings = {"RELATIVE_BASE": relative_base or datetime.now(), "PREFER_DATES_FROM": prefer_dates_from}
        dtparser_settings = merge_dicts(dtquery_settings, self.dtparser_settings)

        # parse date passed in query date filter
        clean_date_str = re.sub("|".join(future_strings), "", date_str)
        try:
            parsed_date = dtparse.parse(clean_date_str, settings=dtparser_settings)
        except Exception as e:
            logger.error(f"Failed to parse date string: {date_str} with error: {e}")
            return None

        if parsed_date is None:
            return None

        return self.date_to_daterange(parsed_date, date_str)

    def date_to_daterange(self, parsed_date, date_str):
        "Convert parsed date to date ranges at natural granularity (day, week, month or year)"

        start_of_day = parsed_date.replace(hour=0, minute=0, second=0, microsecond=0)

        if "year" in date_str:
            return (datetime(parsed_date.year, 1, 1, 0, 0, 0), datetime(parsed_date.year + 1, 1, 1, 0, 0, 0))
        if "month" in date_str:
            start_of_month = datetime(parsed_date.year, parsed_date.month, 1, 0, 0, 0)
            next_month = start_of_month + relativedelta(months=1)
            return (start_of_month, next_month)
        if "week" in date_str:
            # if week in date string, dateparser parses it to next week start
            # so today = end of this week
            start_of_week = start_of_day - timedelta(days=7)
            return (start_of_week, start_of_day)
        else:
            next_day = start_of_day + relativedelta(days=1)
        return (start_of_day, next_day)
