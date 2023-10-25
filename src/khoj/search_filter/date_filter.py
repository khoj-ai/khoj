# Standard Packages
import re
import logging
from collections import defaultdict
from datetime import timedelta, datetime
from typing import List
from dateutil.relativedelta import relativedelta
from math import inf

# External Packages
import dateparser as dtparse

# Internal Packages
from khoj.search_filter.base_filter import BaseFilter
from khoj.utils.helpers import LRU, timer


logger = logging.getLogger(__name__)


class DateFilter(BaseFilter):
    # Date Range Filter Regexes
    # Example filter queries:
    # - dt>="yesterday" dt<"tomorrow"
    # - dt>="last week"
    # - dt:"2 years ago"
    date_regex = r"dt([:><=]{1,2})[\"'](.*?)[\"']"
    raw_date_regex = r"\d{4}-\d{2}-\d{2}"

    def __init__(self, entry_key="compiled"):
        self.entry_key = entry_key
        self.date_to_entry_ids = defaultdict(set)
        self.cache = LRU()

    def extract_dates(self, content):
        pattern_matched_dates = re.findall(self.raw_date_regex, content)

        # Filter down to valid dates
        valid_dates = []
        for date_str in pattern_matched_dates:
            try:
                valid_dates.append(datetime.strptime(date_str, "%Y-%m-%d"))
            except ValueError:
                continue

        return valid_dates

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
        # e.g today maps to (start_of_day, start_of_tomorrow)
        date_ranges_from_filter = []
        for cmp, date_str in date_range_matches:
            if self.parse(date_str):
                dt_start, dt_end = self.parse(date_str)
                date_ranges_from_filter += [[cmp, (dt_start.timestamp(), dt_end.timestamp())]]

        # Combine dates with their comparators to form date range intervals
        # For e.g
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
        clean_date_str = re.sub("|".join(future_strings), "", date_str)

        # parse date passed in query date filter
        try:
            parsed_date = dtparse.parse(
                clean_date_str,
                settings={
                    "RELATIVE_BASE": relative_base or datetime.now(),
                    "PREFER_DAY_OF_MONTH": "first",
                    "PREFER_DATES_FROM": prefer_dates_from,
                },
            )
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
