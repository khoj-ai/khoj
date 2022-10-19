# Standard Packages
import re
import time
import logging
from collections import defaultdict
from datetime import timedelta, datetime
from dateutil.relativedelta import relativedelta
from math import inf

# External Packages
import dateparser as dtparse

# Internal Packages
from src.search_filter.base_filter import BaseFilter
from src.utils.helpers import LRU


logger = logging.getLogger(__name__)


class DateFilter(BaseFilter):
    # Date Range Filter Regexes
    # Example filter queries:
    # - dt>="yesterday" dt<"tomorrow"
    # - dt>="last week"
    # - dt:"2 years ago"
    date_regex = r"dt([:><=]{1,2})\"(.*?)\""


    def __init__(self, entry_key='raw'):
        self.entry_key = entry_key
        self.date_to_entry_ids = defaultdict(set)
        self.cache = LRU()


    def load(self, entries, **_):
        start = time.time()
        for id, entry in enumerate(entries):
            # Extract dates from entry
            for date_in_entry_string in re.findall(r'\d{4}-\d{2}-\d{2}', getattr(entry, self.entry_key)):
                # Convert date string in entry to unix timestamp
                try:
                    date_in_entry = datetime.strptime(date_in_entry_string, '%Y-%m-%d').timestamp()
                except ValueError:
                    continue
                self.date_to_entry_ids[date_in_entry].add(id)
        end = time.time()
        logger.debug(f"Created date filter index: {end - start} seconds")


    def can_filter(self, raw_query):
        "Check if query contains date filters"
        return self.extract_date_range(raw_query) is not None


    def apply(self, query, raw_entries):
        "Find entries containing any dates that fall within date range specified in query"
        # extract date range specified in date filter of query
        start = time.time()
        query_daterange = self.extract_date_range(query)
        end = time.time()
        logger.debug(f"Extract date range to filter from query: {end - start} seconds")

        # if no date in query, return all entries
        if query_daterange is None:
            return query, set(range(len(raw_entries)))

        # remove date range filter from query
        query = re.sub(rf'\s+{self.date_regex}', ' ', query)
        query = re.sub(r'\s{2,}', ' ', query).strip()  # remove multiple spaces

        # return results from cache if exists
        cache_key = tuple(query_daterange)
        if cache_key in self.cache:
            logger.info(f"Return date filter results from cache")
            entries_to_include = self.cache[cache_key]
            return query, entries_to_include

        if not self.date_to_entry_ids:
            self.load(raw_entries)

        # find entries containing any dates that fall with date range specified in query
        start = time.time()
        entries_to_include = set()
        for date_in_entry in self.date_to_entry_ids.keys():
            # Check if date in entry is within date range specified in query
            if query_daterange[0] <= date_in_entry < query_daterange[1]:
                entries_to_include |= self.date_to_entry_ids[date_in_entry]
        end = time.time()
        logger.debug(f"Mark entries satisfying filter: {end - start} seconds")

        # cache results
        self.cache[cache_key] = entries_to_include

        return query, entries_to_include


    def extract_date_range(self, query):
        # find date range filter in query
        date_range_matches = re.findall(self.date_regex, query)

        if len(date_range_matches) == 0:
            return None

        # extract, parse natural dates ranges from date range filter passed in query
        # e.g today maps to (start_of_day, start_of_tomorrow)
        date_ranges_from_filter = []
        for (cmp, date_str) in date_range_matches:
            if self.parse(date_str):
                dt_start, dt_end = self.parse(date_str)
                date_ranges_from_filter += [[cmp, (dt_start.timestamp(), dt_end.timestamp())]]

        # Combine dates with their comparators to form date range intervals
        # For e.g
        #   >=yesterday maps to [start_of_yesterday, inf)
        #   <tomorrow maps to [0, start_of_tomorrow)
        # ---
        effective_date_range = [0, inf]
        date_range_considering_comparator = []
        for cmp, (dtrange_start, dtrange_end) in date_ranges_from_filter:
            if cmp == '>':
                date_range_considering_comparator += [[dtrange_end, inf]]
            elif cmp == '>=':
                date_range_considering_comparator += [[dtrange_start, inf]]
            elif cmp == '<':
                date_range_considering_comparator += [[0, dtrange_start]]
            elif cmp == '<=':
                date_range_considering_comparator += [[0, dtrange_end]]
            elif cmp == '=' or cmp == ':' or cmp == '==':
                date_range_considering_comparator += [[dtrange_start, dtrange_end]]

        # Combine above intervals (via AND/intersect)
        # In the above example, this gives us [start_of_yesterday, start_of_tomorrow)
        # This is the effective date range to filter entries by
        # ---
        for date_range in date_range_considering_comparator:
            effective_date_range = [
                max(effective_date_range[0], date_range[0]),
                min(effective_date_range[1], date_range[1])]

        if effective_date_range == [0, inf] or effective_date_range[0] > effective_date_range[1]:
            return None
        else:
            return effective_date_range


    def parse(self, date_str, relative_base=None):
        "Parse date string passed in date filter of query to datetime object"
        # clean date string to handle future date parsing by date parser
        future_strings = ['later', 'from now', 'from today']
        prefer_dates_from = {True: 'future', False: 'past'}[any([True for fstr in future_strings if fstr in date_str])]
        clean_date_str = re.sub('|'.join(future_strings), '', date_str)

        # parse date passed in query date filter
        parsed_date = dtparse.parse(
            clean_date_str,
            settings= {
                'RELATIVE_BASE': relative_base or datetime.now(),
                'PREFER_DAY_OF_MONTH': 'first',
                'PREFER_DATES_FROM': prefer_dates_from
            })

        if parsed_date is None:
            return None

        return self.date_to_daterange(parsed_date, date_str)


    def date_to_daterange(self, parsed_date, date_str):
        "Convert parsed date to date ranges at natural granularity (day, week, month or year)"

        start_of_day = parsed_date.replace(hour=0, minute=0, second=0, microsecond=0)

        if 'year' in date_str:
            return (datetime(parsed_date.year, 1, 1, 0, 0, 0), datetime(parsed_date.year+1, 1, 1, 0, 0, 0))
        if 'month' in date_str:
            start_of_month = datetime(parsed_date.year, parsed_date.month, 1, 0, 0, 0)
            next_month = start_of_month + relativedelta(months=1)
            return (start_of_month, next_month)
        if 'week' in date_str:
            # if week in date string, dateparser parses it to next week start
            # so today = end of this week
            start_of_week = start_of_day - timedelta(days=7)
            return (start_of_week, start_of_day)
        else:
            next_day = start_of_day + relativedelta(days=1)
        return (start_of_day, next_day)
