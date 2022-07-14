# Standard Packages
import re
from datetime import timedelta, datetime
from dateutil.relativedelta import relativedelta, MO
from math import inf

# External Packages
import torch
import dateparser as dtparse


# Date Range Filter Regexes
# Example filter queries:
#   - dt>=yesterday dt<"tomorrow"
#   - dt>="last week"
#   - dt:"next year"
date_regex = r'(?:dt([:><=]{1,2})\"?([\w\-\/]+))?\"?'
date_range_regex=f'.*?\s+{date_regex}\s*{date_regex}.*?'


def date_filter(query, entries, embeddings):
    # extract date from query
    date_regex = r'\d{4}-\d{2}-\d{2}'
    dates_in_query = re.findall(date_regex, query)

    # if no date in query, return all entries
    if dates_in_query is None or len(dates_in_query) == 0:
        return query, entries, embeddings

    # remove dates from query
    query = re.sub(date_regex, '', query)

    # find entries with dates from query in them
    entries_to_include = set()
    for id, entry in enumerate(entries):
        for date in dates_in_query:
            if date in entry[1]:
                entries_to_include.add(id)

    # delete entries (and their embeddings) marked for exclusion
    entries_to_exclude = set(range(len(entries))) - entries_to_include
    for id in sorted(list(entries_to_exclude), reverse=True):
        del entries[id]
        embeddings = torch.cat((embeddings[:id], embeddings[id+1:]))

    return query, entries, embeddings

def parse(date_str, relative_base=None):
    "Parse date string passed in date filter of query to datetime object"
    # clean date string to handle future date parsing by date parser
    clean_date_str = re.sub(r'later|from now|from today', '', date_str)

    # parse date passed in query date filter
    parsed_date = dtparse.parse(
        clean_date_str,
        settings= {
            'RELATIVE_BASE': relative_base or datetime.now(),
            'PREFER_DAY_OF_MONTH': 'first',
            'PREFER_DATES_FROM': 'future'
        })

    if parsed_date is None:
        return None

    return date_to_daterange(parsed_date, date_str)


def date_to_daterange(parsed_date, date_str):
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
