# Standard Packages
import re

# External Packages
import torch


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