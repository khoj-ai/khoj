# Standard Packages
import re

# External Packages
import torch


def explicit_filter(raw_query, entries, embeddings):
    # Separate natural query from explicit required, blocked words filters
    query = " ".join([word for word in raw_query.split() if not word.startswith("+") and not word.startswith("-")])
    required_words = set([word[1:].lower() for word in raw_query.split() if word.startswith("+")])
    blocked_words = set([word[1:].lower() for word in raw_query.split() if word.startswith("-")])

    if len(required_words) == 0 and len(blocked_words) == 0:
        return query, entries, embeddings

    # convert each entry to a set of words
    entries_by_word_set = [set(word.lower()
                             for word
                             in re.split(
                                 r',|\.| |\]|\[\(|\)|\{|\}|\t|\n|\:',  # split on fullstop, comma or any brackets
                                 entry[1])
                             if word != "")
                        for entry in entries]

    # track id of entries to exclude
    entries_to_exclude = set()

    # mark entries that do not contain all required_words for exclusion
    if len(required_words) > 0:
        for id, words_in_entry in enumerate(entries_by_word_set):
            if not required_words.issubset(words_in_entry):
                entries_to_exclude.add(id)

    # mark entries that contain any blocked_words for exclusion
    if len(blocked_words) > 0:
        for id, words_in_entry in enumerate(entries_by_word_set):
            if words_in_entry.intersection(blocked_words):
                entries_to_exclude.add(id)

    # delete entries (and their embeddings) marked for exclusion
    for id in sorted(list(entries_to_exclude), reverse=True):
        del entries[id]
        embeddings = torch.cat((embeddings[:id], embeddings[id+1:]))

    return query, entries, embeddings