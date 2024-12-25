import logging
import re
from typing import List

from khoj.search_filter.base_filter import BaseFilter

logger = logging.getLogger(__name__)


class PersonFilter(BaseFilter):
    person_filter_regex = r'(?<!-)person=[\'"](.+?)[\'"] ?'
    excluded_person_filter_regex = r'-person=[\'"](.+?)[\'"] ?'

    def __init__(self, entry_key="person"):
        self.entry_key = entry_key

    def get_filter_terms(self, query: str) -> List[str]:
        "Get all filter terms in query"
        included_persons = [f"{included_person}" for included_person in re.findall(self.person_filter_regex, query)]
        excluded_persons = [
            f"-{excluded_person}" for excluded_person in re.findall(self.excluded_person_filter_regex, query)
        ]
        return list(map(lambda t: t.lower(), included_persons + excluded_persons))

    def defilter(self, query: str) -> str:
        return re.sub(self.person_filter_regex, "", query).strip()
