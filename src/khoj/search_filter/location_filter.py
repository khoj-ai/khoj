import logging
import re
from typing import List

from khoj.search_filter.base_filter import BaseFilter

logger = logging.getLogger(__name__)


class LocationFilter(BaseFilter):
    location_filter_regex = r'(?<!-)location=[\'"](.+?)[\'"] ?'
    excluded_location_filter_regex = r'-location=[\'"](.+?)[\'"] ?'

    def __init__(self, entry_key="location"):
        self.entry_key = entry_key

    def get_filter_terms(self, query: str) -> List[str]:
        "Get all filter terms in query"
        mandatory_locations = [
            f"{required_location}" for required_location in re.findall(self.location_filter_regex, query)
        ]
        prohibited_locations = [
            f"-{prohibited_location}" for prohibited_location in re.findall(self.excluded_location_filter_regex, query)
        ]
        return list(map(lambda t: t.lower(), mandatory_locations + prohibited_locations))

    def defilter(self, query: str) -> str:
        return re.sub(self.location_filter_regex, "", query).strip()
