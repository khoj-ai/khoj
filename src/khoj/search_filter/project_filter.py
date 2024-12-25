import logging
import re
from typing import List

from khoj.search_filter.base_filter import BaseFilter

logger = logging.getLogger(__name__)


class ProjectFilter(BaseFilter):
    project_filter_regex = r'(?<!-)project=[\'"](.+?)[\'"] ?'
    excluded_project_filter_regex = r'-project=[\'"](.+?)[\'"] ?'

    def __init__(self, entry_key="project"):
        self.entry_key = entry_key

    def get_filter_terms(self, query: str) -> List[str]:
        "Get all filter terms in query"
        included_projects = [f"{included_project}" for included_project in re.findall(self.project_filter_regex, query)]
        excluded_projects = [
            f"-{excluded_project}" for excluded_project in re.findall(self.excluded_project_filter_regex, query)
        ]
        return list(map(lambda t: t.lower(), included_projects + excluded_projects))

    def defilter(self, query: str) -> str:
        return re.sub(self.project_filter_regex, "", query).strip()
