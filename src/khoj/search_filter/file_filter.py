import logging
import re
from collections import defaultdict
from typing import List

from khoj.search_filter.base_filter import BaseFilter
from khoj.utils.helpers import LRU

logger = logging.getLogger(__name__)


class FileFilter(BaseFilter):
    file_filter_regex = r'(?<!-)file:"(.+?)" ?'
    excluded_file_filter_regex = r'-file:"(.+?)" ?'

    def __init__(self, entry_key="file"):
        self.entry_key = entry_key
        self.file_to_entry_map = defaultdict(set)
        self.cache = LRU()

    def get_filter_terms(self, query: str) -> List[str]:
        "Get all filter terms in query"
        required_files = [f"{required_file}" for required_file in re.findall(self.file_filter_regex, query)]
        excluded_files = [f"-{excluded_file}" for excluded_file in re.findall(self.excluded_file_filter_regex, query)]
        return required_files + excluded_files

    def convert_to_regex(self, file_filter: str) -> str:
        "Convert file filter to regex"
        return file_filter.replace(".", r"\.").replace("*", r".*")

    def defilter(self, query: str) -> str:
        return re.sub(self.file_filter_regex, "", query).strip()
