# Standard Packages
import glob
import logging
import time
from typing import Iterable

# Internal Packages
from src.processor.org_mode import orgnode
from src.processor.text_to_jsonl import TextToJsonl
from src.utils.helpers import get_absolute_path, is_none_or_empty
from src.utils.jsonl import dump_jsonl, compress_jsonl_data
from src.utils.rawconfig import Entry
from src.utils import state


logger = logging.getLogger(__name__)


class OrgToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries: list[Entry]=None):
        # Extract required fields from config
        org_files, org_file_filter, output_file = self.config.input_files, self.config.input_filter, self.config.compressed_jsonl
        index_heading_entries = self.config.index_heading_entries

        # Input Validation
        if is_none_or_empty(org_files) and is_none_or_empty(org_file_filter):
            print("At least one of org-files or org-file-filter is required to be specified")
            exit(1)

        # Get Org Files to Process
        start = time.time()
        org_files = OrgToJsonl.get_org_files(org_files, org_file_filter)

        # Extract Entries from specified Org files
        start = time.time()
        entry_nodes, file_to_entries = self.extract_org_entries(org_files)
        end = time.time()
        logger.debug(f"Parse entries from org files into OrgNode objects: {end - start} seconds")

        start = time.time()
        current_entries = self.convert_org_nodes_to_entries(entry_nodes, file_to_entries, index_heading_entries)
        end = time.time()
        logger.debug(f"Convert OrgNodes into entry dictionaries: {end - start} seconds")

        # Identify, mark and merge any new entries with previous entries
        if not previous_entries:
            entries_with_ids = list(enumerate(current_entries))
        else:
            entries_with_ids = self.mark_entries_for_update(current_entries, previous_entries, key='compiled', logger=logger)

        # Process Each Entry from All Notes Files
        start = time.time()
        entries = map(lambda entry: entry[1], entries_with_ids)
        jsonl_data = self.convert_org_entries_to_jsonl(entries)

        # Compress JSONL formatted Data
        if output_file.suffix == ".gz":
            compress_jsonl_data(jsonl_data, output_file)
        elif output_file.suffix == ".jsonl":
            dump_jsonl(jsonl_data, output_file)
        end = time.time()
        logger.debug(f"Write org entries to JSONL file: {end - start} seconds")

        return entries_with_ids

    @staticmethod
    def get_org_files(org_files=None, org_file_filters=None):
        "Get Org files to process"
        absolute_org_files, filtered_org_files = set(), set()
        if org_files:
            absolute_org_files = {
                get_absolute_path(org_file)
                for org_file
                in org_files
            }
        if org_file_filters:
            filtered_org_files = {
                filtered_file
                for org_file_filter in org_file_filters
                for filtered_file in glob.glob(get_absolute_path(org_file_filter))
            }

        all_org_files = sorted(absolute_org_files | filtered_org_files)

        files_with_non_org_extensions = {org_file for org_file in all_org_files if not org_file.endswith(".org")}
        if any(files_with_non_org_extensions):
            logger.warn(f"There maybe non org-mode files in the input set: {files_with_non_org_extensions}")

        logger.info(f'Processing files: {all_org_files}')

        return all_org_files

    @staticmethod
    def extract_org_entries(org_files):
        "Extract entries from specified Org files"
        entries = []
        entry_to_file_map = []
        for org_file in org_files:
            org_file_entries = orgnode.makelist(str(org_file))
            entry_to_file_map += zip(org_file_entries, [org_file]*len(org_file_entries))
            entries.extend(org_file_entries)

        return entries, dict(entry_to_file_map)

    @staticmethod
    def convert_org_nodes_to_entries(parsed_entries: list[orgnode.Orgnode], entry_to_file_map, index_heading_entries=False) -> list[Entry]:
        "Convert Org-Mode nodes into list of Entry objects"
        entries: list[Entry] = []
        for parsed_entry in parsed_entries:
            if not parsed_entry.hasBody and not index_heading_entries:
                # Ignore title notes i.e notes with just headings and empty body
                continue

            compiled = f'{parsed_entry.heading}.'
            if state.verbose > 2:
                logger.debug(f"Title: {parsed_entry.heading}")

            if parsed_entry.tags:
                tags_str = " ".join(parsed_entry.tags)
                compiled += f'\t {tags_str}.'
                if state.verbose > 2:
                    logger.debug(f"Tags: {tags_str}")

            if parsed_entry.closed:
                compiled += f'\n Closed on {parsed_entry.closed.strftime("%Y-%m-%d")}.'
                if state.verbose > 2:
                    logger.debug(f'Closed: {parsed_entry.closed.strftime("%Y-%m-%d")}')

            if parsed_entry.scheduled:
                compiled += f'\n Scheduled for {parsed_entry.scheduled.strftime("%Y-%m-%d")}.'
                if state.verbose > 2:
                    logger.debug(f'Scheduled: {parsed_entry.scheduled.strftime("%Y-%m-%d")}')

            if parsed_entry.hasBody:
                compiled += f'\n {parsed_entry.body}'
                if state.verbose > 2:
                    logger.debug(f"Body: {parsed_entry.body}")

            if compiled:
                entries += [Entry(
                    compiled=compiled,
                    raw=f'{parsed_entry}',
                    file=f'{entry_to_file_map[parsed_entry]}')]

        return entries

    @staticmethod
    def convert_org_entries_to_jsonl(entries: Iterable[Entry]) -> str:
        "Convert each Org-Mode entry to JSON and collate as JSONL"
        return ''.join([f'{entry_dict.to_json()}\n' for entry_dict in entries])
