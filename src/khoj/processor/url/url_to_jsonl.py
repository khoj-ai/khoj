# Standard Packages
import glob
import logging
from pathlib import Path
from typing import List

# External Packages
from langchain.document_loaders import PyPDFLoader

# Internal Packages
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.helpers import get_absolute_path, is_none_or_empty, timer
from khoj.utils.jsonl import compress_jsonl_data
from khoj.utils.rawconfig import Entry, TextConfigBase
from khoj.processor.url.web_crawler import scrape_text_with_selenium, get_webdriver

logger = logging.getLogger(__name__)


class URLToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries=[]):
        # Extract required fields from config
        urls, output_file = (
            self.config.input_files,
            self.config.compressed_jsonl,
        )

        urls = [str(u) for u in urls]
        logger.debug(f"input urls = {urls}")

        # Input Validation
        if is_none_or_empty(urls):
            print("At least one of pdf-files or pdf-file-filter is required to be specified")
            exit(1)

        # Get Pdf Files to Process
        # pdf_files = URLToJsonl.get_pdf_files(pdf_files, pdf_file_filter)

        # Extract Entries from specified Pdf files
        with timer("Parse entries from URL into dictionaries", logger):
            current_entries = URLToJsonl.convert_url_entries_to_maps(*URLToJsonl.extract_url_entries(urls))

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            entries_with_ids = TextToJsonl.mark_entries_for_update(
                current_entries, previous_entries, key="compiled", logger=logger
            )

        with timer("Write URL entries to JSONL file", logger):
            # Process Each Entry from All Notes Files
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            jsonl_data = URLToJsonl.convert_url_maps_to_jsonl(entries)

            # Compress JSONL formatted Data
            compress_jsonl_data(jsonl_data, output_file)

        return entries_with_ids

    @staticmethod
    def extract_url_entries(urls):
        """Extract entries by page from specified URL"""
        driver = get_webdriver()

        entries = []
        entry_to_location_map = {}
        for url in urls:
            try:
                url_entries_per_page = scrape_text_with_selenium(driver, url)
                entry_to_location_map[url_entries_per_page] = url
                entries.append(url_entries_per_page)
            except Exception as e:
                logger.warning(f"Unable to process url: {url}. This Page will not be indexed.")
                logger.warning(e)
        driver.quit()
        return entries, entry_to_location_map

    @staticmethod
    def convert_url_entries_to_maps(parsed_entries: List[str], entry_to_url_map) -> List[Entry]:
        "Convert each PDF entries into a dictionary"
        entries = []
        for parsed_entry in parsed_entries:
            entry_url = entry_to_url_map[parsed_entry]
            # Append base filename to compiled entry for context to model
            heading = f"{entry_url}\n"
            compiled_entry = f"{heading}{parsed_entry}"
            entries.append(
                Entry(
                    compiled=compiled_entry,
                    raw=parsed_entry,
                    heading=heading,
                    file=f"{entry_url}",
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} PDF entries to dictionaries")

        return entries

    @staticmethod
    def convert_url_maps_to_jsonl(entries: List[Entry]):
        "Convert each PDF entry to JSON and collate as JSONL"
        return "".join([f"{entry.to_json()}\n" for entry in entries])
