# Standard Packages
import glob
import logging
import re
from pathlib import Path
from typing import List

# Internal Packages
from khoj.processor.text_to_jsonl import TextToJsonl
from khoj.utils.helpers import get_absolute_path, is_none_or_empty, timer
from khoj.utils.constants import empty_escape_sequences
from khoj.utils.jsonl import dump_jsonl, compress_jsonl_data
from khoj.utils.rawconfig import Entry


logger = logging.getLogger(__name__)


class PdfToJsonl(TextToJsonl):
    # Define Functions
    def process(self, previous_entries=None):
        # Extract required fields from config
        pdf_files, pdf_file_filter, output_file = (
            self.config.input_files,
            self.config.input_filter,
            self.config.compressed_jsonl,
        )

        # Input Validation
        if is_none_or_empty(pdf_files) and is_none_or_empty(pdf_file_filter):
            print("At least one of pdf-files or pdf-file-filter is required to be specified")
            exit(1)

        # Get Pdf Files to Process
        pdf_files = PdfToJsonl.get_pdf_files(pdf_files, pdf_file_filter)

        # Extract Entries from specified Pdf files
        with timer("Parse entries from PDF files into dictionaries", logger):
            current_entries = PdfToJsonl.convert_pdf_entries_to_maps(*PdfToJsonl.extract_pdf_entries(pdf_files))

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            if not previous_entries:
                entries_with_ids = list(enumerate(current_entries))
            else:
                entries_with_ids = self.mark_entries_for_update(
                    current_entries, previous_entries, key="compiled", logger=logger
                )

        with timer("Write PDF entries to JSONL file", logger):
            # Process Each Entry from All Notes Files
            entries = list(map(lambda entry: entry[1], entries_with_ids))
            jsonl_data = PdfToJsonl.convert_pdf_maps_to_jsonl(entries)

            # Compress JSONL formatted Data
            if output_file.suffix == ".gz":
                compress_jsonl_data(jsonl_data, output_file)
            elif output_file.suffix == ".jsonl":
                dump_jsonl(jsonl_data, output_file)

        return entries_with_ids

    @staticmethod
    def get_pdf_files(pdf_files=None, pdf_file_filters=None):
        "Get PDF files to process"
        absolute_pdf_files, filtered_pdf_files = set(), set()
        if pdf_files:
            absolute_pdf_files = {get_absolute_path(pdf_file) for pdf_file in pdf_files}
        if pdf_file_filters:
            filtered_pdf_files = {
                filtered_file
                for pdf_file_filter in pdf_file_filters
                for filtered_file in glob.glob(get_absolute_path(pdf_file_filter), recursive=True)
            }

        all_pdf_files = sorted(absolute_pdf_files | filtered_pdf_files)

        files_with_non_pdf_extensions = {pdf_file for pdf_file in all_pdf_files if not pdf_file.endswith(".pdf")}

        if any(files_with_non_pdf_extensions):
            logger.warn(f"[Warning] There maybe non pdf-mode files in the input set: {files_with_non_pdf_extensions}")

        logger.debug(f"Processing files: {all_pdf_files}")

        return all_pdf_files

    @staticmethod
    def extract_pdf_entries(pdf_files):
        """Extract entries by heading from specified PDF files"""

        # Regex to extract PDF Entries by Heading
        pdf_heading_regex = r"^#"

        entries = []
        entry_to_file_map = []
        for pdf_file in pdf_files:
            with open(pdf_file, "r", encoding="utf8") as f:
                pdf_content = f.read()
                pdf_entries_per_file = []
                any_headings = re.search(pdf_heading_regex, pdf_content, flags=re.MULTILINE)
                for entry in re.split(pdf_heading_regex, pdf_content, flags=re.MULTILINE):
                    # Add heading level as the regex split removed it from entries with headings
                    prefix = "#" if entry.startswith("#") else "# " if any_headings else ""
                    stripped_entry = entry.strip(empty_escape_sequences)
                    if stripped_entry != "":
                        pdf_entries_per_file.append(f"{prefix}{stripped_entry}")

                entry_to_file_map += zip(pdf_entries_per_file, [pdf_file] * len(pdf_entries_per_file))
                entries.extend(pdf_entries_per_file)

        return entries, dict(entry_to_file_map)

    @staticmethod
    def convert_pdf_entries_to_maps(parsed_entries: List[str], entry_to_file_map) -> List[Entry]:
        "Convert each PDF entries into a dictionary"
        entries = []
        for parsed_entry in parsed_entries:
            entry_filename = Path(entry_to_file_map[parsed_entry])
            heading = parsed_entry.splitlines()[0] if re.search("^#+\s", parsed_entry) else ""
            # Append base filename to compiled entry for context to model
            # Increment heading level for heading entries and make filename as its top level heading
            prefix = f"# {entry_filename.stem}\n#" if heading else f"# {entry_filename.stem}\n"
            compiled_entry = f"{prefix}{parsed_entry}"
            entries.append(
                Entry(
                    compiled=compiled_entry,
                    raw=parsed_entry,
                    heading=f"{prefix}{heading}",
                    file=f"{entry_filename}",
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} pdf entries to dictionaries")

        return entries

    @staticmethod
    def convert_pdf_maps_to_jsonl(entries: List[Entry]):
        "Convert each PDF entry to JSON and collate as JSONL"
        return "".join([f"{entry.to_json()}\n" for entry in entries])
