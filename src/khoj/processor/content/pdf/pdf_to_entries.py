import base64
import logging
import os
from datetime import datetime
from typing import Dict, List, Tuple

from langchain_community.document_loaders import PyMuPDFLoader

# importing FileObjectAdapter so that we can add new files and debug file object db.
# from khoj.database.adapters import FileObjectAdapters
from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

logger = logging.getLogger(__name__)


class PdfToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(self, files: dict[str, str] = None, user: KhojUser = None, regenerate: bool = False) -> Tuple[int, int]:
        # Extract required fields from config
        deletion_file_names = set([file for file in files if files[file] == b""])
        files_to_process = set(files) - deletion_file_names
        files = {file: files[file] for file in files_to_process}

        # Extract Entries from specified Pdf files
        with timer("Extract entries from specified PDF files", logger):
            file_to_text_map, current_entries = PdfToEntries.extract_pdf_entries(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                current_entries,
                DbEntry.EntryType.PDF,
                DbEntry.EntrySource.COMPUTER,
                "compiled",
                logger,
                deletion_file_names,
                user,
                regenerate=regenerate,
                file_to_text_map=file_to_text_map,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_pdf_entries(pdf_files) -> Tuple[Dict, List[Entry]]:  # important function
        """Extract entries by page from specified PDF files"""
        file_to_text_map = dict()
        entries: List[str] = []
        entry_to_location_map: List[Tuple[str, str]] = []
        for pdf_file in pdf_files:
            try:
                # Write the PDF file to a temporary file, as it is stored in byte format in the pdf_file object and the PDF Loader expects a file path
                timestamp_now = datetime.utcnow().timestamp()
                tmp_file = f"tmp_pdf_file_{timestamp_now}.pdf"
                with open(f"{tmp_file}", "wb") as f:
                    bytes = pdf_files[pdf_file]
                    f.write(bytes)
                try:
                    loader = PyMuPDFLoader(f"{tmp_file}", extract_images=True)
                    pdf_entries_per_file = [page.page_content for page in loader.load()]
                except ImportError:
                    loader = PyMuPDFLoader(f"{tmp_file}")
                    pdf_entries_per_file = [
                        page.page_content for page in loader.load()
                    ]  # page_content items list for a given pdf.
                entry_to_location_map += zip(
                    pdf_entries_per_file, [pdf_file] * len(pdf_entries_per_file)
                )  # this is an indexed map of pdf_entries for the pdf.
                entries.extend(pdf_entries_per_file)
                file_to_text_map[pdf_file] = pdf_entries_per_file
            except Exception as e:
                logger.warning(f"Unable to process file: {pdf_file}. This file will not be indexed.")
                logger.warning(e, exc_info=True)
            finally:
                if os.path.exists(f"{tmp_file}"):
                    os.remove(f"{tmp_file}")

        return file_to_text_map, PdfToEntries.convert_pdf_entries_to_maps(entries, dict(entry_to_location_map))

    @staticmethod
    def convert_pdf_entries_to_maps(parsed_entries: List[str], entry_to_file_map) -> List[Entry]:
        "Convert each PDF entries into a dictionary"
        entries = []
        for parsed_entry in parsed_entries:
            entry_filename = entry_to_file_map[parsed_entry]
            # Append base filename to compiled entry for context to model
            heading = f"{entry_filename}\n"
            compiled_entry = f"{heading}{parsed_entry}"
            entries.append(
                Entry(
                    compiled=compiled_entry,
                    raw=parsed_entry,
                    heading=heading,
                    file=f"{entry_filename}",
                )
            )

        logger.debug(f"Converted {len(parsed_entries)} PDF entries to dictionaries")

        return entries
