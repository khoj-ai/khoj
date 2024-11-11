import logging
import tempfile
from typing import Dict, List, Tuple

from langchain_community.document_loaders import Docx2txtLoader

from khoj.database.models import Entry as DbEntry
from khoj.database.models import KhojUser
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import timer
from khoj.utils.rawconfig import Entry

logger = logging.getLogger(__name__)


class DocxToEntries(TextToEntries):
    def __init__(self):
        super().__init__()

    # Define Functions
    def process(self, files: dict[str, str], user: KhojUser, regenerate: bool = False) -> Tuple[int, int]:
        # Extract required fields from config
        deletion_file_names = set([file for file in files if files[file] == b""])
        files_to_process = set(files) - deletion_file_names
        files = {file: files[file] for file in files_to_process}

        # Extract Entries from specified Docx files
        with timer("Extract entries from specified DOCX files", logger):
            file_to_text_map, current_entries = DocxToEntries.extract_docx_entries(files)

        # Split entries by max tokens supported by model
        with timer("Split entries by max token size supported by model", logger):
            current_entries = self.split_entries_by_max_tokens(current_entries, max_tokens=256)

        # Identify, mark and merge any new entries with previous entries
        with timer("Identify new or updated entries", logger):
            num_new_embeddings, num_deleted_embeddings = self.update_embeddings(
                user,
                current_entries,
                DbEntry.EntryType.DOCX,
                DbEntry.EntrySource.COMPUTER,
                "compiled",
                logger,
                deletion_file_names,
                regenerate=regenerate,
                file_to_text_map=file_to_text_map,
            )

        return num_new_embeddings, num_deleted_embeddings

    @staticmethod
    def extract_docx_entries(docx_files) -> Tuple[Dict, List[Entry]]:
        """Extract entries from specified DOCX files"""

        entries: List[str] = []
        entry_to_location_map: List[Tuple[str, str]] = []
        file_to_text_map = dict()
        for docx_file in docx_files:
            try:
                docx_texts = DocxToEntries.extract_text(docx_files[docx_file])
                entry_to_location_map += zip(docx_texts, [docx_file] * len(docx_texts))
                entries.extend(docx_texts)
                file_to_text_map[docx_file] = docx_texts
            except Exception as e:
                logger.warning(f"Unable to extract entries from file: {docx_file}")
                logger.warning(e, exc_info=True)
        return file_to_text_map, DocxToEntries.convert_docx_entries_to_maps(entries, dict(entry_to_location_map))

    @staticmethod
    def convert_docx_entries_to_maps(parsed_entries: List[str], entry_to_file_map) -> List[Entry]:
        """Convert each DOCX entry into a dictionary"""
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

        logger.debug(f"Converted {len(parsed_entries)} DOCX entries to dictionaries")

        return entries

    @staticmethod
    def extract_text(docx_file):
        """Extract text from specified DOCX file"""
        try:
            docx_entry_by_pages = []
            # Create temp file with .docx extension that gets auto-deleted
            with tempfile.NamedTemporaryFile(suffix=".docx", delete=True) as tmp:
                tmp.write(docx_file)
                tmp.flush()  # Ensure all data is written

                # Load the content using Docx2txtLoader
                loader = Docx2txtLoader(tmp.name)
                docx_entries_per_file = loader.load()

                # Convert the loaded entries into the desired format
                docx_entry_by_pages = [page.page_content for page in docx_entries_per_file]
        except Exception as e:
            logger.warning(f"Unable to extract text from file: {docx_file}")
            logger.warning(e, exc_info=True)

        return docx_entry_by_pages
