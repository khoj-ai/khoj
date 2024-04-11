import glob
import logging
import os
from pathlib import Path
from typing import Optional

from bs4 import BeautifulSoup
from magika import Magika

from khoj.database.models import (
    LocalMarkdownConfig,
    LocalOrgConfig,
    LocalPdfConfig,
    LocalPlaintextConfig,
)
from khoj.utils.config import SearchType
from khoj.utils.helpers import get_absolute_path, is_none_or_empty
from khoj.utils.rawconfig import TextContentConfig

logger = logging.getLogger(__name__)
magika = Magika()


def collect_files(search_type: Optional[SearchType] = SearchType.All, user=None) -> dict:
    files = {}

    if search_type == SearchType.All or search_type == SearchType.Org:
        org_config = LocalOrgConfig.objects.filter(user=user).first()
        files["org"] = get_org_files(construct_config_from_db(org_config)) if org_config else {}
    if search_type == SearchType.All or search_type == SearchType.Markdown:
        markdown_config = LocalMarkdownConfig.objects.filter(user=user).first()
        files["markdown"] = get_markdown_files(construct_config_from_db(markdown_config)) if markdown_config else {}
    if search_type == SearchType.All or search_type == SearchType.Plaintext:
        plaintext_config = LocalPlaintextConfig.objects.filter(user=user).first()
        files["plaintext"] = get_plaintext_files(construct_config_from_db(plaintext_config)) if plaintext_config else {}
    if search_type == SearchType.All or search_type == SearchType.Pdf:
        pdf_config = LocalPdfConfig.objects.filter(user=user).first()
        files["pdf"] = get_pdf_files(construct_config_from_db(pdf_config)) if pdf_config else {}
    return files


def construct_config_from_db(db_config) -> TextContentConfig:
    return TextContentConfig(
        input_files=db_config.input_files,
        input_filter=db_config.input_filter,
        index_heading_entries=db_config.index_heading_entries,
    )


def get_plaintext_files(config: TextContentConfig) -> dict[str, str]:
    def is_plaintextfile(file: str):
        "Check if file is plaintext file"
        # Check if file path exists
        content_group = magika.identify_path(Path(file)).output.group
        # Use file extension to decide plaintext if file content is not identifiable
        valid_text_file_extensions = ("txt", "md", "markdown", "org" "mbox", "rst", "html", "htm", "xml")
        return file.endswith(valid_text_file_extensions) or content_group in ["text", "code"]

    def extract_html_content(html_content: str):
        "Extract content from HTML"
        soup = BeautifulSoup(html_content, "html.parser")
        return soup.get_text(strip=True, separator="\n")

    # Extract required fields from config
    input_files, input_filters = (
        config.input_files,
        config.input_filter,
    )

    # Input Validation
    if is_none_or_empty(input_files) and is_none_or_empty(input_filters):
        logger.debug("At least one of input-files or input-file-filter is required to be specified")
        return {}

    # Get all plain text files to process
    absolute_plaintext_files, filtered_plaintext_files = set(), set()
    if input_files:
        absolute_plaintext_files = {get_absolute_path(jsonl_file) for jsonl_file in input_files}
    if input_filters:
        filtered_plaintext_files = {
            filtered_file
            for plaintext_file_filter in input_filters
            for filtered_file in glob.glob(get_absolute_path(plaintext_file_filter), recursive=True)
            if os.path.isfile(filtered_file)
        }

    all_target_files = sorted(absolute_plaintext_files | filtered_plaintext_files)

    files_with_no_plaintext_extensions = {
        target_files for target_files in all_target_files if not is_plaintextfile(target_files)
    }
    if any(files_with_no_plaintext_extensions):
        logger.warning(f"Skipping unsupported files from plaintext indexing: {files_with_no_plaintext_extensions}")
        all_target_files = list(set(all_target_files) - files_with_no_plaintext_extensions)

    logger.debug(f"Processing files: {all_target_files}")

    filename_to_content_map = {}
    for file in all_target_files:
        with open(file, "r", encoding="utf8") as f:
            try:
                plaintext_content = f.read()
                if file.endswith(("html", "htm", "xml")):
                    plaintext_content = extract_html_content(plaintext_content)
                filename_to_content_map[file] = plaintext_content
            except Exception as e:
                logger.warning(f"Unable to read file: {file} as plaintext. Skipping file.")
                logger.warning(e, exc_info=True)

    return filename_to_content_map


def get_org_files(config: TextContentConfig):
    # Extract required fields from config
    org_files, org_file_filters = (
        config.input_files,
        config.input_filter,
    )

    # Input Validation
    if is_none_or_empty(org_files) and is_none_or_empty(org_file_filters):
        logger.debug("At least one of org-files or org-file-filter is required to be specified")
        return {}

    "Get Org files to process"
    absolute_org_files, filtered_org_files = set(), set()
    if org_files:
        absolute_org_files = {get_absolute_path(org_file) for org_file in org_files}
    if org_file_filters:
        filtered_org_files = {
            filtered_file
            for org_file_filter in org_file_filters
            for filtered_file in glob.glob(get_absolute_path(org_file_filter), recursive=True)
            if os.path.isfile(filtered_file)
        }

    all_org_files = sorted(absolute_org_files | filtered_org_files)

    files_with_non_org_extensions = {org_file for org_file in all_org_files if not org_file.endswith(".org")}
    if any(files_with_non_org_extensions):
        logger.warning(f"There maybe non org-mode files in the input set: {files_with_non_org_extensions}")

    logger.debug(f"Processing files: {all_org_files}")

    filename_to_content_map = {}
    for file in all_org_files:
        with open(file, "r", encoding="utf8") as f:
            try:
                filename_to_content_map[file] = f.read()
            except Exception as e:
                logger.warning(f"Unable to read file: {file} as org. Skipping file.")
                logger.warning(e, exc_info=True)

    return filename_to_content_map


def get_markdown_files(config: TextContentConfig):
    # Extract required fields from config
    markdown_files, markdown_file_filters = (
        config.input_files,
        config.input_filter,
    )

    # Input Validation
    if is_none_or_empty(markdown_files) and is_none_or_empty(markdown_file_filters):
        logger.debug("At least one of markdown-files or markdown-file-filter is required to be specified")
        return {}

    # Get markdown files to process
    absolute_markdown_files, filtered_markdown_files = set(), set()
    if markdown_files:
        absolute_markdown_files = {get_absolute_path(markdown_file) for markdown_file in markdown_files}

    if markdown_file_filters:
        filtered_markdown_files = {
            filtered_file
            for markdown_file_filter in markdown_file_filters
            for filtered_file in glob.glob(get_absolute_path(markdown_file_filter), recursive=True)
            if os.path.isfile(filtered_file)
        }

    all_markdown_files = sorted(absolute_markdown_files | filtered_markdown_files)

    files_with_non_markdown_extensions = {
        md_file for md_file in all_markdown_files if not md_file.endswith(".md") and not md_file.endswith(".markdown")
    }

    if any(files_with_non_markdown_extensions):
        logger.warning(
            f"[Warning] There maybe non markdown-mode files in the input set: {files_with_non_markdown_extensions}"
        )

    logger.debug(f"Processing files: {all_markdown_files}")

    filename_to_content_map = {}
    for file in all_markdown_files:
        with open(file, "r", encoding="utf8") as f:
            try:
                filename_to_content_map[file] = f.read()
            except Exception as e:
                logger.warning(f"Unable to read file: {file} as markdown. Skipping file.")
                logger.warning(e, exc_info=True)

    return filename_to_content_map


def get_pdf_files(config: TextContentConfig):
    # Extract required fields from config
    pdf_files, pdf_file_filters = (
        config.input_files,
        config.input_filter,
    )

    # Input Validation
    if is_none_or_empty(pdf_files) and is_none_or_empty(pdf_file_filters):
        logger.debug("At least one of pdf-files or pdf-file-filter is required to be specified")
        return {}

    # Get PDF files to process
    absolute_pdf_files, filtered_pdf_files = set(), set()
    if pdf_files:
        absolute_pdf_files = {get_absolute_path(pdf_file) for pdf_file in pdf_files}
    if pdf_file_filters:
        filtered_pdf_files = {
            filtered_file
            for pdf_file_filter in pdf_file_filters
            for filtered_file in glob.glob(get_absolute_path(pdf_file_filter), recursive=True)
            if os.path.isfile(filtered_file)
        }

    all_pdf_files = sorted(absolute_pdf_files | filtered_pdf_files)

    files_with_non_pdf_extensions = {pdf_file for pdf_file in all_pdf_files if not pdf_file.endswith(".pdf")}

    if any(files_with_non_pdf_extensions):
        logger.warning(f"[Warning] There maybe non pdf-mode files in the input set: {files_with_non_pdf_extensions}")

    logger.debug(f"Processing files: {all_pdf_files}")

    filename_to_content_map = {}
    for file in all_pdf_files:
        with open(file, "rb") as f:
            try:
                filename_to_content_map[file] = f.read()
            except Exception as e:
                logger.warning(f"Unable to read file: {file} as PDF. Skipping file.")
                logger.warning(e, exc_info=True)

    return filename_to_content_map
