import os
import re

import pytest

from khoj.processor.content.pdf.pdf_to_entries import PdfToEntries


def test_single_page_pdf_to_jsonl():
    "Convert single page PDF file to jsonl."
    # Act
    # Extract Entries from specified Pdf files
    # Read singlepage.pdf into memory as bytes
    with open("tests/data/pdf/singlepage.pdf", "rb") as f:
        pdf_bytes = f.read()

    data = {"tests/data/pdf/singlepage.pdf": pdf_bytes}
    entries = PdfToEntries.extract_pdf_entries(pdf_files=data)

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 1


def test_multi_page_pdf_to_jsonl():
    "Convert multiple pages from single PDF file to jsonl."
    # Act
    # Extract Entries from specified Pdf files
    with open("tests/data/pdf/multipage.pdf", "rb") as f:
        pdf_bytes = f.read()

    data = {"tests/data/pdf/multipage.pdf": pdf_bytes}
    entries = PdfToEntries.extract_pdf_entries(pdf_files=data)

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 6


@pytest.mark.skip(reason="Temporarily disabled OCR due to performance issues")
def test_ocr_page_pdf_to_jsonl():
    "Convert multiple pages from single PDF file to jsonl."
    # Arrange
    expected_str = "playing on a strip of marsh"
    expected_str_with_variable_spaces = re.compile(expected_str.replace(" ", r"\s*"), re.IGNORECASE)

    # Extract Entries from specified Pdf files
    with open("tests/data/pdf/ocr_samples.pdf", "rb") as f:
        pdf_bytes = f.read()
    data = {"tests/data/pdf/ocr_samples.pdf": pdf_bytes}

    # Act
    entries = PdfToEntries.extract_pdf_entries(pdf_files=data)
    raw_entry = entries[1][0].raw

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 1
    assert re.search(expected_str_with_variable_spaces, raw_entry) is not None


# Helper Functions
def create_file(tmp_path, entry=None, filename="document.pdf"):
    pdf_file = tmp_path / filename
    pdf_file.touch()
    if entry:
        pdf_file.write_text(entry)
    return pdf_file
