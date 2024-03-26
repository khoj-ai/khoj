import os

from khoj.processor.content.pdf.pdf_to_entries import PdfToEntries
from khoj.utils.fs_syncer import get_pdf_files
from khoj.utils.rawconfig import TextContentConfig


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
    assert len(entries) == 1


def test_multi_page_pdf_to_jsonl():
    "Convert multiple pages from single PDF file to jsonl."
    # Act
    # Extract Entries from specified Pdf files
    with open("tests/data/pdf/multipage.pdf", "rb") as f:
        pdf_bytes = f.read()

    data = {"tests/data/pdf/multipage.pdf": pdf_bytes}
    entries = PdfToEntries.extract_pdf_entries(pdf_files=data)

    # Assert
    assert len(entries) == 6


def test_ocr_page_pdf_to_jsonl():
    "Convert multiple pages from single PDF file to jsonl."
    # Act
    # Extract Entries from specified Pdf files
    with open("tests/data/pdf/ocr_samples.pdf", "rb") as f:
        pdf_bytes = f.read()

    data = {"tests/data/pdf/ocr_samples.pdf": pdf_bytes}
    entries = PdfToEntries.extract_pdf_entries(pdf_files=data)

    assert len(entries) == 1
    assert "playing on a strip of marsh" in entries[0].raw


def test_get_pdf_files(tmp_path):
    "Ensure Pdf files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.pdf")
    group1_file2 = create_file(tmp_path, filename="group1-file2.pdf")
    group2_file1 = create_file(tmp_path, filename="group2-file1.pdf")
    group2_file2 = create_file(tmp_path, filename="group2-file2.pdf")
    # Include via input-file field
    file1 = create_file(tmp_path, filename="document.pdf")
    # Not included by any filter
    create_file(tmp_path, filename="not-included-document.pdf")
    create_file(tmp_path, filename="not-included-text.txt")

    expected_files = set(
        [os.path.join(tmp_path, file.name) for file in [group1_file1, group1_file2, group2_file1, group2_file2, file1]]
    )

    # Setup input-files, input-filters
    input_files = [tmp_path / "document.pdf"]
    input_filter = [tmp_path / "group1*.pdf", tmp_path / "group2*.pdf"]

    pdf_config = TextContentConfig(
        input_files=input_files,
        input_filter=[str(path) for path in input_filter],
        compressed_jsonl=tmp_path / "test.jsonl",
        embeddings_file=tmp_path / "test_embeddings.jsonl",
    )

    # Act
    extracted_pdf_files = get_pdf_files(pdf_config)

    # Assert
    assert len(extracted_pdf_files) == 5
    assert set(extracted_pdf_files.keys()) == expected_files


# Helper Functions
def create_file(tmp_path, entry=None, filename="document.pdf"):
    pdf_file = tmp_path / filename
    pdf_file.touch()
    if entry:
        pdf_file.write_text(entry)
    return pdf_file
