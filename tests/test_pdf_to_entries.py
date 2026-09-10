import os
import re
import tempfile
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

from khoj.processor.content.pdf.pdf_to_entries import PdfToEntries


@pytest.fixture
def tracked_pdf_tempfiles(monkeypatch, tmp_path):
    """Track real temporary files without touching the working directory."""
    created = []
    named_temporary_file = tempfile.NamedTemporaryFile

    def create_tempfile(*args, **kwargs):
        kwargs["dir"] = tmp_path
        handle = named_temporary_file(*args, **kwargs)
        created.append(handle)
        return handle

    monkeypatch.setattr("khoj.processor.content.pdf.pdf_to_entries.tempfile.NamedTemporaryFile", create_tempfile)
    yield created
    for handle in created:
        handle.close()
        if os.path.exists(handle.name):
            os.unlink(handle.name)


def test_extract_text_closes_tempfile_before_loader_and_cleans_up(monkeypatch, tracked_pdf_tempfiles):
    """The loader can reopen the file only after the writer has closed it."""
    observed = {}

    class FakeLoader:
        def __init__(self, path):
            observed["closed_at_construction"] = tracked_pdf_tempfiles[0].closed
            with open(path, "rb") as file:
                observed["content"] = file.read()

        def load(self):
            return [SimpleNamespace(page_content="hello\x00")]

    monkeypatch.setattr("khoj.processor.content.pdf.pdf_to_entries.PyMuPDFLoader", FakeLoader)

    assert PdfToEntries.extract_text(b"fake pdf bytes") == ["hello"]
    assert observed == {"closed_at_construction": True, "content": b"fake pdf bytes"}
    assert len(tracked_pdf_tempfiles) == 1
    assert tracked_pdf_tempfiles[0].closed
    assert not os.path.exists(tracked_pdf_tempfiles[0].name)


@pytest.mark.parametrize("failure_stage", ["init", "load"])
def test_extract_text_cleans_up_tempfile_on_loader_error(monkeypatch, tracked_pdf_tempfiles, failure_stage):
    """Cleanup also runs when either loader construction or loading fails."""
    observed = {}

    class RaisingLoader:
        def __init__(self, path):
            observed["closed_at_construction"] = tracked_pdf_tempfiles[0].closed
            observed["exists_at_construction"] = os.path.exists(path)
            if failure_stage == "init":
                raise RuntimeError("loader construction failed")

        def load(self):
            raise RuntimeError("loading failed")

    monkeypatch.setattr("khoj.processor.content.pdf.pdf_to_entries.PyMuPDFLoader", RaisingLoader)

    assert PdfToEntries.extract_text(b"fake pdf bytes") == []
    assert observed == {"closed_at_construction": True, "exists_at_construction": True}
    assert len(tracked_pdf_tempfiles) == 1
    assert tracked_pdf_tempfiles[0].closed
    assert not os.path.exists(tracked_pdf_tempfiles[0].name)


@pytest.mark.parametrize("operation", ["write", "flush"])
def test_extract_text_closes_and_removes_tempfile_on_write_error(monkeypatch, tracked_pdf_tempfiles, operation):
    """An I/O error must not leave a handle open and prevent Windows cleanup."""
    named_temporary_file = tempfile.NamedTemporaryFile

    def fail_io(*args, **kwargs):
        raise OSError("temporary file I/O failed")

    def create_failing_tempfile(*args, **kwargs):
        handle = named_temporary_file(*args, **kwargs)
        monkeypatch.setattr(handle, operation, fail_io)
        return handle

    loader = Mock()
    monkeypatch.setattr(
        "khoj.processor.content.pdf.pdf_to_entries.tempfile.NamedTemporaryFile", create_failing_tempfile
    )
    monkeypatch.setattr("khoj.processor.content.pdf.pdf_to_entries.PyMuPDFLoader", loader)

    assert PdfToEntries.extract_text(b"fake pdf bytes") == []
    loader.assert_not_called()
    assert len(tracked_pdf_tempfiles) == 1
    assert tracked_pdf_tempfiles[0].closed
    assert not os.path.exists(tracked_pdf_tempfiles[0].name)


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
