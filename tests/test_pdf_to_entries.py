import hashlib
import re
from unittest.mock import Mock

import pytest

from khoj.database.models import Entry as DbEntry
from khoj.processor.content.pdf.pdf_to_entries import PdfToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.search_type.text_search import collate_results
from khoj.utils import state
from tests.helpers import SearchModelFactory


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
    assert entries[1][0].uri == "file://tests/data/pdf/singlepage.pdf#page=1"
    assert entries[1][0].heading == "tests/data/pdf/singlepage.pdf (page 1)\n"


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
    for page_number, entry in enumerate(entries[1], start=1):
        assert entry.uri == f"file://tests/data/pdf/multipage.pdf#page={page_number}"
        assert entry.compiled.startswith(f"tests/data/pdf/multipage.pdf (page {page_number})\n")


def test_identical_pdf_pages_keep_their_source(monkeypatch):
    monkeypatch.setattr(PdfToEntries, "extract_text", lambda _: ["Shared text", "Shared text"])

    file_to_text, entries = PdfToEntries.extract_pdf_entries({"first.pdf": b"first", "second.pdf": b"second"})

    assert file_to_text == {"first.pdf": ["Shared text"] * 2, "second.pdf": ["Shared text"] * 2}
    assert [(entry.file, entry.uri) for entry in entries] == [
        ("first.pdf", "file://first.pdf#page=1"),
        ("first.pdf", "file://first.pdf#page=2"),
        ("second.pdf", "file://second.pdf#page=1"),
        ("second.pdf", "file://second.pdf#page=2"),
    ]
    assert len({TextToEntries.hash_func("compiled")(entry) for entry in entries}) == 4


@pytest.mark.parametrize("filename", ["/docs/paper.pdf", "https://example.com/paper.pdf"])
def test_pdf_chunks_keep_the_page_location(monkeypatch, filename):
    monkeypatch.setattr(PdfToEntries, "extract_text", lambda _: [" ".join(f"word{i}" for i in range(80))])
    _, entries = PdfToEntries.extract_pdf_entries({filename: b"pdf"})

    chunks = TextToEntries.split_entries_by_max_tokens(entries, max_tokens=20)

    expected_uri = filename if filename.startswith("https://") else f"file://{filename}"
    assert len(chunks) > 1
    assert all(chunk.uri == f"{expected_uri}#page=1" for chunk in chunks)
    assert all(chunk.heading == f"{filename} (page 1)\n" for chunk in chunks)


@pytest.mark.parametrize("legacy_index", [False, True])
def test_pdf_page_locations_reach_search_results(default_user, monkeypatch, legacy_index):
    model = SearchModelFactory()
    encoder = Mock()
    encoder.embed_documents.side_effect = lambda texts: [[0.1, 0.2, 0.3] for _ in texts]
    monkeypatch.setattr(state, "embeddings_model", {model.name: encoder})
    monkeypatch.setattr(PdfToEntries, "extract_text", lambda _: ["Shared text", "Shared text"])
    if legacy_index:
        compiled = "paper.pdf\nShared text"
        DbEntry.objects.bulk_create(
            [
                DbEntry(
                    user=default_user,
                    embeddings=[0.1, 0.2, 0.3],
                    raw="Shared text",
                    compiled=compiled,
                    file_path="paper.pdf",
                    file_type=DbEntry.EntryType.PDF,
                    url="file://paper.pdf",
                    hashed_value=hashlib.md5(compiled.encode()).hexdigest(),
                    search_model=model,
                )
            ]
        )

    added, deleted = PdfToEntries().process({"paper.pdf": b"pdf"}, default_user)

    assert (added, deleted) == (2, int(legacy_index))
    hits = list(DbEntry.objects.filter(user=default_user).order_by("url"))
    for hit in hits:
        hit.distance = 0.1
    results = list(collate_results(hits))
    assert [result.additional["uri"] for result in results] == [
        "file://paper.pdf#page=1",
        "file://paper.pdf#page=2",
    ]
    assert all(result.additional["file"] == "paper.pdf" for result in results)
    assert all(result.entry == "Shared text" for result in results)

    assert PdfToEntries().process({"paper.pdf": b"pdf"}, default_user) == (0, 0)
    assert DbEntry.objects.filter(user=default_user).count() == 2


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
