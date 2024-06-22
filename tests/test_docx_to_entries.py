from khoj.processor.content.docx.docx_to_entries import DocxToEntries


def test_single_page_docx_to_jsonl():
    "Convert single page DOCX file to jsonl."
    # Act
    # Extract Entries from specified Docx files
    # Read singlepage.docx into memory as bytes
    with open("tests/data/docx/iceland.docx", "rb") as f:
        docx_bytes = f.read()

    data = {"tests/data/docx/iceland.docx": docx_bytes}
    entries = DocxToEntries.extract_docx_entries(docx_files=data)

    # Assert
    assert "The Icelandic horse" in entries[0]["tests/data/docx/iceland.docx"][0]
    assert len(entries) == 2
    assert len(entries[1]) == 1


def test_multi_page_docx_to_jsonl():
    "Convert multi page DOCX file to jsonl."
    # Act
    # Extract Entries from specified Docx files
    # Read multipage.docx into memory as bytes
    with open("tests/data/docx/bangalore.docx", "rb") as f:
        docx_bytes = f.read()

    data = {"tests/data/docx/bangalore.docx": docx_bytes}
    entries = DocxToEntries.extract_docx_entries(docx_files=data)

    # Assert
    assert "Bangalore" in entries[0]["tests/data/docx/bangalore.docx"][0]
    assert len(entries) == 2
    assert len(entries[1]) == 1
