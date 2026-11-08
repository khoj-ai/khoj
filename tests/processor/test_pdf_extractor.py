from pathlib import Path
import pytest
import pypdf
from khoj.processor.content.pdf.text_extractor import extract_text_from_pdf, EncryptedDocumentError


def test_extract_text_empty_pdf(tmp_path: Path):
    empty_pdf = tmp_path / "empty.pdf"
    empty_pdf.touch()
    assert extract_text_from_pdf(empty_pdf) is None


def test_extract_text_encrypted_pdf(tmp_path: Path):
    encrypted_pdf = tmp_path / "encrypted.pdf"
    writer = pypdf.PdfWriter()
    writer.add_blank_page(width=100, height=100)
    writer.encrypt("super_secret_password")
    with open(encrypted_pdf, "wb") as f:
        writer.write(f)

    with pytest.raises(EncryptedDocumentError) as exc_info:
        extract_text_from_pdf(encrypted_pdf)
    assert "encrypted or password-protected" in str(exc_info.value)
