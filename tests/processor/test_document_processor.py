from pathlib import Path
import pytest
import pypdf
from sqlmodel import Session, SQLModel, create_engine
from khoj.database.models import Document, ProcessingStatus
from khoj.processor.content.document_processor import process_document


@pytest.fixture
def session():
    engine = create_engine("sqlite:///:memory:")
    SQLModel.metadata.create_all(engine)
    with Session(engine) as session:
        yield session


def test_process_document_success(session, tmp_path):
    test_file = tmp_path / "test.txt"
    test_file.write_text("Hello world")
    doc = Document(filename="test.txt", file_path=str(test_file))
    session.add(doc)
    session.commit()
    session.refresh(doc)

    processed = process_document(doc.id, session)
    assert processed.status == ProcessingStatus.COMPLETED
    assert processed.error_message is None


def test_process_document_encrypted_pdf(session, tmp_path):
    encrypted_pdf = tmp_path / "secret.pdf"
    writer = pypdf.PdfWriter()
    writer.add_blank_page(width=100, height=100)
    writer.encrypt("lock123")
    with open(encrypted_pdf, "wb") as f:
        writer.write(f)

    doc = Document(filename="secret.pdf", file_path=str(encrypted_pdf))
    session.add(doc)
    session.commit()
    session.refresh(doc)

    processed = process_document(doc.id, session)
    assert processed.status == ProcessingStatus.FAILED
    assert processed.error_message == "Document is encrypted or password-protected"
