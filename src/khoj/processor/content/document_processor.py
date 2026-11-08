import logging
from pathlib import Path
from datetime import datetime
from sqlmodel import Session, select
from khoj.database.models import Document, ProcessingStatus, Entry
from khoj.processor.content.text_to_entries import file_to_entries
from khoj.processor.content.pdf.text_extractor import EncryptedDocumentError
from pypdf.errors import FileNotDecryptedError

logger = logging.getLogger(__name__)


def process_document(document_id: int, session: Session) -> Document:
    """Process an uploaded document and extract its entries."""
    statement = select(Document).where(Document.id == document_id)
    doc = session.exec(statement).first()
    if not doc:
        raise ValueError(f"Document {document_id} not found")

    doc.status = ProcessingStatus.PROCESSING
    doc.updated_at = datetime.utcnow()
    session.add(doc)
    session.commit()
    session.refresh(doc)

    try:
        file_path = Path(doc.file_path)
        entries = file_to_entries(file_path)
        doc.status = ProcessingStatus.COMPLETED
        doc.updated_at = datetime.utcnow()
        session.add(doc)
        session.commit()
        session.refresh(doc)
        return doc
    except (EncryptedDocumentError, FileNotDecryptedError) as e:
        logger.warning(f"Document {document_id} is encrypted or password-protected: {e}")
        doc.status = ProcessingStatus.FAILED
        doc.error_message = "Document is encrypted or password-protected"
        doc.updated_at = datetime.utcnow()
        session.add(doc)
        session.commit()
        session.refresh(doc)
        return doc
    except Exception as e:
        logger.error(f"Error processing document {document_id}: {e}")
        doc.status = ProcessingStatus.FAILED
        doc.error_message = str(e)
        doc.updated_at = datetime.utcnow()
        session.add(doc)
        session.commit()
        session.refresh(doc)
        return doc
