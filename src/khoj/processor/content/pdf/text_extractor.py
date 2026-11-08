import logging
import os
from pathlib import Path
from typing import List, Optional

import pypdf
from pypdf.errors import EmptyFileError, PdfReadError, FileNotDecryptedError

logger = logging.getLogger(__name__)


class EncryptedDocumentError(Exception):
    """Raised when a document cannot be read because it is password protected or encrypted."""
    pass


def extract_text_from_pdf(pdf_path: Path) -> Optional[str]:
    """Extract text from a PDF file."""
    try:
        reader = pypdf.PdfReader(str(pdf_path))
        if reader.is_encrypted:
            try:
                decrypt_result = reader.decrypt("")
                if decrypt_result == 0:
                    raise EncryptedDocumentError("Document is encrypted or password-protected")
            except Exception as e:
                if isinstance(e, EncryptedDocumentError):
                    raise
                raise EncryptedDocumentError("Document is encrypted or password-protected") from e
        text = ""
        for page in reader.pages:
            page_text = page.extract_text()
            if page_text:
                text += page_text + "\n"
        return text.strip()
    except EmptyFileError:
        logger.warning(f"PDF file is empty: {pdf_path}")
        return None
    except FileNotDecryptedError as e:
        logger.warning(f"PDF file is password protected: {pdf_path}")
        raise EncryptedDocumentError("Document is encrypted or password-protected") from e
    except PdfReadError as e:
        logger.error(f"Error reading PDF {pdf_path}: {e}")
        raise
    except Exception as e:
        logger.error(f"Unexpected error reading PDF {pdf_path}: {e}")
        raise
