# External Packages
import pytest

# Internal Packages
from khoj.utils.rawconfig import TextContentConfig, ImageContentConfig


# Test
# ----------------------------------------------------------------------------------------------------
def test_input_file_or_filter_required_in_text_content_config():
    # Act
    with pytest.raises(ValueError):
        TextContentConfig(
            input_files=None,
            input_filter=None,
            compressed_jsonl="notes.jsonl",
            embeddings_file="note_embeddings.pt",
        )


# ----------------------------------------------------------------------------------------------------
def test_input_filter_or_directories_required_in_image_content_config():
    # Act
    with pytest.raises(ValueError):
        ImageContentConfig(
            input_directories=None,
            input_filter=None,
            embeddings_file="note_embeddings.pt",
        )
