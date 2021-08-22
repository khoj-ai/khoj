# Standard Modules
from pathlib import Path

# External Packages
import pytest
from fastapi.testclient import TestClient

# Internal Packages
from main import app
from search_type import asymmetric


# Arrange
# ----------------------------------------------------------------------------------------------------
client = TestClient(app)


# Test
# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup():
    # Arrange
    input_files = [Path('tests/data/main_readme.org'), Path('tests/data/interface_emacs_readme.org')]
    input_filter = None
    compressed_jsonl = Path('tests/data/.test.jsonl.gz')
    embeddings = Path('tests/data/.test_embeddings.pt')
    regenerate = False
    verbose = 1

    # Act
    entries, corpus_embeddings, bi_encoder, cross_encoder, top_k = asymmetric.setup(input_files, input_filter, compressed_jsonl, embeddings, regenerate, verbose)

    # Assert
    assert len(entries) == 10
    assert len(corpus_embeddings) == 10
