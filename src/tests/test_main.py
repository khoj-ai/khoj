# Standard Modules
from pathlib import Path

# External Packages
import pytest
from fastapi.testclient import TestClient

# Internal Packages
from main import app, cli
from search_type import asymmetric


# Arrange
# ----------------------------------------------------------------------------------------------------
client = TestClient(app)

input_files = [Path('tests/data/main_readme.org'), Path('tests/data/interface_emacs_readme.org')]
input_filter = None
compressed_jsonl = Path('tests/data/.test.jsonl.gz')
embeddings = Path('tests/data/.test_embeddings.pt')
regenerate = False
verbose = 1


# ----------------------------------------------------------------------------------------------------
def test_asymmetric_setup():
    # Act
    entries, corpus_embeddings, bi_encoder, cross_encoder, top_k = asymmetric.setup(input_files, input_filter, compressed_jsonl, embeddings, regenerate, verbose)

    # Assert
    assert len(entries) == 10
    assert len(corpus_embeddings) == 10


# ----------------------------------------------------------------------------------------------------
def test_cli_default():
    # Act
    args = cli(['--input-files=tests/data/test.org'])

    # Assert
    assert args.input_files == ['tests/data/test.org']
    assert args.input_filter == None
    assert args.compressed_jsonl == Path('.notes.jsonl.gz')
    assert args.embeddings == Path('.notes_embeddings.pt')
    assert args.regenerate == False
    assert args.verbose == 0


# ----------------------------------------------------------------------------------------------------
def test_cli_set_by_user():
    # Act
    actual_args = cli(['--input-files=tests/data/test.org',
                       '--input-filter=tests/data/*.org',
                       '--compressed-jsonl=tests/data/.test.jsonl.gz',
                       '--embeddings=tests/data/.test_embeddings.pt',
                       '--regenerate',
                       '-vvv'])

    # Assert
    assert actual_args.input_files == ['tests/data/test.org']
    assert actual_args.input_filter == 'tests/data/*.org'
    assert actual_args.compressed_jsonl == Path('tests/data/.test.jsonl.gz')
    assert actual_args.embeddings == Path('tests/data/.test_embeddings.pt')
    assert actual_args.regenerate == True
    assert actual_args.verbose == 3
