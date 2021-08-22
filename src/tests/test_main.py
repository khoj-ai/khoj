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


def test_cli_minimal_default():
    # Act
    actual_args = cli(['--config-file=tests/data/config.yml'])

    # Assert
    assert actual_args.config_file == Path('tests/data/config.yml')
    assert actual_args.regenerate == False
    assert actual_args.verbose == 0

# ----------------------------------------------------------------------------------------------------
def test_cli_flags():
    # Act
    actual_args = cli(['--config-file=tests/data/config.yml',
                       '--regenerate',
                       '-vvv'])

    # Assert
    assert actual_args.config_file == Path('tests/data/config.yml')
    assert actual_args.regenerate == True
    assert actual_args.verbose == 3


# ----------------------------------------------------------------------------------------------------
def test_cli_config_from_file():
    # Act
    actual_args = cli(['--config-file=tests/data/config.yml',
                       '--regenerate',
                       '-vvv'])

    # Assert
    assert actual_args.config_file == Path('tests/data/config.yml')
    assert actual_args.regenerate == True
    assert actual_args.config is not None
    assert actual_args.config['content-type']['org']['input-files'] == ['~/first_from_config.org', '~/second_from_config.org']
    assert actual_args.verbose == 3


# ----------------------------------------------------------------------------------------------------
def test_cli_config_from_cmd_args():
    ""
    # Act
    actual_args = cli(['--org-files=first.org'])

    # Assert
    assert actual_args.org_files == ['first.org']
    assert actual_args.config_file is None
    assert actual_args.config is not None
    assert actual_args.config['content-type']['org']['input-files'] == ['first.org']


# ----------------------------------------------------------------------------------------------------
def test_cli_config_from_cmd_args_override_config_file():
    # Act
    actual_args = cli(['--config-file=tests/data/config.yml',
                       '--org-files=first.org'])

    # Assert
    assert actual_args.org_files == ['first.org']
    assert actual_args.config_file == Path('tests/data/config.yml')
    assert actual_args.config is not None
    assert actual_args.config['content-type']['org']['input-files'] == ['first.org']
