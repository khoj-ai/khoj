# Standard Modules
from pathlib import Path
from random import random

# Internal Packages
from src.utils.cli import cli
from src.utils.helpers import resolve_absolute_path


# Test
# ----------------------------------------------------------------------------------------------------
def test_cli_minimal_default():
    # Act
    actual_args = cli([])

    # Assert
    assert actual_args.config_file == resolve_absolute_path(Path('~/.khoj/khoj.yml'))
    assert actual_args.regenerate == False
    assert actual_args.no_gui == False
    assert actual_args.verbose == 0

# ----------------------------------------------------------------------------------------------------
def test_cli_invalid_config_file_path():
    # Arrange
    non_existent_config_file = f"non-existent-khoj-{random()}.yml"

    # Act
    actual_args = cli([f'-c={non_existent_config_file}'])

    # Assert
    assert actual_args.config_file == resolve_absolute_path(non_existent_config_file)
    assert actual_args.config == None

# ----------------------------------------------------------------------------------------------------
def test_cli_config_from_file():
    # Act
    actual_args = cli(['-c=tests/data/config.yml',
                       '--regenerate',
                       '--no-gui',
                       '-vvv'])

    # Assert
    assert actual_args.config_file == resolve_absolute_path(Path('tests/data/config.yml'))
    assert actual_args.no_gui == True
    assert actual_args.regenerate == True
    assert actual_args.config is not None
    assert actual_args.config.content_type.org.input_files == [Path('~/first_from_config.org'), Path('~/second_from_config.org')]
    assert actual_args.verbose == 3
