# Standard Modules
from pathlib import Path

from khoj.utils.cli import cli


# Test
# ----------------------------------------------------------------------------------------------------
def test_cli_minimal_default():
    # Act
    actual_args = cli(["-vvv"])

    # Assert
    assert actual_args.log_file == Path("~/.khoj/khoj.log")
    assert actual_args.verbose == 3
