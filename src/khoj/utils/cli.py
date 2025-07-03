import argparse
import logging
import pathlib
from importlib.metadata import version

logger = logging.getLogger(__name__)


def cli(args=None):
    # Setup Argument Parser for the Commandline Interface
    parser = argparse.ArgumentParser(description="Start Khoj; An AI personal assistant for your Digital Brain")
    parser.add_argument(
        "--log-file",
        default="~/.khoj/khoj.log",
        type=pathlib.Path,
        help="File path for server logs. Default: ~/.khoj/khoj.log",
    )
    parser.add_argument("--verbose", "-v", action="count", default=0, help="Show verbose conversion logs. Default: 0")
    parser.add_argument("--host", type=str, default="127.0.0.1", help="Host address of the server. Default: 127.0.0.1")
    parser.add_argument("--port", "-p", type=int, default=42110, help="Port of the server. Default: 42110")
    parser.add_argument(
        "--socket",
        type=pathlib.Path,
        help="Path to UNIX socket for server. Use to run server behind reverse proxy. Default: /tmp/uvicorn.sock",
    )
    parser.add_argument("--sslcert", type=str, help="Path to SSL certificate file")
    parser.add_argument("--sslkey", type=str, help="Path to SSL key file")
    parser.add_argument("--version", "-V", action="store_true", help="Print the installed Khoj version and exit")
    parser.add_argument(
        "--anonymous-mode",
        action="store_true",
        default=False,
        help="Run Khoj in single user mode with no login required. Useful for personal use or testing.",
    )
    parser.add_argument(
        "--non-interactive",
        action="store_true",
        default=False,
        help="Start Khoj in non-interactive mode. Assumes interactive shell unavailable for config. E.g when run via Docker.",
    )

    args, remaining_args = parser.parse_known_args(args)

    if len(remaining_args) > 0:
        logger.info(f"⚠️  Ignoring unknown commandline args: {remaining_args}")

    args.version_no = version("khoj")
    if args.version:
        # Show version of khoj installed and exit
        print(args.version_no)
        exit(0)

    return args
