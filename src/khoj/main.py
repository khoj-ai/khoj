""" Main module for Khoj Assistant
   isort:skip_file
"""

# Standard Packages
from contextlib import redirect_stdout
import io
import os
import sys
import locale

import logging
import threading
import warnings
from importlib.metadata import version

# Ignore non-actionable warnings
warnings.filterwarnings("ignore", message=r"snapshot_download.py has been made private", category=FutureWarning)
warnings.filterwarnings("ignore", message=r"legacy way to download files from the HF hub,", category=FutureWarning)

# External Packages
import uvicorn
import django
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from rich.logging import RichHandler
import schedule

from django.core.asgi import get_asgi_application
from django.core.management import call_command

# Initialize Django
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "khoj.app.settings")
django.setup()

# Initialize Django Database
db_migrate_output = io.StringIO()
with redirect_stdout(db_migrate_output):
    call_command("migrate", "--noinput")

# Initialize Django Static Files
collectstatic_output = io.StringIO()
with redirect_stdout(collectstatic_output):
    call_command("collectstatic", "--noinput")

# Initialize the Application Server
app = FastAPI()

# Get Django Application
django_app = get_asgi_application()

# Add CORS middleware
KHOJ_DOMAIN = os.getenv("KHOJ_DOMAIN", "app.khoj.dev")
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "app://obsidian.md",
        "http://localhost:*",
        "http://127.0.0.1:*",
        f"https://{KHOJ_DOMAIN}",
        "app://khoj.dev",
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Set Locale
locale.setlocale(locale.LC_ALL, "")

# Internal Packages. We do this after setting up Django so that Django features are accessible to the app.
from khoj.configure import configure_routes, initialize_server, configure_middleware
from khoj.utils import state
from khoj.utils.cli import cli
from khoj.utils.initialization import initialization

# Setup Logger
rich_handler = RichHandler(rich_tracebacks=True)
rich_handler.setFormatter(fmt=logging.Formatter(fmt="%(message)s", datefmt="[%X]"))
logging.basicConfig(handlers=[rich_handler])

logger = logging.getLogger("khoj")


def run(should_start_server=True):
    # Turn Tokenizers Parallelism Off. App does not support it.
    os.environ["TOKENIZERS_PARALLELISM"] = "false"

    # Load config from CLI
    state.cli_args = sys.argv[1:]
    args = cli(state.cli_args)
    set_state(args)

    # Set Logging Level
    if args.verbose == 0:
        logger.setLevel(logging.INFO)
    elif args.verbose >= 1:
        logger.setLevel(logging.DEBUG)

    logger.info(f"🚒 Initializing Khoj v{state.khoj_version}")
    logger.info(f"📦 Initializing DB:\n{db_migrate_output.getvalue().strip()}")
    logger.debug(f"🌍 Initializing Web Client:\n{collectstatic_output.getvalue().strip()}")

    initialization()

    # Create app directory, if it doesn't exist
    state.config_file.parent.mkdir(parents=True, exist_ok=True)

    # Set Log File
    fh = logging.FileHandler(state.config_file.parent / "khoj.log", encoding="utf-8")
    fh.setLevel(logging.DEBUG)
    logger.addHandler(fh)

    logger.info("🌘 Starting Khoj")

    # Setup task scheduler
    poll_task_scheduler()

    # Start Server
    configure_routes(app)

    #  Mount Django and Static Files
    app.mount("/server", django_app, name="server")
    static_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "static")
    if not os.path.exists(static_dir):
        os.mkdir(static_dir)
    app.mount(f"/static", StaticFiles(directory=static_dir), name=static_dir)

    # Configure Middleware
    configure_middleware(app)

    initialize_server(args.config)

    # If the server is started through gunicorn (external to the script), don't start the server
    if should_start_server:
        start_server(app, host=args.host, port=args.port, socket=args.socket)


def set_state(args):
    state.config_file = args.config_file
    state.config = args.config
    state.verbose = args.verbose
    state.host = args.host
    state.port = args.port
    state.anonymous_mode = args.anonymous_mode
    state.khoj_version = version("khoj-assistant")
    state.chat_on_gpu = args.chat_on_gpu


def start_server(app, host=None, port=None, socket=None):
    logger.info("🌖 Khoj is ready to use")
    if socket:
        uvicorn.run(app, proxy_headers=True, uds=socket, log_level="debug", use_colors=True, log_config=None)
    else:
        uvicorn.run(app, host=host, port=port, log_level="debug", use_colors=True, log_config=None)
    logger.info("🌒 Stopping Khoj")


def poll_task_scheduler():
    timer_thread = threading.Timer(60.0, poll_task_scheduler)
    timer_thread.daemon = True
    timer_thread.start()
    schedule.run_pending()


if __name__ == "__main__":
    run()
else:
    run(should_start_server=False)
