# Standard Packages
import os
import signal
import sys
import logging
import warnings
from platform import system

# Ignore non-actionable warnings
warnings.filterwarnings("ignore", message=r'snapshot_download.py has been made private', category=FutureWarning)
warnings.filterwarnings("ignore", message=r'legacy way to download files from the HF hub,', category=FutureWarning)

# External Packages
import uvicorn
from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from PyQt6 import QtWidgets
from PyQt6.QtCore import QThread, QTimer

# Internal Packages
from src.configure import configure_server
from src.routers.api import api
from src.routers.api_beta import api_beta
from src.routers.web_client import web_client
from src.utils import constants, state
from src.utils.cli import cli
from src.utils.helpers import CustomFormatter
from src.interface.desktop.main_window import MainWindow
from src.interface.desktop.system_tray import create_system_tray


# Initialize the Application Server
app = FastAPI()
app.mount("/static", StaticFiles(directory=constants.web_directory), name="static")
app.include_router(api, prefix="/api")
app.include_router(api_beta, prefix="/api/beta")
app.include_router(web_client)

logger = logging.getLogger('src')


def run():
    # Turn Tokenizers Parallelism Off. App does not support it.
    os.environ["TOKENIZERS_PARALLELISM"] = 'false'

    # Load config from CLI
    state.cli_args = sys.argv[1:]
    args = cli(state.cli_args)
    set_state(args)

    # Create app directory, if it doesn't exist
    state.config_file.parent.mkdir(parents=True, exist_ok=True)

    # Setup Logger
    if args.verbose == 0:
        logger.setLevel(logging.WARN)
    elif args.verbose == 1:
        logger.setLevel(logging.INFO)
    elif args.verbose >= 2:
        logger.setLevel(logging.DEBUG)

    # Set Log Format
    ch = logging.StreamHandler()
    ch.setFormatter(CustomFormatter())
    logger.addHandler(ch)

    # Set Log File
    fh = logging.FileHandler(state.config_file.parent / 'khoj.log')
    fh.setLevel(logging.DEBUG)
    logger.addHandler(fh)

    logger.info("Starting Khoj...")

    if args.no_gui:
        # Start Server
        configure_server(args, required=True)
        start_server(app, host=args.host, port=args.port, socket=args.socket)
    else:
        # Setup GUI
        gui = QtWidgets.QApplication([])
        main_window = MainWindow(args.config_file)

        # System tray is only available on Windows, MacOS.
        # On Linux (Gnome) the System tray is not supported.
        # Since only the Main Window is available
        # Quitting it should quit the application
        if system() in ['Windows', 'Darwin']:
            gui.setQuitOnLastWindowClosed(False)
            tray = create_system_tray(gui, main_window)
            tray.show()

        # Setup Server
        configure_server(args, required=False)
        server = ServerThread(app, args.host, args.port, args.socket)

        # Show Main Window on First Run Experience or if on Linux
        if args.config is None or system() not in ['Windows', 'Darwin']:
            main_window.show()

        # Setup Signal Handlers
        signal.signal(signal.SIGINT, sigint_handler)
        # Invoke python Interpreter every 500ms to handle signals
        timer = QTimer()
        timer.start(500)
        timer.timeout.connect(lambda: None)

        # Start Application
        server.start()
        gui.aboutToQuit.connect(server.terminate)

        # Close Splash Screen if still open
        if system() != 'Darwin':
            try:
                import pyi_splash
                # Update the text on the splash screen
                pyi_splash.update_text("Khoj setup complete")
                # Close Splash Screen
                pyi_splash.close()
            except:
                pass

        gui.exec()


def sigint_handler(*args):
    print("\nShutting down Khoj...")
    QtWidgets.QApplication.quit()


def set_state(args):
    state.config_file = args.config_file
    state.config = args.config
    state.verbose = args.verbose
    state.host = args.host
    state.port = args.port


def start_server(app, host=None, port=None, socket=None):
    if socket:
        uvicorn.run(app, proxy_headers=True, uds=socket)
    else:
        uvicorn.run(app, host=host, port=port)


class ServerThread(QThread):
    def __init__(self, app, host=None, port=None, socket=None):
        super(ServerThread, self).__init__()
        self.app = app
        self.host = host
        self.port = port
        self.socket = socket

    def __del__(self):
        self.wait()

    def run(self):
        start_server(self.app, self.host, self.port, self.socket)


if __name__ == '__main__':
    run()
