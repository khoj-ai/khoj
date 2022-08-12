# Standard Packages
import signal
import sys

# External Packages
import uvicorn
from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from PyQt6 import QtWidgets
from PyQt6.QtCore import QThread, QTimer

# Internal Packages
from src.configure import configure_server
from src.router import router
from src.utils import constants, state
from src.utils.cli import cli
from src.interface.desktop.configure_screen import ConfigureScreen
from src.interface.desktop.system_tray import create_system_tray


# Initialize the Application Server
app = FastAPI()
app.mount("/static", StaticFiles(directory=constants.web_directory), name="static")
app.include_router(router)


def run():
    # Load config from CLI
    state.cli_args = sys.argv[1:]
    args = cli(state.cli_args)
    set_state(args)

    if args.no_gui:
        # Start Server
        configure_server(args, required=True)
        start_server(app, host=args.host, port=args.port, socket=args.socket)
    else:
        # Setup GUI
        gui = QtWidgets.QApplication([])
        gui.setQuitOnLastWindowClosed(False)
        configure_screen = ConfigureScreen(args.config_file)
        tray = create_system_tray(gui, configure_screen)
        tray.show()

        # Setup Server
        configure_server(args, required=False)
        server = ServerThread(app, args.host, args.port, args.socket)

        # Trigger First Run Experience, if required
        if args.config is None:
            configure_screen.show()

        # Setup Signal Handlers
        signal.signal(signal.SIGINT, sigint_handler)
        # Invoke python Interpreter every 500ms to handle signals
        timer = QTimer()
        timer.start(500)
        timer.timeout.connect(lambda: None)

        # Start Application
        server.start()
        gui.aboutToQuit.connect(server.terminate)
        gui.exec()


def sigint_handler(*args):
    print("\nShutting down Khoj...")
    QtWidgets.QApplication.quit()


def set_state(args):
    state.config_file = args.config_file
    state.config = args.config
    state.verbose = args.verbose


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
