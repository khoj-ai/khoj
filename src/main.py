# Standard Packages
import sys

# External Packages
import uvicorn
from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from PyQt6 import QtWidgets
from PyQt6.QtCore import QThread

# Internal Packages
from src.configure import configure_server
from src.router import router
from src.utils import constants
from src.utils.cli import cli
from src.interface.desktop.configure_window import ConfigureWindow
from src.interface.desktop.system_tray import create_system_tray


# Initialize the Application Server
app = FastAPI()
app.mount("/static", StaticFiles(directory=constants.web_directory), name="static")
app.include_router(router)


def run():
    # Setup Base GUI
    gui = QtWidgets.QApplication([])
    gui.setQuitOnLastWindowClosed(False)
    window = ConfigureWindow()
    tray = create_system_tray(gui, window)
    tray.show()

    # Load config from CLI
    args = cli(sys.argv[1:])

    # Trigger First Run Experience, if required
    if args.config is None:
        window.show()
        gui.exec()

    # Reload config after first run
    args = cli(sys.argv[1:])
    # Quit if app still not configured
    if args.config is None:
        print('Exiting as Khoj is not configured. Configure the application to use it.')
        sys.exit(1)

    # Setup Application Server
    host, port, socket = configure_server(args)

    # Start Application Server
    server = ServerThread(app, host, port, socket)
    server.start()
    gui.aboutToQuit.connect(server.terminate)

    # Start the GUI
    gui.exec()


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
        if self.socket:
            uvicorn.run(app, proxy_headers=True, uds=self.socket)
        else:
            uvicorn.run(app, host=self.host, port=self.port)


if __name__ == '__main__':
    run()
