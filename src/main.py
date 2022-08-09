# Standard Packages
import sys
import webbrowser

# External Packages
import uvicorn
from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from PyQt6 import QtGui, QtWidgets
from PyQt6.QtCore import Qt, QThread

# Internal Packages
from src.configure import configure_server
from src.router import router
from src.utils import constants
from src.utils.cli import cli


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


class ConfigureWindow(QtWidgets.QMainWindow):
    """Create Window to Configure Khoj
    Allow user to
    1. Enable/Disable search on 1. org-mode, 2. markdown, 3. beancount or 4. image content types
    2. Configure the server host and port
    3. Save the configuration to khoj.yml and start the server
    """

    def __init__(self):
        super().__init__()

        # Initialize Configure Window
        self.setWindowTitle("Khoj - Configure")
        self.layout = QtWidgets.QVBoxLayout()

        # Org Mode Settings
        orgmode_settings = QtWidgets.QWidget()
        self.orgmode_layout = QtWidgets.QVBoxLayout(orgmode_settings)
        enable_orgmode_search = QtWidgets.QCheckBox(
            "Search Org-Mode Files",
            stateChanged = self.show_orgmode_search_options)
        self.orgmode_layout.addWidget(enable_orgmode_search)
        self.layout.addWidget(orgmode_settings)

        # Ledger Settings
        ledger_settings = QtWidgets.QWidget()
        self.ledger_layout = QtWidgets.QVBoxLayout(ledger_settings)
        enable_ledger_search = QtWidgets.QCheckBox(
            "Search Beancount Files",
            state_changed=self.show_ledger_search_options)
        self.ledger_layout.addWidget(enable_ledger_search)
        self.layout.addWidget(ledger_settings)

        # Button to Save Settings
        action_bar = QtWidgets.QWidget()
        action_bar_layout = QtWidgets.QHBoxLayout(action_bar)
        save_button = QtWidgets.QPushButton("Start", clicked=self.save_settings)
        action_bar_layout.addWidget(save_button)
        self.layout.addWidget(action_bar)

        # Set the central widget of the Window. Widget will expand
        # to take up all the space in the window by default.
        self.config_window = QtWidgets.QWidget()
        self.config_window.setLayout(self.layout)

        self.setCentralWidget(self.config_window)

    def save_settings(self, s):
        # Save the settings to khoj.yml
        pass

    def show_orgmode_search_options(self, s):
        if Qt.CheckState(s) == Qt.CheckState.Checked:
            self.orgmode_layout.layout().addWidget(QtWidgets.QLabel("Search Org-Mode Files"))
            self.orgmode_layout.layout().addWidget(QtWidgets.QLineEdit())
        else:
            self.orgmode_layout.layout().removeWidget(self.orgmode_layout.layout().itemAt(1).widget())
            self.orgmode_layout.layout().removeWidget(self.orgmode_layout.layout().itemAt(1).widget())

    def show_ledger_search_options(self, s):
        if Qt.CheckState(s) == Qt.CheckState.Checked:
            self.ledger_layout.layout().addWidget(QtWidgets.QLabel("Search Ledger Files"))
            self.ledger_layout.layout().addWidget(QtWidgets.QLineEdit())
        else:
            self.ledger_layout.layout().removeWidget(self.ledger_layout.layout().itemAt(1).widget())
            self.ledger_layout.layout().removeWidget(self.ledger_layout.layout().itemAt(1).widget())
  

def create_system_tray(gui: QtWidgets.QApplication, window: QtWidgets.QMainWindow):
    """Create System Tray with Menu
    Menu Actions should contain
    1. option to open search page at localhost:8000/
    2. option to open config page at localhost:8000/config
    3. to quit
    """

    # Create the system tray with icon
    icon_path = constants.web_directory / 'assets/icons/favicon-144x144.png'
    icon = QtGui.QIcon(f'{icon_path.absolute()}')
    tray = QtWidgets.QSystemTrayIcon(icon)
    tray.setVisible(True)

    # Create the menu and menu actions
    menu = QtWidgets.QMenu()
    menu_actions = [
        ('Search', lambda: webbrowser.open('http://localhost:8000/')),
        ('Configure', window.show),
        ('Quit', gui.quit),
    ]

    # Add the menu actions to the menu
    for action_text, action_function in menu_actions:
        menu_action = QtGui.QAction(action_text, menu)
        menu_action.triggered.connect(action_function)
        menu.addAction(menu_action)

    # Add the menu to the system tray
    tray.setContextMenu(menu)

    return tray


if __name__ == '__main__':
    run()
