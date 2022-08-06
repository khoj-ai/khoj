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


# Initialize the Application Server
app = FastAPI()
app.mount("/static", StaticFiles(directory=constants.web_directory), name="static")
app.include_router(router)


def run():
    # Setup Application Server
    host, port, socket = configure_server(sys.argv[1:])

    # Setup GUI
    gui = QtWidgets.QApplication([])
    gui.setQuitOnLastWindowClosed(False)
    tray = create_system_tray()
    window = ConfigureWindow()

    # Start Application Server
    server = ServerThread(app, host, port, socket)
    server.start()
    gui.aboutToQuit.connect(server.terminate)

    # Start the GUI
    window.show()
    tray.show()
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


# Subclass QMainWindow to customize your application's main window
class ConfigureWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("Khoj - Configure")

        self.layout = QtWidgets.QVBoxLayout()

        enable_orgmode_search = QtWidgets.QCheckBox("Enable Search on Org-Mode Files")
        enable_orgmode_search.stateChanged.connect(self.show_orgmode_search_options) 
        self.layout.addWidget(enable_orgmode_search)

        enable_ledger_search = QtWidgets.QCheckBox("Enable Search on Beancount Files")
        enable_ledger_search.stateChanged.connect(self.show_ledger_search_options) 
        self.layout.addWidget(enable_ledger_search)

        # Set the central widget of the Window. Widget will expand
        # to take up all the space in the window by default.
        # Create Widget for Setting Directory with Org-Mode Files
        self.config_window = QtWidgets.QWidget()
        self.config_window.setLayout(self.layout)

        self.setCentralWidget(self.config_window)

    def show_orgmode_search_options(self, s):
        if Qt.CheckState(s) == Qt.CheckState.Checked:
            self.config_window.layout().addWidget(QtWidgets.QLabel("Search Org-Mode Files"))
            self.config_window.layout().addWidget(QtWidgets.QLineEdit())
        else:
            self.config_window.layout().removeWidget(self.config_window.layout().itemAt(2).widget())
            self.config_window.layout().removeWidget(self.config_window.layout().itemAt(2).widget())

    def show_ledger_search_options(self, s):
        if Qt.CheckState(s) == Qt.CheckState.Checked:
            self.config_window.layout().addWidget(QtWidgets.QLabel("Search Ledger Files"))
            self.config_window.layout().addWidget(QtWidgets.QLineEdit())
        else:
            self.config_window.layout().removeWidget(self.config_window.layout().itemAt(2).widget())
            self.config_window.layout().removeWidget(self.config_window.layout().itemAt(2).widget())
  

def create_system_tray():
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
        ('Configure', lambda: webbrowser.open('http://localhost:8000/config')),
        ('Quit', quit),
    ]

    # Add the menu actions to the menu
    for action_text, action_function in menu_actions:
        menu_action = QtGui.QAction(action_text, menu)
        menu_action.triggered.connect(action_function)
        menu.addAction(menu_action)

    # Add the menu to the system tray
    tray.setContextMenu(menu)

    return tray

def create_window_to_configure_khoj():
    """Create Window to Configure Khoj
    Allow user to 
    1. Enable/Disable search on 1. org-mode, 2. markdown, 3. beancount or 4. image content types
    2. Configure the server host and port
    3. Save the configuration to khoj.yml and start the server
    """



if __name__ == '__main__':
    run()