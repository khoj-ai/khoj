# Standard Packages
import webbrowser

# External Packages
from PySide6 import QtGui, QtWidgets
from PySide6.QtCore import Qt

# Internal Packages
from khoj.utils import constants
from PySide6.QtCore import QThread


class ServerThread(QThread):
    def __init__(self, start_server_func):
        super(ServerThread, self).__init__()
        self.start_server_func = start_server_func

    def __del__(self):
        self.wait()

    def run(self):
        self.start_server_func()


class MainWindow(QtWidgets.QMainWindow):
    """Create Window to Navigate users to the web UI"""

    def __init__(self, host: str, port: int):
        super(MainWindow, self).__init__()

        # Initialize Configure Window
        self.setWindowTitle("Khoj")

        # Set Window Icon
        icon_path = constants.web_directory / "assets/icons/favicon-128x128.png"
        self.setWindowIcon(QtGui.QIcon(f"{icon_path.absolute()}"))

        # Initialize Configure Window Layout
        self.wlayout = QtWidgets.QVBoxLayout()

        # Add a Label that says "Khoj Configuration" to the Window
        self.wlayout.addWidget(QtWidgets.QLabel("Welcome to Khoj"))

        # Add a Button to open the Web UI at http://host:port/config
        self.open_web_ui_button = QtWidgets.QPushButton("Open Web UI")
        self.open_web_ui_button.clicked.connect(lambda: webbrowser.open(f"http://{host}:{port}/config"))

        self.wlayout.addWidget(self.open_web_ui_button)

        # Set the central widget of the Window. Widget will expand
        # to take up all the space in the window by default.
        self.config_window = QtWidgets.QWidget()
        self.config_window.setLayout(self.wlayout)
        self.setCentralWidget(self.config_window)
        self.position_window()

    def position_window(self):
        "Position the window at center of X axis and near top on Y axis"
        window_rectangle = self.geometry()
        screen_center = self.screen().availableGeometry().center()
        window_rectangle.moveCenter(screen_center)
        self.move(window_rectangle.topLeft().x(), 25)

    def show_on_top(self):
        "Bring Window on Top"
        self.show()
        self.setWindowState(Qt.WindowState.WindowActive)
        self.activateWindow()  # For Bringing to Top on Windows
        self.raise_()  # For Bringing to Top from Minimized State on OSX
