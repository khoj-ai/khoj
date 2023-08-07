# External Packages
from PySide6 import QtGui
from PySide6.QtCore import Qt, QThread, QUrl
from PySide6.QtWebEngineWidgets import QWebEngineView
from PySide6.QtWebEngineCore import QWebEnginePage

# Internal Packages
from khoj.utils import constants


class ServerThread(QThread):
    def __init__(self, start_server_func):
        super(ServerThread, self).__init__()
        self.start_server_func = start_server_func

    def __del__(self):
        self.wait()

    def run(self):
        self.start_server_func()


class MainWindow(QWebEngineView):
    """Create Window to Navigate users to the web UI"""

    def __init__(self, url: str):
        super(MainWindow, self).__init__()
        self.base_url = url

        # Initialize Configure Window
        self.setWindowTitle("Khoj")

        # Set Window Icon
        icon_path = constants.web_directory / "assets/icons/favicon-128x128.png"
        self.setWindowIcon(QtGui.QIcon(f"{icon_path.absolute()}"))

        # Open Khoj Web App Root
        self.webpage = QWebEnginePage()
        self.setPage(self.webpage)
        self.webpage.load(QUrl(self.base_url))

        self.position_window()

    def show_page(self, page: str = ""):
        def load_page():
            self.webpage.load(QUrl(f"{self.base_url}/{page}"))
            self.show()

        return load_page

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
