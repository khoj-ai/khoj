# Standard Packages
import webbrowser

# External Packages
from PyQt6 import QtGui, QtWidgets

# Internal Packages
from src.utils import constants, state
from src.interface.desktop.main_window import MainWindow


def create_system_tray(gui: QtWidgets.QApplication, main_window: MainWindow):
    """Create System Tray with Menu.  Menu contain options to
    1. Open Search Page on the Web Interface
    2. Open App Configuration Screen
    3. Quit Application
    """

    # Create the system tray with icon
    icon_path = constants.web_directory / 'assets/icons/favicon-144x144.png'
    icon = QtGui.QIcon(f'{icon_path.absolute()}')
    tray = QtWidgets.QSystemTrayIcon(icon)
    tray.setVisible(True)

    # Create the menu and menu actions
    menu = QtWidgets.QMenu()
    menu_actions = [
        ('Search', lambda: webbrowser.open(f'http://{state.host}:{state.port}/')),
        ('Configure', main_window.show_on_top),
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
