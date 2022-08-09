# External Packages
from PyQt6 import QtWidgets
from PyQt6.QtCore import Qt


class ConfigureWindow(QtWidgets.QDialog):
    """Create Window to Configure Khoj
    Allow user to
    1. Enable/Disable search on 1. org-mode, 2. markdown, 3. beancount or 4. image content types
    2. Configure the server host and port
    3. Save the configuration to khoj.yml and start the server
    """

    def __init__(self, parent=None):
        super(ConfigureWindow, self).__init__(parent=parent)

        # Initialize Configure Window
        self.setWindowTitle("Khoj - Configure")

        # Initialize Configure Window Layout
        layout = QtWidgets.QVBoxLayout()
        self.setLayout(layout)

        # Add Panels to Configure Window Layout
        self.show_orgmode_settings(layout)
        self.show_ledger_settings(layout)
        self.show_action_bar(layout)

    def show_orgmode_settings(self, parent_layout):
        "Add Org Mode Settings to the Configure Window"
        orgmode_settings = QtWidgets.QWidget()
        self.orgmode_layout = QtWidgets.QVBoxLayout(orgmode_settings)
        enable_orgmode_search = QtWidgets.QCheckBox("Search Org-Mode Files")
        enable_orgmode_search.stateChanged.connect(self.show_orgmode_search_options)
        self.orgmode_layout.addWidget(enable_orgmode_search)
        parent_layout.addWidget(orgmode_settings)

    def show_ledger_settings(self, parent_layout):
        "Add Ledger Settings to the Configure Window"
        ledger_settings = QtWidgets.QWidget()
        self.ledger_layout = QtWidgets.QVBoxLayout(ledger_settings)
        enable_ledger_search = QtWidgets.QCheckBox("Search Beancount Files")
        enable_ledger_search.stateChanged.connect(self.show_ledger_search_options)
        self.ledger_layout.addWidget(enable_ledger_search)
        parent_layout.addWidget(ledger_settings)

    def show_action_bar(self, parent_layout):
        "Add Action Bar to the Configure Window"
        # Button to Save Settings
        action_bar = QtWidgets.QWidget()
        action_bar_layout = QtWidgets.QHBoxLayout(action_bar)
        save_button = QtWidgets.QPushButton("Start", clicked=self.save_settings)
        action_bar_layout.addWidget(save_button)
        parent_layout.addWidget(action_bar)

    def save_settings(self, s):
        # Save the settings to khoj.yml
        pass

    def show_orgmode_search_options(self, s):
        if Qt.CheckState(s) == Qt.CheckState.Checked:
            self.orgmode_layout.layout().addWidget(QtWidgets.QLabel("Search Org-Mode Notes"))
            self.orgmode_layout.layout().addWidget(QtWidgets.QLineEdit())
        else:
            self.orgmode_layout.layout().removeWidget(self.orgmode_layout.layout().itemAt(1).widget())
            self.orgmode_layout.layout().removeWidget(self.orgmode_layout.layout().itemAt(1).widget())

    def show_ledger_search_options(self, s):
        if Qt.CheckState(s) == Qt.CheckState.Checked:
            self.ledger_layout.layout().addWidget(QtWidgets.QLabel("Search Beancount Transactions"))
            self.ledger_layout.layout().addWidget(QtWidgets.QLineEdit())
        else:
            self.ledger_layout.layout().removeWidget(self.ledger_layout.layout().itemAt(1).widget())
            self.ledger_layout.layout().removeWidget(self.ledger_layout.layout().itemAt(1).widget())
  