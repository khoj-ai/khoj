# External Packages
from PyQt6 import QtWidgets
from PyQt6.QtCore import Qt


class ConfigureScreen(QtWidgets.QDialog):
    """Create Window to Configure Khoj
    Allow user to
    1. Enable/Disable search on 1. org-mode, 2. markdown, 3. beancount or 4. image content types
    2. Configure the server host and port
    3. Save the configuration to khoj.yml and start the server
    """

    def __init__(self, parent=None):
        super(ConfigureScreen, self).__init__(parent=parent)

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
        orgmode_layout = QtWidgets.QVBoxLayout(orgmode_settings)

        enable_orgmode_search = QtWidgets.QCheckBox("Search Org-Mode Notes")
        input_files_label = QtWidgets.QLabel("Org-Mode Files")
        input_files = QtWidgets.QLineEdit()
        input_files.setEnabled(enable_orgmode_search.isChecked())

        enable_orgmode_search.stateChanged.connect(lambda _: input_files.setEnabled(enable_orgmode_search.isChecked()))

        orgmode_layout.addWidget(enable_orgmode_search)
        orgmode_layout.addWidget(input_files_label)
        orgmode_layout.addWidget(input_files)

        parent_layout.addWidget(orgmode_settings)

    def show_ledger_settings(self, parent_layout):
        "Add Ledger Settings to the Configure Window"
        ledger_settings = QtWidgets.QWidget()
        ledger_layout = QtWidgets.QVBoxLayout(ledger_settings)

        enable_ledger_search = QtWidgets.QCheckBox("Search Beancount Transactions")
        input_files_label = QtWidgets.QLabel("Beancount Files")
        input_files = QtWidgets.QLineEdit()
        input_files.setEnabled(enable_ledger_search.isChecked())

        enable_ledger_search.stateChanged.connect(lambda _: input_files.setEnabled(enable_ledger_search.isChecked()))

        ledger_layout.addWidget(enable_ledger_search)
        ledger_layout.addWidget(input_files_label)
        ledger_layout.addWidget(input_files)

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
