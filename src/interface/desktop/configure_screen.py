# External Packages
from PyQt6 import QtWidgets


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

        # Add Settings Panels for each Search Type to Configure Window Layout
        for search_type in ["Org-Mode", "Markdown", "Beancount", "Image"]:
            self.add_settings_panel(search_type, layout)
        self.add_action_panel(layout)

    def add_settings_panel(self, search_type, parent_layout):
        "Add Settings Panel for specified Search Type. Toggle Editable Search Types"
        orgmode_settings = QtWidgets.QWidget()
        orgmode_layout = QtWidgets.QVBoxLayout(orgmode_settings)

        enable_search_type = QtWidgets.QCheckBox(f"Search {search_type} Notes")
        input_files_label = QtWidgets.QLabel(f"{search_type} Files")
        input_files = QtWidgets.QLineEdit()
        input_files.setEnabled(enable_search_type.isChecked())

        enable_search_type.stateChanged.connect(lambda _: input_files.setEnabled(enable_search_type.isChecked()))

        orgmode_layout.addWidget(enable_search_type)
        orgmode_layout.addWidget(input_files_label)
        orgmode_layout.addWidget(input_files)

        parent_layout.addWidget(orgmode_settings)

    def add_action_panel(self, parent_layout):
        "Add Action Panel"
        # Button to Save Settings
        action_bar = QtWidgets.QWidget()
        action_bar_layout = QtWidgets.QHBoxLayout(action_bar)

        save_button = QtWidgets.QPushButton("Start", clicked=self.save_settings)

        action_bar_layout.addWidget(save_button)
        parent_layout.addWidget(action_bar)

    def save_settings(self, s):
        # Save the settings to khoj.yml
        pass
