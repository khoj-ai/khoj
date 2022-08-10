# External Packages
from PyQt6 import QtWidgets
from PyQt6.QtCore import Qt

# Internal Packages
from src.configure import configure_server
from src.utils import constants, state, yaml as yaml_utils
from src.utils.cli import cli
from src.utils.config import SearchType
from src.interface.desktop.file_browser import FileBrowser


class ConfigureScreen(QtWidgets.QDialog):
    """Create Window to Configure Khoj
    Allow user to
    1. Enable/Disable search on 1. org-mode, 2. markdown, 3. beancount or 4. image content types
    2. Configure the server host and port
    3. Save the configuration to khoj.yml and start the server
    """

    def __init__(self, config_file, parent=None):
        super(ConfigureScreen, self).__init__(parent=parent)
        self.config_file = config_file

        # Initialize Configure Window
        self.setWindowFlags(Qt.WindowType.WindowStaysOnTopHint)
        self.setWindowTitle("Khoj - Configure")

        # Initialize Configure Window Layout
        layout = QtWidgets.QVBoxLayout()
        self.setLayout(layout)

        # Add Settings Panels for each Search Type to Configure Window Layout
        self.settings_panels = []
        for search_type in SearchType:
            self.settings_panels += [self.add_settings_panel(search_type, layout)]
        self.add_action_panel(layout)

    def add_settings_panel(self, search_type: SearchType, parent_layout: QtWidgets.QLayout):
        "Add Settings Panel for specified Search Type. Toggle Editable Search Types"
        search_type_settings = QtWidgets.QWidget()
        search_type_layout = QtWidgets.QVBoxLayout(search_type_settings)

        enable_search_type = CheckBox(f"Search {search_type.name}", search_type)
        input_files = FileBrowser(f'{search_type.name} Files', search_type)
        input_files.setEnabled(enable_search_type.isChecked())

        enable_search_type.stateChanged.connect(lambda _: input_files.setEnabled(enable_search_type.isChecked()))

        search_type_layout.addWidget(enable_search_type)
        search_type_layout.addWidget(input_files)

        parent_layout.addWidget(search_type_settings)
        return search_type_settings

    def add_action_panel(self, parent_layout: QtWidgets.QLayout):
        "Add Action Panel"
        # Button to Save Settings
        action_bar = QtWidgets.QWidget()
        action_bar_layout = QtWidgets.QHBoxLayout(action_bar)

        save_button = QtWidgets.QPushButton("Start", clicked=self.save_settings)

        action_bar_layout.addWidget(save_button)
        parent_layout.addWidget(action_bar)

    def save_settings(self, _):
        "Save the settings to khoj.yml"
        # Load the default config
        config = yaml_utils.load_config_from_file(constants.app_root_directory / 'config/khoj_sample.yml')

        # Update the default config with the settings from the UI
        for settings_panel in self.settings_panels:
            for child in settings_panel.children():
                if isinstance(child, CheckBox) and not child.isChecked():
                        del config['content-type'][child.search_type]
                elif isinstance(child, FileBrowser) and child.search_type in config['content-type']:
                    config['content-type'][child.search_type]['input-files'] = child.getPaths()
                    print(f"{child.search_type} files are {child.getPaths()}")

        # Save the config to app config file
        del config['processor']['conversation']
        yaml_utils.save_config_to_file(config, self.config_file)

        # Load config from app config file
        args = cli(state.cli_args)

        # Configure server with loaded config
        configure_server(args, required=True)

        self.hide()


class CheckBox(QtWidgets.QCheckBox):
    def __init__(self, text, search_type: SearchType, parent=None):
        self.search_type = search_type
        super(CheckBox, self).__init__(text, parent=parent)