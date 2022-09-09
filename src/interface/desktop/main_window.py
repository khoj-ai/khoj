# Standard Packages
from enum import Enum
from pathlib import Path
from copy import deepcopy
import webbrowser

# External Packages
from PyQt6 import QtGui, QtWidgets
from PyQt6.QtCore import Qt, QThread, QObject, pyqtSignal

# Internal Packages
from src.configure import configure_server
from src.interface.desktop.file_browser import FileBrowser
from src.interface.desktop.labelled_text_field import LabelledTextField
from src.utils import constants, state, yaml as yaml_utils
from src.utils.cli import cli
from src.utils.config import SearchType, ProcessorType
from src.utils.helpers import merge_dicts, resolve_absolute_path


class MainWindow(QtWidgets.QMainWindow):
    """Create Window to Configure Khoj
    Allow user to
    1. Configure content types to search
    2. Configure conversation processor
    3. Save the configuration to khoj.yml
    """

    def __init__(self, config_file: Path):
        super(MainWindow, self).__init__()
        self.config_file = config_file
        # Set regenerate flag to regenerate embeddings everytime user clicks configure
        if state.cli_args:
            state.cli_args += ['--regenerate']
        else:
            state.cli_args = ['--regenerate']

        # Load config from existing config, if exists, else load from default config
        if resolve_absolute_path(self.config_file).exists():
            self.first_run = False
            self.current_config = yaml_utils.load_config_from_file(self.config_file)
        else:
            self.first_run = True
            self.current_config = deepcopy(constants.default_config)
        self.new_config = self.current_config

        # Initialize Configure Window
        self.setWindowTitle("Khoj")
        self.setFixedWidth(600)

        # Set Window Icon
        icon_path = constants.web_directory / 'assets/icons/favicon-144x144.png'
        self.setWindowIcon(QtGui.QIcon(f'{icon_path.absolute()}'))

        # Initialize Configure Window Layout
        self.layout = QtWidgets.QVBoxLayout()

        # Add Settings Panels for each Search Type to Configure Window Layout
        self.search_settings_panels = []
        for search_type in SearchType:
            current_content_config = self.current_config['content-type'].get(search_type, {})
            self.search_settings_panels += [self.add_settings_panel(current_content_config, search_type)]

        # Add Conversation Processor Panel to Configure Screen
        self.processor_settings_panels = []
        conversation_type = ProcessorType.Conversation
        current_conversation_config = self.current_config['processor'].get(conversation_type, {})
        self.processor_settings_panels += [self.add_processor_panel(current_conversation_config, conversation_type)]

        # Add Action Buttons Panel
        self.add_action_panel()

        # Set the central widget of the Window. Widget will expand
        # to take up all the space in the window by default.
        self.config_window = QtWidgets.QWidget()
        self.config_window.setLayout(self.layout)
        self.setCentralWidget(self.config_window)
        self.position_window()

    def add_settings_panel(self, current_content_config: dict, search_type: SearchType):
        "Add Settings Panel for specified Search Type. Toggle Editable Search Types"
        # Get current files from config for given search type
        if search_type == SearchType.Image:
            current_content_files = current_content_config.get('input-directories', [])
            file_input_text = f'{search_type.name} Folders'
        else:
            current_content_files = current_content_config.get('input-files', [])
            file_input_text = f'{search_type.name} Files'

        # Create widgets to display settings for given search type
        search_type_settings = QtWidgets.QWidget()
        search_type_layout = QtWidgets.QVBoxLayout(search_type_settings)
        enable_search_type = SearchCheckBox(f"Search {search_type.name}", search_type)
        # Add file browser to set input files for given search type
        input_files = FileBrowser(file_input_text, search_type, current_content_files or [])

        # Set enabled/disabled based on checkbox state
        enable_search_type.setChecked(current_content_files is not None and len(current_content_files) > 0)
        input_files.setEnabled(enable_search_type.isChecked())
        enable_search_type.stateChanged.connect(lambda _: input_files.setEnabled(enable_search_type.isChecked()))

        # Add setting widgets for given search type to panel
        search_type_layout.addWidget(enable_search_type)
        search_type_layout.addWidget(input_files)
        self.layout.addWidget(search_type_settings)

        return search_type_settings

    def add_processor_panel(self, current_conversation_config: dict, processor_type: ProcessorType):
        "Add Conversation Processor Panel"
        # Get current settings from config for given processor type
        current_openai_api_key = current_conversation_config.get('openai-api-key', None)

        # Create widgets to display settings for given processor type
        processor_type_settings = QtWidgets.QWidget()
        processor_type_layout = QtWidgets.QVBoxLayout(processor_type_settings)
        enable_conversation = ProcessorCheckBox(f"Conversation", processor_type)
        # Add file browser to set input files for given processor type
        input_field = LabelledTextField("OpenAI API Key", processor_type, current_openai_api_key)

        # Set enabled/disabled based on checkbox state
        enable_conversation.setChecked(current_openai_api_key is not None)
        input_field.setEnabled(enable_conversation.isChecked())
        enable_conversation.stateChanged.connect(lambda _: input_field.setEnabled(enable_conversation.isChecked()))

        # Add setting widgets for given processor type to panel
        processor_type_layout.addWidget(enable_conversation)
        processor_type_layout.addWidget(input_field)
        self.layout.addWidget(processor_type_settings)

        return processor_type_settings

    def add_action_panel(self):
        "Add Action Panel"
        # Button to Save Settings
        action_bar = QtWidgets.QWidget()
        action_bar_layout = QtWidgets.QHBoxLayout(action_bar)

        self.configure_button = QtWidgets.QPushButton("Configure", clicked=self.configure_app)
        self.search_button = QtWidgets.QPushButton("Search", clicked=lambda: webbrowser.open(f'http://{state.host}:{state.port}/'))
        self.search_button.setEnabled(not self.first_run)

        action_bar_layout.addWidget(self.configure_button)
        action_bar_layout.addWidget(self.search_button)
        self.layout.addWidget(action_bar)

    def get_default_config(self, search_type:SearchType=None, processor_type:ProcessorType=None):
        "Get default config"
        config = constants.default_config
        if search_type:
            return config['content-type'][search_type]
        elif processor_type:
            return config['processor'][processor_type]
        else:
            return config

    def add_error_message(self, message: str):
        "Add Error Message to Configure Screen"
        # Remove any existing error messages
        for message_prefix in ErrorType:
            for i in reversed(range(self.layout.count())):
                current_widget = self.layout.itemAt(i).widget()
                if isinstance(current_widget, QtWidgets.QLabel) and current_widget.text().startswith(message_prefix.value):
                    self.layout.removeWidget(current_widget)
                    current_widget.deleteLater()

        # Add new error message
        if message:
            error_message = QtWidgets.QLabel()
            error_message.setWordWrap(True)
            error_message.setText(message)
            error_message.setStyleSheet("color: red")
            self.layout.addWidget(error_message)

    def update_search_settings(self):
        "Update config with search settings from UI"
        for settings_panel in self.search_settings_panels:
            for child in settings_panel.children():
                if not isinstance(child, (SearchCheckBox, FileBrowser)):
                    continue
                if isinstance(child, SearchCheckBox):
                    # Search Type Disabled
                    if not child.isChecked() and child.search_type in self.new_config['content-type']:
                        del self.new_config['content-type'][child.search_type]
                    # Search Type (re)-Enabled
                    if child.isChecked():
                        current_search_config = self.current_config['content-type'].get(child.search_type, {})
                        default_search_config = self.get_default_config(search_type = child.search_type)
                        self.new_config['content-type'][child.search_type.value] = merge_dicts(current_search_config, default_search_config)
                elif isinstance(child, FileBrowser) and child.search_type in self.new_config['content-type']:
                    if child.search_type.value == SearchType.Image:
                        self.new_config['content-type'][child.search_type.value]['input-directories'] = child.getPaths() if child.getPaths() != [] else None
                    else:
                        self.new_config['content-type'][child.search_type.value]['input-files'] = child.getPaths() if child.getPaths() != [] else None

    def update_processor_settings(self):
        "Update config with conversation settings from UI"
        for settings_panel in self.processor_settings_panels:
            for child in settings_panel.children():
                if not isinstance(child, (ProcessorCheckBox, LabelledTextField)):
                    continue
                if isinstance(child, ProcessorCheckBox):
                    # Processor Type Disabled
                    if not child.isChecked() and child.processor_type in self.new_config['processor']:
                        del self.new_config['processor'][child.processor_type]
                    # Processor Type (re)-Enabled
                    if child.isChecked():
                        current_processor_config = self.current_config['processor'].get(child.processor_type, {})
                        default_processor_config = self.get_default_config(processor_type = child.processor_type)
                        self.new_config['processor'][child.processor_type.value] = merge_dicts(current_processor_config, default_processor_config)
                elif isinstance(child, LabelledTextField) and child.processor_type in self.new_config['processor']:
                    if child.processor_type == ProcessorType.Conversation:
                        self.new_config['processor'][child.processor_type.value]['openai-api-key'] = child.input_field.toPlainText() if child.input_field.toPlainText() != '' else None

    def save_settings_to_file(self) -> bool:
        "Save validated settings to file"
        # Validate config before writing to file
        try:
            yaml_utils.parse_config_from_string(self.new_config)
        except Exception as e:
            print(f"Error validating config: {e}")
            self.add_error_message(f"{ErrorType.ConfigValidationError.value}: {e}")
            return False

        # Save the config to app config file
        self.add_error_message(None)
        yaml_utils.save_config_to_file(self.new_config, self.config_file)
        return True

    def load_updated_settings(self):
        "Hot swap to use the updated config from config file"
        # Load parsed, validated config from app config file
        args = cli(state.cli_args)
        self.current_config = self.new_config

        # Configure server with loaded config
        configure_server(args, required=True)

    def configure_app(self):
        "Save the new settings to khoj.yml. Reload app with updated settings"
        self.update_search_settings()
        self.update_processor_settings()
        if self.save_settings_to_file():
            # Setup thread to load updated settings in background
            self.thread = QThread()
            self.settings_loader = SettingsLoader(self.load_updated_settings)
            self.settings_loader.moveToThread(self.thread)

            # Connect slots and signals for thread
            self.thread.started.connect(self.settings_loader.run)
            self.settings_loader.finished.connect(self.thread.quit)
            self.settings_loader.finished.connect(self.settings_loader.deleteLater)
            self.settings_loader.error.connect(self.add_error_message)
            self.thread.finished.connect(self.thread.deleteLater)

            # Start thread
            self.thread.start()

            # Disable Save Button
            self.search_button.setEnabled(False)
            self.configure_button.setEnabled(False)
            self.configure_button.setText("Configuring...")

            # Reset UI
            self.thread.finished.connect(lambda: self.configure_button.setText("Configure"))
            self.thread.finished.connect(lambda: self.configure_button.setEnabled(True))
            self.thread.finished.connect(lambda: self.search_button.setEnabled(True))

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
        self.raise_()          # For Bringing to Top from Minimized State on OSX


class SettingsLoader(QObject):
    "Load Settings Thread"
    finished = pyqtSignal()
    error = pyqtSignal(str)

    def __init__(self, load_settings_func):
        super(SettingsLoader, self).__init__()
        self.load_settings_func = load_settings_func

    def run(self):
        "Load Settings"
        try:
            self.load_settings_func()
        except FileNotFoundError as e:
            self.error.emit(f"{ErrorType.ConfigLoadingError.value}: {e}")
        else:
            self.error.emit(None)
        self.finished.emit()


class SearchCheckBox(QtWidgets.QCheckBox):
    def __init__(self, text, search_type: SearchType, parent=None):
        self.search_type = search_type
        super(SearchCheckBox, self).__init__(text, parent=parent)


class ProcessorCheckBox(QtWidgets.QCheckBox):
    def __init__(self, text, processor_type: ProcessorType, parent=None):
        self.processor_type = processor_type
        super(ProcessorCheckBox, self).__init__(text, parent=parent)

class ErrorType(Enum):
    "Error Types"
    ConfigLoadingError = "Config Loading Error"
    ConfigValidationError = "Config Validation Error"
