# External Packages
import torch
from pathlib import Path

# Internal Packages
from src.utils.config import SearchModels, ProcessorConfigModel
from src.utils.rawconfig import FullConfig

# Application Global State
config = FullConfig()
model = SearchModels()
processor_config = ProcessorConfigModel()
config_file: Path = ""
verbose: int = 0
device = torch.device("cuda:0") if torch.cuda.is_available() else torch.device("cpu") # Set device to GPU if available

# Other Constants
web_directory = Path(__file__).parent.parent / 'interface/web/'
empty_escape_sequences = r'\n|\r\t '
