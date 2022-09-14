# Standard Packages
from packaging import version

# External Packages
import torch
from pathlib import Path

# Internal Packages
from src.utils.config import SearchModels, ProcessorConfigModel
from src.utils.helpers import LRU
from src.utils.rawconfig import FullConfig

# Application Global State
config = FullConfig()
model = SearchModels()
processor_config = ProcessorConfigModel()
config_file: Path = None
verbose: int = 0
host: str = None
port: int = None
cli_args: list[str] = None
query_cache = LRU()

if torch.cuda.is_available():
    # Use CUDA GPU
    device = torch.device("cuda:0")
elif version.parse(torch.__version__) >= version.parse("1.13.0.dev") and torch.backends.mps.is_available():
    # Use Apple M1 Metal Acceleration
    device = torch.device("mps")
else:
    device = torch.device("cpu")
