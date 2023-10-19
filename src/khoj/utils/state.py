# Standard Packages
import threading
from typing import List, Dict
from packaging import version
from collections import defaultdict

# External Packages
import torch
from pathlib import Path

# Internal Packages
from khoj.utils import config as utils_config
from khoj.utils.config import ContentIndex, SearchModels, GPT4AllProcessorModel
from khoj.utils.helpers import LRU
from khoj.utils.rawconfig import FullConfig
from khoj.processor.embeddings import EmbeddingsModel, CrossEncoderModel

# Application Global State
config = FullConfig()
search_models = SearchModels()
embeddings_model = EmbeddingsModel()
cross_encoder_model = CrossEncoderModel()
content_index = ContentIndex()
gpt4all_processor_config: GPT4AllProcessorModel = None
config_file: Path = None
verbose: int = 0
host: str = None
port: int = None
cli_args: List[str] = None
query_cache: Dict[str, LRU] = defaultdict(LRU)
config_lock = threading.Lock()
chat_lock = threading.Lock()
SearchType = utils_config.SearchType
telemetry: List[Dict[str, str]] = []
demo: bool = False
khoj_version: str = None
anonymous_mode: bool = False

if torch.cuda.is_available():
    # Use CUDA GPU
    device = torch.device("cuda:0")
elif version.parse(torch.__version__) >= version.parse("1.13.0.dev") and torch.backends.mps.is_available():
    # Use Apple M1 Metal Acceleration
    device = torch.device("mps")
else:
    device = torch.device("cpu")
