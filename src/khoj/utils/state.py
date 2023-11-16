# Standard Packages
import os
import threading
from typing import List, Dict
from collections import defaultdict

# External Packages
from pathlib import Path
from khoj.processor.embeddings import CrossEncoderModel, EmbeddingsModel

# Internal Packages
from khoj.utils import config as utils_config
from khoj.utils.config import ContentIndex, SearchModels, GPT4AllProcessorModel
from khoj.utils.helpers import LRU, get_device
from khoj.utils.rawconfig import FullConfig

# Application Global State
config = FullConfig()
search_models = SearchModels()
embeddings_model: EmbeddingsModel = None
cross_encoder_model: CrossEncoderModel = None
content_index = ContentIndex()
gpt4all_processor_config: GPT4AllProcessorModel = None
config_file: Path = None
verbose: int = 0
host: str = None
port: int = None
cli_args: List[str] = None
query_cache: Dict[str, LRU] = defaultdict(LRU)
chat_lock = threading.Lock()
SearchType = utils_config.SearchType
telemetry: List[Dict[str, str]] = []
khoj_version: str = None
device = get_device()
chat_on_gpu: bool = True
anonymous_mode: bool = False
billing_enabled: bool = (
    os.getenv("STRIPE_API_KEY") is not None
    and os.getenv("STRIPE_SIGNING_SECRET") is not None
    and os.getenv("KHOJ_CLOUD_SUBSCRIPTION_URL") is not None
)
