# Standard Packages
from __future__ import annotations  # to avoid quoting type hints
from collections import OrderedDict
import datetime
from enum import Enum
from importlib import import_module
from importlib.metadata import version
from itertools import islice
import logging
from os import path
import os
from pathlib import Path
import platform
import random
from time import perf_counter
import torch
from typing import Optional, Union, TYPE_CHECKING
import uuid
from asgiref.sync import sync_to_async

# Internal Packages
from khoj.utils import constants


if TYPE_CHECKING:
    # External Packages
    from sentence_transformers import SentenceTransformer, CrossEncoder

    # Internal Packages
    from khoj.utils.models import BaseEncoder
    from khoj.utils.rawconfig import AppConfig


class AsyncIteratorWrapper:
    def __init__(self, obj):
        self._it = iter(obj)

    def __aiter__(self):
        return self

    async def __anext__(self):
        try:
            value = await self.next_async()
        except StopAsyncIteration:
            return
        return value

    @sync_to_async
    def next_async(self):
        try:
            return next(self._it)
        except StopIteration:
            raise StopAsyncIteration


def is_none_or_empty(item):
    return item == None or (hasattr(item, "__iter__") and len(item) == 0) or item == ""


def to_snake_case_from_dash(item: str):
    return item.replace("_", "-")


def get_absolute_path(filepath: Union[str, Path]) -> str:
    return str(Path(filepath).expanduser().absolute())


def resolve_absolute_path(filepath: Union[str, Optional[Path]], strict=False) -> Path:
    return Path(filepath).expanduser().absolute().resolve(strict=strict)


def get_from_dict(dictionary, *args):
    """null-aware get from a nested dictionary
    Returns: dictionary[args[0]][args[1]]... or None if any keys missing"""
    current = dictionary
    for arg in args:
        if not hasattr(current, "__iter__") or not arg in current:
            return None
        current = current[arg]
    return current


def merge_dicts(priority_dict: dict, default_dict: dict):
    merged_dict = priority_dict.copy()
    for key, _ in default_dict.items():
        if key not in priority_dict:
            merged_dict[key] = default_dict[key]
        elif isinstance(priority_dict[key], dict) and isinstance(default_dict[key], dict):
            merged_dict[key] = merge_dicts(priority_dict[key], default_dict[key])
    return merged_dict


def get_file_type(file_type: str) -> tuple[str, str]:
    "Get file type from file mime type"

    encoding = file_type.split("=")[1].strip().lower() if ";" in file_type else None
    file_type = file_type.split(";")[0].strip() if ";" in file_type else file_type
    if file_type in ["text/markdown"]:
        return "markdown", encoding
    elif file_type in ["text/org"]:
        return "org", encoding
    elif file_type in ["application/pdf"]:
        return "pdf", encoding
    elif file_type in ["image/jpeg"]:
        return "jpeg", encoding
    elif file_type in ["image/png"]:
        return "png", encoding
    elif file_type in ["text/plain", "text/html", "application/xml", "text/x-rst"]:
        return "plaintext", encoding
    else:
        return "other", encoding


def load_model(
    model_name: str, model_type, model_dir=None, device: str = None
) -> Union[BaseEncoder, SentenceTransformer, CrossEncoder]:
    "Load model from disk or huggingface"
    # Construct model path
    logger = logging.getLogger(__name__)
    model_path = path.join(model_dir, model_name.replace("/", "_")) if model_dir is not None else None

    # Load model from model_path if it exists there
    model_type_class = get_class_by_name(model_type) if isinstance(model_type, str) else model_type
    if model_path is not None and resolve_absolute_path(model_path).exists():
        logger.debug(f"Loading {model_name} model from disk")
        model = model_type_class(get_absolute_path(model_path), device=device)
    # Else load the model from the model_name
    else:
        logger.info(f"🤖 Downloading {model_name} model from web")
        model = model_type_class(model_name, device=device)
        if model_path is not None:
            logger.info(f"📩 Saved {model_name} model to disk")
            model.save(model_path)

    return model


def get_class_by_name(name: str) -> object:
    "Returns the class object from name string"
    module_name, class_name = name.rsplit(".", 1)
    return getattr(import_module(module_name), class_name)


class timer:
    """Context manager to log time taken for a block of code to run"""

    def __init__(self, message: str, logger: logging.Logger, device: torch.device = None):
        self.message = message
        self.logger = logger
        self.device = device

    def __enter__(self):
        self.start = perf_counter()
        return self

    def __exit__(self, *_):
        elapsed = perf_counter() - self.start
        if self.device is None:
            self.logger.debug(f"{self.message}: {elapsed:.3f} seconds")
        else:
            self.logger.debug(f"{self.message}: {elapsed:.3f} seconds on device: {self.device}")


class LRU(OrderedDict):
    def __init__(self, *args, capacity=128, **kwargs):
        self.capacity = capacity
        super().__init__(*args, **kwargs)

    def __getitem__(self, key):
        value = super().__getitem__(key)
        self.move_to_end(key)
        return value

    def __setitem__(self, key, value):
        super().__setitem__(key, value)
        if len(self) > self.capacity:
            oldest = next(iter(self))
            del self[oldest]


def get_server_id():
    """Get, Generate Persistent, Random ID per server install.
    Helps count distinct khoj servers deployed.
    Maintains anonymity by using non-PII random id."""
    # Initialize server_id to None
    server_id = None
    # Expand path to the khoj env file. It contains persistent internal app data
    app_env_filename = path.expanduser(constants.app_env_filepath)

    # Check if the file exists
    if path.exists(app_env_filename):
        # Read the contents of the file
        with open(app_env_filename, "r") as f:
            contents = f.readlines()

        # Extract the server_id from the contents
        for line in contents:
            key, value = line.strip().split("=")
            if key.strip() == "server_id":
                server_id = value.strip()
                break

        # If server_id is not found, generate and write to env file
        if server_id is None:
            # If server_id is not found, generate a new one
            server_id = str(uuid.uuid4())

            with open(app_env_filename, "a") as f:
                f.write("server_id=" + server_id + "\n")
    else:
        # If server_id is not found, generate a new one
        server_id = str(uuid.uuid4())

        # Create khoj config directory if it doesn't exist
        os.makedirs(path.dirname(app_env_filename), exist_ok=True)

        # Write the server_id to the env file
        with open(app_env_filename, "w") as f:
            f.write("server_id=" + server_id + "\n")

    return server_id


def log_telemetry(
    telemetry_type: str,
    api: str = None,
    client: Optional[str] = None,
    app_config: Optional[AppConfig] = None,
    properties: dict = None,
):
    """Log basic app usage telemetry like client, os, api called"""
    # Do not log usage telemetry, if telemetry is disabled via app config
    if not app_config or not app_config.should_log_telemetry:
        return []

    if properties.get("server_id") is None:
        properties["server_id"] = get_server_id()

    # Populate telemetry data to log
    request_body = {
        "telemetry_type": telemetry_type,
        "server_version": version("khoj-assistant"),
        "os": platform.system(),
        "timestamp": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
    }
    request_body.update(properties or {})
    if api:
        # API endpoint on server called by client
        request_body["api"] = api
    if client:
        # Client from which the API was called. E.g Emacs, Obsidian
        request_body["client"] = client

    # Log telemetry data to telemetry endpoint
    return request_body


def get_device() -> torch.device:
    """Get device to run model on"""
    if torch.cuda.is_available():
        # Use CUDA GPU
        return torch.device("cuda:0")
    elif torch.backends.mps.is_available():
        # Use Apple M1 Metal Acceleration
        return torch.device("mps")
    else:
        return torch.device("cpu")


class ConversationCommand(str, Enum):
    Default = "default"
    General = "general"
    Notes = "notes"
    Help = "help"


command_descriptions = {
    ConversationCommand.General: "Only talk about information that relies on Khoj's general knowledge, not your personal knowledge base.",
    ConversationCommand.Notes: "Only talk about information that is available in your knowledge base.",
    ConversationCommand.Default: "The default command when no command specified. It intelligently auto-switches between general and notes mode.",
    ConversationCommand.Help: "Display a help message with all available commands and other metadata.",
}


def generate_random_name():
    # List of adjectives and nouns to choose from
    adjectives = [
        "happy",
        "serendipitous",
        "exuberant",
        "calm",
        "brave",
        "scared",
        "energetic",
        "chivalrous",
        "kind",
        "suave",
    ]
    nouns = ["dog", "cat", "falcon", "whale", "turtle", "rabbit", "hamster", "snake", "spider", "elephant"]

    # Select two random words from the lists
    adjective = random.choice(adjectives)
    noun = random.choice(nouns)

    # Combine the words to form a name
    name = f"{adjective} {noun}"

    return name


def batcher(iterable, max_n):
    "Split an iterable into chunks of size max_n"
    it = iter(iterable)
    while True:
        chunk = list(islice(it, max_n))
        if not chunk:
            return
        yield (x for x in chunk if x is not None)
