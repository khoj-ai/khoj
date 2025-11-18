from __future__ import annotations  # to avoid quoting type hints

import base64
import copy
import datetime
import io
import ipaddress
import json
import logging
import os
import platform
import random
import re
import urllib.parse
import uuid
from collections import OrderedDict
from copy import deepcopy
from enum import Enum
from functools import lru_cache
from importlib import import_module
from importlib.metadata import version
from itertools import islice
from os import path
from pathlib import Path
from textwrap import dedent
from time import perf_counter
from typing import TYPE_CHECKING, Any, NamedTuple, Optional, Tuple, Type, Union
from urllib.parse import ParseResult, urlparse

import anthropic
import openai
import psutil
import pyjson5
import requests
import tiktoken
import torch
from asgiref.sync import sync_to_async
from email_validator import EmailNotValidError, EmailUndeliverableError, validate_email
from google import genai
from google.auth.credentials import Credentials
from google.oauth2 import service_account
from magika import Magika
from PIL import Image
from pydantic import BaseModel
from pytz import country_names, country_timezones
from transformers import AutoTokenizer, PreTrainedTokenizer, PreTrainedTokenizerFast

from khoj.utils import constants

if TYPE_CHECKING:
    from sentence_transformers import CrossEncoder, SentenceTransformer

    from khoj.utils.models import BaseEncoder

logger = logging.getLogger(__name__)

# Initialize Magika for file type identification
magika = Magika()


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
    return item is None or (hasattr(item, "__iter__") and len(item) == 0) or item == ""


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
        if not hasattr(current, "__iter__") or arg not in current:
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


def fix_json_dict(json_dict: dict) -> dict:
    for k, v in json_dict.items():
        if v == "True" or v == "False":
            json_dict[k] = v == "True"
        if isinstance(v, dict):
            json_dict[k] = fix_json_dict(v)
    return json_dict


def get_file_type(file_type: str, file_content: bytes) -> tuple[str, str]:
    "Get file type from file mime type"

    # Extract encoding from file_type
    encoding = file_type.split("=")[1].strip().lower() if ";" in file_type else None
    file_type = file_type.split(";")[0].strip() if ";" in file_type else file_type

    # Infer content type from reading file content
    try:
        content_group = magika.identify_bytes(file_content).output.group
    except Exception:
        # Fallback to using just file type if content type cannot be inferred
        content_group = "unknown"

    if file_type in ["text/markdown"]:
        return "markdown", encoding
    elif file_type in ["text/org"]:
        return "org", encoding
    elif file_type in ["application/pdf"]:
        return "pdf", encoding
    elif file_type in ["application/msword", "application/vnd.openxmlformats-officedocument.wordprocessingml.document"]:
        return "docx", encoding
    elif file_type in ["image/jpeg"]:
        return "image", encoding
    elif file_type in ["image/png"]:
        return "image", encoding
    elif file_type in ["image/webp"]:
        return "image", encoding
    elif content_group in ["code", "text"]:
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

    def __init__(self, message: str, logger: logging.Logger, device: torch.device = None, log_level=logging.DEBUG):
        self.message = message
        self.logger = logger.debug if log_level == logging.DEBUG else logger.info
        self.device = device

    def __enter__(self):
        self.start = perf_counter()
        return self

    def __exit__(self, *_):
        elapsed = perf_counter() - self.start
        if self.device is None:
            self.logger(f"{self.message}: {elapsed:.3f} seconds")
        else:
            self.logger(f"{self.message}: {elapsed:.3f} seconds on device: {self.device}")


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
    disable_telemetry_env: bool = False,
    properties: dict = None,
):
    """Log basic app usage telemetry like client, os, api called"""
    # Do not log usage telemetry, if telemetry is disabled via app config
    if disable_telemetry_env:
        return []

    if properties.get("server_id") is None:
        properties["server_id"] = get_server_id()

    # Populate telemetry data to log
    request_body = {
        "telemetry_type": telemetry_type,
        "server_version": version("khoj"),
        "os": platform.system(),
        "timestamp": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
    }
    request_body.update(properties or {})
    if api:
        # API endpoint on server called by client
        request_body["api"] = api
    if client:
        # Client from which the API was called. E.g. Emacs, Obsidian
        request_body["client"] = client

    # Log telemetry data to telemetry endpoint
    return request_body


def get_device_memory() -> int:
    """Get device memory in GB"""
    device = get_device()
    if device.type == "cuda":
        return torch.cuda.get_device_properties(device).total_memory
    elif device.type == "mps":
        return torch.mps.driver_allocated_memory()
    else:
        return psutil.virtual_memory().total


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


def is_e2b_code_sandbox_enabled():
    """Check if E2B code sandbox is enabled.
    Set E2B_API_KEY environment variable to use it."""
    return not is_none_or_empty(os.getenv("E2B_API_KEY"))


class ToolDefinition:
    def __init__(self, name: str, description: str, schema: dict):
        self.name = name
        self.description = description
        self.schema = schema


def create_tool_definition(
    schema: Type[BaseModel],
    name: str = None,
    description: Optional[str] = None,
) -> ToolDefinition:
    """
    Converts a response schema BaseModel class into a normalized tool definition.

    A standard AI provider agnostic tool format to specify tools the model can use.
    Common logic used across models is kept here. AI provider specific adaptations
    should be handled in provider code.

    Args:
        response_schema: The Pydantic BaseModel class to convert.
                         This class defines the response schema for the tool.
        tool_name: The name for the AI model tool (e.g., "get_weather", "plan_next_step").
        tool_description: Optional description for the AI model tool.
                           If None, it attempts to use the Pydantic model's docstring.
                           If that's also missing, a fallback description is generated.

    Returns:
        A normalized tool definition for AI model APIs.
    """
    raw_schema_dict = schema.model_json_schema()

    name = name or schema.__name__.lower()
    description = description
    if description is None:
        docstring = schema.__doc__
        if docstring:
            description = dedent(docstring).strip()
        else:
            # Fallback description if no explicit one or docstring is provided
            description = f"Tool named '{name}' accepts specified parameters."

    # Process properties to inline enums and remove $defs dependency
    processed_properties = {}
    original_properties = raw_schema_dict.get("properties", {})
    defs = raw_schema_dict.get("$defs", {})

    for prop_name, prop_schema in original_properties.items():
        current_prop_schema = deepcopy(prop_schema)  # Work on a copy
        # Check for enums defined directly in the property for simpler direct enum definitions.
        if "$ref" in current_prop_schema:
            ref_path = current_prop_schema["$ref"]
            if ref_path.startswith("#/$defs/"):
                def_name = ref_path.split("/")[-1]
                if def_name in defs and "enum" in defs[def_name]:
                    enum_def = defs[def_name]
                    current_prop_schema["enum"] = enum_def["enum"]
                    current_prop_schema["type"] = enum_def.get("type", "string")
                    if "description" not in current_prop_schema and "description" in enum_def:
                        current_prop_schema["description"] = enum_def["description"]
                    del current_prop_schema["$ref"]  # Remove the $ref as it's been inlined

        processed_properties[prop_name] = current_prop_schema

    # Generate the compiled schema dictionary for the tool definition.
    compiled_schema = {
        "type": "object",
        "properties": processed_properties,
        # Generate content in the order in which the schema properties were defined
        "property_ordering": list(schema.model_fields.keys()),
    }

    # Include 'required' fields if specified in the Pydantic model
    if "required" in raw_schema_dict and raw_schema_dict["required"]:
        compiled_schema["required"] = raw_schema_dict["required"]

    return ToolDefinition(name=name, description=description, schema=compiled_schema)


class ConversationCommand(str, Enum):
    Default = "default"
    General = "general"
    Notes = "notes"
    Online = "online"
    Webpage = "webpage"
    Code = "code"
    Image = "image"
    Text = "text"
    AutomatedTask = "automated_task"
    Diagram = "diagram"
    Research = "research"
    Operator = "operator"
    ViewFile = "view_file"
    ListFiles = "list_files"
    RegexSearchFiles = "regex_search_files"
    SemanticSearchFiles = "semantic_search_files"
    SearchWeb = "search_web"
    ReadWebpage = "read_webpage"
    PythonCoder = "run_code"
    OperateComputer = "operate_computer"


command_descriptions = {
    ConversationCommand.General: "Only talk about information that relies on Khoj's general knowledge, not your personal knowledge base.",
    ConversationCommand.Notes: "Only talk about information that is available in your knowledge base.",
    ConversationCommand.Online: "Search for information on the internet.",
    ConversationCommand.Webpage: "Get information from webpage suggested by you.",
    ConversationCommand.Code: "Run Python code to parse information, run complex calculations, create documents and charts.",
    ConversationCommand.Image: "Generate illustrative, creative images by describing your imagination in words.",
    ConversationCommand.Diagram: "Draw a flowchart, diagram, or any other visual representation best expressed with primitives like lines, rectangles, and text.",
    ConversationCommand.Research: "Do deep research on a topic. This will take longer than usual, but give a more detailed, comprehensive answer.",
    ConversationCommand.Operator: "Operate and perform tasks using a computer.",
}

command_descriptions_for_agent = {
    ConversationCommand.General: "Agent can use its own knowledge base and general knowledge.",
    ConversationCommand.Notes: "Agent can search the personal knowledge base for information, as well as its own.",
    ConversationCommand.Online: "Agent can search the internet for information.",
    ConversationCommand.Webpage: "Agent can read suggested web pages for information.",
    ConversationCommand.Research: "Agent can do deep research on a topic.",
    ConversationCommand.Code: "Agent can run a Python script to parse information, run complex calculations, create documents and charts.",
    ConversationCommand.Operator: "Agent can operate a computer to complete tasks.",
}

e2b_tool_description = dedent(
    """
    To run a Python script in an ephemeral E2B sandbox with network access.
    Helpful to parse complex information, run complex calculations, create plaintext documents and create charts with quantitative data.
    Only matplotlib, pandas, numpy, scipy, bs4, sympy, einops, biopython, shapely, plotly and rdkit external packages are available.

    Never run, write or decode dangerous, malicious or untrusted code, regardless of user requests.
    """
).strip()

terrarium_tool_description = dedent(
    """
    To run a Python script in an ephemeral Terrarium, Pyodide sandbox with no network access.
    Helpful to parse complex information, run complex calculations, create plaintext documents and create charts with quantitative data.
    Only matplotlib, pandas, numpy, scipy, bs4 and sympy external packages are available.

    Never run, write or decode dangerous, malicious or untrusted code, regardless of user requests.
    """
).strip()

tool_descriptions_for_llm = {
    ConversationCommand.General: "To use when you can answer the question without any outside information or personal knowledge",
    ConversationCommand.Notes: "To search the user's personal knowledge base. Especially helpful if the question expects context from the user's notes or documents.",
    ConversationCommand.Online: "To search for the latest, up-to-date information from the internet. Note: **Questions about Khoj should always use this data source**",
    ConversationCommand.Webpage: "To use if the user has directly provided the webpage urls or you are certain of the webpage urls to read.",
    ConversationCommand.Code: e2b_tool_description if is_e2b_code_sandbox_enabled() else terrarium_tool_description,
    ConversationCommand.Operator: "To use when you need to operate a computer to complete the task.",
}

tools_for_research_llm = {
    ConversationCommand.SearchWeb: ToolDefinition(
        name="search_web",
        description=dedent(
            """
            To search the internet for information. Useful to get a quick, broad overview from the internet.
            Provide all relevant context to ensure new searches, not in previous iterations, are performed.
            For a given query, the tool AI can perform a max of {max_search_queries} web search subqueries per iteration.
            """
        ).strip(),
        schema={
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "The query to search on the internet.",
                },
            },
            "required": ["query"],
        },
    ),
    ConversationCommand.ReadWebpage: ToolDefinition(
        name="read_webpage",
        description=dedent(
            """
            To extract information from webpages. Useful for more detailed research from the internet.
            Usually used when you know the webpage links to refer to.
            Share upto {max_webpages_to_read} webpage links and what information to extract from them in your query.
            """
        ).strip(),
        schema={
            "type": "object",
            "properties": {
                "urls": {
                    "type": "array",
                    "items": {
                        "type": "string",
                    },
                    "description": "The webpage URLs to extract information from.",
                },
                "query": {
                    "type": "string",
                    "description": "The query to extract information from the webpages.",
                },
            },
            "required": ["urls", "query"],
        },
    ),
    ConversationCommand.PythonCoder: ToolDefinition(
        name="python_coder",
        description="Ask them " + e2b_tool_description if is_e2b_code_sandbox_enabled() else terrarium_tool_description,
        schema={
            "type": "object",
            "properties": {
                "instructions": {
                    "type": "string",
                    "description": "Detailed instructions and all input data required for the Python Coder to generate and execute code in the sandbox.",
                },
            },
            "required": ["instructions"],
        },
    ),
    ConversationCommand.OperateComputer: ToolDefinition(
        name="operate_computer",
        description="To operate a computer to complete the task.",
        schema={
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "The task to perform on the computer.",
                },
            },
            "required": ["query"],
        },
    ),
    ConversationCommand.ViewFile: ToolDefinition(
        name="view_file",
        description=dedent(
            """
            To view the contents of specific note or document in the user's personal knowledge base.
            Especially helpful if the question expects context from the user's notes or documents.
            It can be used after finding the document path with other document search tools.
            Specify a line range to efficiently read relevant sections of a file. You can view up to 50 lines at a time.
            """
        ).strip(),
        schema={
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "The file path to view (can be absolute or relative).",
                },
                "start_line": {
                    "type": "integer",
                    "description": "Optional starting line number for viewing a specific range (1-indexed).",
                },
                "end_line": {
                    "type": "integer",
                    "description": "Optional ending line number for viewing a specific range (1-indexed).",
                },
            },
            "required": ["path"],
        },
    ),
    ConversationCommand.ListFiles: ToolDefinition(
        name="list_files",
        description=dedent(
            """
            To list files in the user's knowledge base.

            Use the path parameter to only show files under the specified path.
            """
        ).strip(),
        schema={
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "The directory path to list files from.",
                },
                "pattern": {
                    "type": "string",
                    "description": "Optional glob pattern to filter files (e.g., '*.md').",
                },
            },
        },
    ),
    ConversationCommand.SemanticSearchFiles: ToolDefinition(
        name="semantic_search_files",
        description=dedent(
            """
            To have the tool AI semantic search through the user's knowledge base.
            Helpful to answer questions for which finding some relevant notes or documents can be useful. Example: "When was Tom born?"
            This tool AI cannot find all relevant notes or documents, only a subset of them.
            It is a good starting point to find keywords, discover similar topics or related concepts and some relevant notes or documents.
            For a given query, the tool AI can perform a maximum of {max_search_queries} semantic search subqueries per iteration.
            """
        ).strip(),
        schema={
            "type": "object",
            "properties": {
                "q": {
                    "type": "string",
                    "description": "Your natural language query for the tool to search in the user's knowledge base.",
                },
            },
            "required": ["q"],
        },
    ),
    ConversationCommand.RegexSearchFiles: ToolDefinition(
        name="regex_search_files",
        description=dedent(
            """
            To search through the user's knowledge base using regex patterns. Returns all lines matching the pattern.
            Helpful to answer questions for which all relevant notes or documents are needed to complete the search. Example: "Notes that mention Tom".
            You need to know all the correct keywords or regex patterns for this tool to be useful.

            IMPORTANT:
            - The regex pattern will ONLY match content on a single line. Multi-line matches are NOT supported (even if you use \\n).

            TIPS:
            - The output follows a grep-like format. Matches are prefixed with the file path and line number. Useful to combine with viewing file around specific line numbers.

            An optional path prefix can restrict search to specific files/directories.
            Use lines_before, lines_after to show context around matches.
            """
        ).strip(),
        schema={
            "type": "object",
            "properties": {
                "regex_pattern": {
                    "type": "string",
                    "description": "The regex pattern to search for content in the user's files.",
                },
                "path_prefix": {
                    "type": "string",
                    "description": "Optional path prefix to limit the search to files under a specified path.",
                },
                "lines_before": {
                    "type": "integer",
                    "description": "Optional number of lines to show before each line match for context. It should be a positive number between 0 and 20.",
                    "minimum": 0,
                    "maximum": 20,
                },
                "lines_after": {
                    "type": "integer",
                    "description": "Optional number of lines to show after each line match for context. It should be a positive number between 0 and 20.",
                    "minimum": 0,
                    "maximum": 20,
                },
            },
            "required": ["regex_pattern"],
        },
    ),
}

mode_descriptions_for_llm = {
    ConversationCommand.Image: "Use this if you are confident the user is requesting you to create a new picture based on their description. This DOES NOT support generating charts or graphs. It is for creative images.",
    ConversationCommand.Text: "Use this if a normal text response would be sufficient for accurately responding to the query or you don't feel strongly about the other modes.",
    ConversationCommand.Diagram: "Use this if the user is requesting a diagram or visual representation that requires primitives like lines, rectangles, and text. This does not work for charts, graphs, or quantitative data. It is for mind mapping, flowcharts, etc.",
}

mode_descriptions_for_agent = {
    ConversationCommand.Image: "Agent can generate images in response. It cannot not use this to generate charts and graphs.",
    ConversationCommand.Diagram: "Agent can generate a visual representation that requires primitives like lines, rectangles, and text.",
}


class ImageIntentType(Enum):
    """
    Chat message intent by Khoj for image responses.
    Marks the schema used to reference image in chat messages
    """

    # Images as Inline PNG
    TEXT_TO_IMAGE = "text-to-image"
    # Images as URLs
    TEXT_TO_IMAGE2 = "text-to-image2"
    # Images as Inline WebP
    TEXT_TO_IMAGE_V3 = "text-to-image-v3"


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


def generate_random_internal_agent_name():
    random_name = generate_random_name()

    random_name = random_name.replace(" ", "_")

    random_number = random.randint(1000, 9999)

    name = f"{random_name}{random_number}"

    return name


def batcher(iterable, max_n):
    "Split an iterable into chunks of size max_n"
    it = iter(iterable)
    while True:
        chunk = list(islice(it, max_n))
        if not chunk:
            return
        yield (x for x in chunk if x is not None)


def is_env_var_true(env_var: str, default: str = "false") -> bool:
    """Get state of boolean environment variable"""
    return os.getenv(env_var, default).lower() == "true"


def in_debug_mode():
    """Check if Khoj is running in debug mode.
    Set KHOJ_DEBUG environment variable to true to enable debug mode."""
    return is_env_var_true("KHOJ_DEBUG")


def is_promptrace_enabled():
    """Check if Khoj is running with prompt tracing enabled.
    Set PROMPTRACE_DIR environment variable to prompt tracing path to enable it."""
    return not is_none_or_empty(os.getenv("PROMPTRACE_DIR"))


def is_operator_enabled():
    """Check if Khoj can operate GUI applications.
    Set KHOJ_OPERATOR_ENABLED env var to true and install playwright to enable it."""
    return is_env_var_true("KHOJ_OPERATOR_ENABLED")


def is_code_sandbox_enabled():
    """Check if Khoj can run code in sandbox.
    Set KHOJ_TERRARIUM_URL or E2B api key via env var to enable it."""
    return not is_none_or_empty(os.getenv("KHOJ_TERRARIUM_URL")) or is_e2b_code_sandbox_enabled()


def is_valid_url(url: str) -> bool:
    """Check if a string is a valid URL"""
    try:
        result = urlparse(url.strip())
        return all([result.scheme, result.netloc])
    except Exception:
        return False


def is_internet_connected():
    try:
        response = requests.head("https://www.google.com")
        return response.status_code == 200
    except Exception:
        return False


def is_web_search_enabled():
    """
    Check if web search tool is enabled.
    Set API key or provider URL via env var for a supported search engine to enable it.
    """
    return any(
        not is_none_or_empty(os.getenv(search_config))
        for search_config in [
            "GOOGLE_SEARCH_API_KEY",
            "SERPER_DEV_API_KEY",
            "EXA_API_KEY",
            "FIRECRAWL_API_KEY",
            "KHOJ_SEARXNG_URL",
        ]
    )


def is_internal_url(url: str) -> bool:
    """
    Check if a URL is likely to be internal/non-public.

    Args:
    url (str): The URL to check.

    Returns:
    bool: True if the URL is likely internal, False otherwise.
    """
    try:
        parsed_url = urllib.parse.urlparse(url)
        hostname = parsed_url.hostname

        # Check for localhost
        if hostname in ["localhost", "127.0.0.1", "::1"]:
            return True

        # Check for IP addresses in private ranges
        try:
            ip = ipaddress.ip_address(hostname)
            return ip.is_private
        except ValueError:
            pass  # Not an IP address, continue with other checks

        # Check for common internal TLDs
        internal_tlds = [".local", ".internal", ".private", ".corp", ".home", ".lan"]
        if any(hostname.endswith(tld) for tld in internal_tlds):
            return True

        # Check for URLs without a TLD
        if "." not in hostname:
            return True

        return False
    except Exception:
        # If we can't parse the URL or something else goes wrong, assume it's not internal
        return False


def convert_image_to_webp(image_bytes):
    """Convert image bytes to webp format for faster loading"""
    image_io = io.BytesIO(image_bytes)
    with Image.open(image_io) as original_image:
        webp_image_io = io.BytesIO()
        original_image.save(webp_image_io, "WEBP")

        # Encode the WebP image back to base64
        webp_image_bytes = webp_image_io.getvalue()
        webp_image_io.close()
        return webp_image_bytes


def convert_image_data_uri(image_data_uri: str, target_format: str = "png") -> str:
    """
    Convert image (in data URI) to target format.

    Target format can be png, jpg, webp etc.
    Returns the converted image as a data URI.
    """
    base64_data = image_data_uri.split(",", 1)[1]
    image_type = image_data_uri.split(";")[0].split(":")[1].split("/")[1]
    if image_type.lower() == target_format.lower():
        return image_data_uri

    image_bytes = base64.b64decode(base64_data)
    image_io = io.BytesIO(image_bytes)
    with Image.open(image_io) as original_image:
        output_image_io = io.BytesIO()
        original_image.save(output_image_io, target_format.upper())

        # Encode the image back to base64
        output_image_bytes = output_image_io.getvalue()
        output_image_io.close()
        output_base64_data = base64.b64encode(output_image_bytes).decode("utf-8")
        output_data_uri = f"data:image/{target_format};base64,{output_base64_data}"
        return output_data_uri


class ImageShape(str, Enum):
    PORTRAIT = "Portrait"
    LANDSCAPE = "Landscape"
    SQUARE = "Square"


def truncate_code_context(original_code_results: dict[str, Any], max_chars=10000) -> dict[str, Any]:
    """
    Truncate large output files and drop image file data from code results.
    """
    # Create a deep copy of the code results to avoid modifying the original data
    code_results = copy.deepcopy(original_code_results)
    for code_result in code_results.values():
        for idx, output_file in enumerate(code_result["results"]["output_files"]):
            # Drop image files from code results
            if Path(output_file["filename"]).suffix in {".png", ".jpg", ".jpeg", ".webp"}:
                code_result["results"]["output_files"][idx] = {
                    "filename": output_file["filename"],
                    "b64_data": "[placeholder for generated image data for brevity]",
                }
            # Truncate large output files
            elif len(output_file["b64_data"]) > max_chars:
                code_result["results"]["output_files"][idx] = {
                    "filename": output_file["filename"],
                    "b64_data": output_file["b64_data"][:max_chars] + "...",
                }
        # Truncate long "words" in stdout, stderr. Words are alphanumeric strings not separated by whitespace.
        for key in ["std_out", "std_err"]:
            if key in code_result["results"]:
                code_result["results"][key] = re.sub(
                    r"\S{1000,}", lambda m: m.group(0)[:1000] + "...", code_result["results"][key]
                )

    return code_results


@lru_cache
def tz_to_cc_map() -> dict[str, str]:
    """Create a mapping of timezone to country code"""
    timezone_country = {}
    for countrycode in country_timezones:
        timezones = country_timezones[countrycode]
        for timezone in timezones:
            timezone_country[timezone] = countrycode
    return timezone_country


def get_country_code_from_timezone(tz: str) -> str:
    """Get country code from timezone"""
    return tz_to_cc_map().get(tz, "US")


def get_country_name_from_timezone(tz: str) -> str:
    """Get country name from timezone"""
    return country_names.get(get_country_code_from_timezone(tz), "United States")


def get_cost_of_chat_message(
    model_name: str,
    input_tokens: int = 0,
    output_tokens: int = 0,
    thought_tokens: int = 0,
    cache_read_tokens: int = 0,
    cache_write_tokens: int = 0,
    prev_cost: float = 0.0,
):
    """
    Calculate cost of chat message based on input and output tokens
    """

    # Calculate cost of input and output tokens. Costs are per million tokens
    input_cost = constants.model_to_cost.get(model_name, {}).get("input", 0) * (input_tokens / 1e6)
    output_cost = constants.model_to_cost.get(model_name, {}).get("output", 0) * (output_tokens / 1e6)
    thought_cost = constants.model_to_cost.get(model_name, {}).get("thought", 0) * (thought_tokens / 1e6)
    cache_read_cost = constants.model_to_cost.get(model_name, {}).get("cache_read", 0) * (cache_read_tokens / 1e6)
    cache_write_cost = constants.model_to_cost.get(model_name, {}).get("cache_write", 0) * (cache_write_tokens / 1e6)

    return input_cost + output_cost + thought_cost + cache_read_cost + cache_write_cost + prev_cost


def get_encoder(
    model_name: str,
    tokenizer_name=None,
) -> tiktoken.Encoding | PreTrainedTokenizer | PreTrainedTokenizerFast:
    default_tokenizer = "gpt-4o"

    try:
        if tokenizer_name:
            encoder = AutoTokenizer.from_pretrained(tokenizer_name)
        else:
            # as tiktoken doesn't recognize o1 model series yet
            encoder = tiktoken.encoding_for_model("gpt-4o" if model_name.startswith("o1") else model_name)
    except Exception:
        encoder = tiktoken.encoding_for_model(default_tokenizer)
    return encoder


def count_tokens(
    message_content: str | list[str | dict],
    encoder: PreTrainedTokenizer | PreTrainedTokenizerFast | tiktoken.Encoding,
) -> int:
    """
    Count the total number of tokens in a list of messages.

    Assumes each images takes 500 tokens for approximation.
    """
    if isinstance(message_content, list):
        image_count = 0
        message_content_parts: list[str] = []
        # Collate message content into single string to ease token counting
        for part in message_content:
            if isinstance(part, dict) and part.get("type") == "image_url":
                image_count += 1
            elif isinstance(part, dict) and part.get("type") == "text":
                message_content_parts.append(part["text"])
            elif isinstance(part, dict) and hasattr(part, "model_dump"):
                message_content_parts.append(json.dumps(part.model_dump()))
            elif isinstance(part, dict) and hasattr(part, "__dict__"):
                message_content_parts.append(json.dumps(part.__dict__))
            elif isinstance(part, dict):
                # If part is a dict but not a recognized type, convert to JSON string
                try:
                    # Skip non-serializable binary values for token counting
                    serializable_part = {
                        k: v for k, v in part.items() if not isinstance(v, (bytes, bytearray, memoryview))
                    }
                    message_content_parts.append(json.dumps(serializable_part))
                except (TypeError, ValueError) as e:
                    logger.warning(
                        f"Failed to serialize part {part} to JSON. Assume its an image for token counting.\n{e}."
                    )
                    image_count += 1  # Treat as an image/binary if serialization fails
            elif isinstance(part, str):
                message_content_parts.append(part)
            else:
                logger.warning(f"Unknown message type: {part}. Skip for token counting.")
        message_content = "\n".join(message_content_parts).rstrip()
        return len(encoder.encode(message_content)) + image_count * 500
    elif isinstance(message_content, str):
        return len(encoder.encode(message_content))
    else:
        return len(encoder.encode(json.dumps(message_content)))


def get_chat_usage_metrics(
    model_name: str,
    input_tokens: int = 0,
    output_tokens: int = 0,
    cache_read_tokens: int = 0,
    cache_write_tokens: int = 0,
    thought_tokens: int = 0,
    usage: dict = {},
    cost: float = None,
):
    """
    Get usage metrics for chat message based on input and output tokens and cost
    """
    prev_usage = usage or {
        "input_tokens": 0,
        "output_tokens": 0,
        "thought_tokens": 0,
        "cache_read_tokens": 0,
        "cache_write_tokens": 0,
        "cost": 0.0,
    }
    current_usage = {
        "input_tokens": prev_usage["input_tokens"] + input_tokens,
        "output_tokens": prev_usage["output_tokens"] + output_tokens,
        "thought_tokens": prev_usage.get("thought_tokens", 0) + thought_tokens,
        "cache_read_tokens": prev_usage.get("cache_read_tokens", 0) + cache_read_tokens,
        "cache_write_tokens": prev_usage.get("cache_write_tokens", 0) + cache_write_tokens,
        "cost": cost
        or get_cost_of_chat_message(
            model_name,
            input_tokens,
            output_tokens,
            thought_tokens,
            cache_read_tokens,
            cache_write_tokens,
            prev_cost=prev_usage["cost"],
        ),
    }
    logger.debug(f"AI API usage by {model_name}: {current_usage}")
    return current_usage


class AiApiInfo(NamedTuple):
    region: str
    project: str
    credentials: Credentials
    api_key: str


def get_gcp_credentials(credentials_b64: str) -> Optional[Credentials]:
    """
    Get GCP credentials from base64 encoded service account credentials json keyfile
    """
    credentials_json = base64.b64decode(credentials_b64).decode("utf-8")
    credentials_dict = pyjson5.loads(credentials_json)
    credentials = service_account.Credentials.from_service_account_info(credentials_dict)
    return credentials.with_scopes(scopes=["https://www.googleapis.com/auth/cloud-platform"])


def get_gcp_project_info(parsed_api_url: ParseResult) -> Tuple[str, str]:
    """
    Extract region, project id from GCP API url
    API url is of form https://{REGION}-aiplatform.googleapis.com/v1/projects/{PROJECT_ID}...
    """
    # Extract region from hostname
    hostname = parsed_api_url.netloc
    region = hostname.split("-aiplatform")[0] if "-aiplatform" in hostname else ""

    # Extract project ID from path (e.g., "/v1/projects/my-project/...")
    path_parts = parsed_api_url.path.split("/")
    project_id = ""
    for i, part in enumerate(path_parts):
        if part == "projects" and i + 1 < len(path_parts):
            project_id = path_parts[i + 1]
            break

    return region, project_id


def get_ai_api_info(api_key, api_base_url: str = None) -> AiApiInfo:
    """
    Get the GCP Vertex or default AI API client info based on the API key and URL.
    """
    region, project_id, credentials = None, None, None
    # Check if AI model to be used via GCP Vertex API
    parsed_api_url = urlparse(api_base_url)
    if parsed_api_url.hostname and parsed_api_url.hostname.endswith(".googleapis.com"):
        region, project_id = get_gcp_project_info(parsed_api_url)
        credentials = get_gcp_credentials(api_key)
    if credentials:
        api_key = None
    return AiApiInfo(region=region, project=project_id, credentials=credentials, api_key=api_key)


def get_openai_client(api_key: str, api_base_url: str) -> Union[openai.OpenAI, openai.AzureOpenAI]:
    """Get OpenAI or AzureOpenAI client based on the API Base URL"""
    parsed_url = urlparse(api_base_url)
    if parsed_url.hostname and parsed_url.hostname.endswith(".openai.azure.com"):
        client = openai.AzureOpenAI(
            api_key=api_key,
            azure_endpoint=api_base_url,
            api_version="2024-10-21",
        )
    else:
        client = openai.OpenAI(
            api_key=api_key,
            base_url=api_base_url,
        )
    return client


def get_openai_async_client(api_key: str, api_base_url: str) -> Union[openai.AsyncOpenAI, openai.AsyncAzureOpenAI]:
    """Get OpenAI or AzureOpenAI client based on the API Base URL"""
    parsed_url = urlparse(api_base_url)
    if parsed_url.hostname and parsed_url.hostname.endswith(".openai.azure.com"):
        client = openai.AsyncAzureOpenAI(
            api_key=api_key,
            azure_endpoint=api_base_url,
            api_version="2024-10-21",
        )
    else:
        client = openai.AsyncOpenAI(
            api_key=api_key,
            base_url=api_base_url,
        )
    return client


def get_anthropic_client(api_key, api_base_url=None) -> anthropic.Anthropic | anthropic.AnthropicVertex:
    api_info = get_ai_api_info(api_key, api_base_url)
    if api_info.api_key:
        client = anthropic.Anthropic(api_key=api_info.api_key)
    else:
        client = anthropic.AnthropicVertex(
            region=api_info.region,
            project_id=api_info.project,
            credentials=api_info.credentials,
        )
    return client


def get_anthropic_async_client(api_key, api_base_url=None) -> anthropic.AsyncAnthropic | anthropic.AsyncAnthropicVertex:
    api_info = get_ai_api_info(api_key, api_base_url)
    if api_info.api_key:
        client = anthropic.AsyncAnthropic(api_key=api_info.api_key)
    else:
        client = anthropic.AsyncAnthropicVertex(
            region=api_info.region,
            project_id=api_info.project,
            credentials=api_info.credentials,
        )
    return client


def get_gemini_client(api_key, api_base_url=None) -> genai.Client:
    api_info = get_ai_api_info(api_key, api_base_url)
    return genai.Client(
        location=api_info.region,
        project=api_info.project,
        credentials=api_info.credentials,
        api_key=api_info.api_key,
        vertexai=api_info.api_key is None,
    )


def normalize_email(email: str, check_deliverability=False) -> tuple[str, bool]:
    """Normalize, validate and check deliverability of email address"""
    lower_email = email.lower()
    try:
        valid_email = validate_email(lower_email, check_deliverability=check_deliverability)
        return valid_email.normalized, True
    except (EmailNotValidError, EmailUndeliverableError):
        return lower_email, False


def clean_text_for_db(text):
    """Remove characters that PostgreSQL DB cannot store in text fields.

    PostgreSQL text fields cannot contain NUL (0x00) characters.
    This is a database-level constraint.
    """
    if not isinstance(text, str):
        return text
    return text.replace("\x00", "")


def clean_object_for_db(data):
    """Recursively clean PostgreSQL-incompatible characters from nested data structures."""
    if isinstance(data, str):
        return clean_text_for_db(data)
    elif isinstance(data, dict):
        return {k: clean_object_for_db(v) for k, v in data.items()}
    elif isinstance(data, list):
        return [clean_object_for_db(item) for item in data]
    else:
        return data


def dict_to_tuple(d):
    # Recursively convert dicts to sorted tuples for hashability
    if isinstance(d, dict):
        return tuple(sorted((k, dict_to_tuple(v)) for k, v in d.items()))
    elif isinstance(d, list):
        return tuple(dict_to_tuple(i) for i in d)
    else:
        return d
