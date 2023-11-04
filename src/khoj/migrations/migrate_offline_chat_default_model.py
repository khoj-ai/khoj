"""
Current format of khoj.yml
---
app:
    ...
content-type:
    ...
processor:
  conversation:
    offline-chat:
        enable-offline-chat: false
        chat-model: llama-2-7b-chat.ggmlv3.q4_0.bin
    ...
search-type:
    ...

New format of khoj.yml
---
app:
    ...
content-type:
    ...
processor:
  conversation:
    offline-chat:
        enable-offline-chat: false
        chat-model: mistral-7b-instruct-v0.1.Q4_0.gguf
    ...
search-type:
    ...
"""
import logging
from packaging import version

from khoj.utils.yaml import load_config_from_file, save_config_to_file


logger = logging.getLogger(__name__)


def migrate_offline_chat_default_model(args):
    schema_version = "0.12.4"
    raw_config = load_config_from_file(args.config_file)
    previous_version = raw_config.get("version")

    if "processor" not in raw_config:
        return args
    if raw_config["processor"] is None:
        return args
    if "conversation" not in raw_config["processor"]:
        return args
    if "offline-chat" not in raw_config["processor"]["conversation"]:
        return args
    if "chat-model" not in raw_config["processor"]["conversation"]["offline-chat"]:
        return args

    if previous_version is None or version.parse(previous_version) < version.parse("0.12.4"):
        logger.info(
            f"Upgrading config schema to {schema_version} from {previous_version} to change default (offline) chat model to mistral GGUF"
        )
        raw_config["version"] = schema_version

        # Update offline chat model to mistral in GGUF format to use latest GPT4All
        offline_chat_model = raw_config["processor"]["conversation"]["offline-chat"]["chat-model"]
        if offline_chat_model.endswith(".bin"):
            raw_config["processor"]["conversation"]["offline-chat"]["chat-model"] = "mistral-7b-instruct-v0.1.Q4_0.gguf"

        save_config_to_file(raw_config, args.config_file)
    return args
