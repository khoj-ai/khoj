"""
Current format of khoj.yml
---
app:
    ...
content-type:
    ...
processor:
  conversation:
    enable-offline-chat: false
    conversation-logfile: ~/.khoj/processor/conversation/conversation_logs.json
    openai:
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
        chat-model: llama-2-7b-chat.ggmlv3.q4_0.bin
    tokenizer: null
    max_prompt_size: null
    conversation-logfile: ~/.khoj/processor/conversation/conversation_logs.json
    openai:
        ...
search-type:
    ...
"""
import logging

from packaging import version

from khoj.utils.yaml import load_config_from_file, save_config_to_file

logger = logging.getLogger(__name__)


def migrate_offline_chat_schema(args):
    schema_version = "0.12.3"
    raw_config = load_config_from_file(args.config_file)
    previous_version = raw_config.get("version")

    if "processor" not in raw_config:
        return args
    if raw_config["processor"] is None:
        return args
    if "conversation" not in raw_config["processor"]:
        return args

    if previous_version is None or version.parse(previous_version) < version.parse("0.12.3"):
        logger.info(
            f"Upgrading config schema to {schema_version} from {previous_version} to make (offline) chat more configuration"
        )
        raw_config["version"] = schema_version

        # Create max-prompt-size field in conversation processor schema
        raw_config["processor"]["conversation"]["max-prompt-size"] = None
        raw_config["processor"]["conversation"]["tokenizer"] = None

        # Create offline chat schema based on existing enable_offline_chat field in khoj config schema
        offline_chat_model = (
            raw_config["processor"]["conversation"]
            .get("offline-chat", {})
            .get("chat-model", "llama-2-7b-chat.ggmlv3.q4_0.bin")
        )
        raw_config["processor"]["conversation"]["offline-chat"] = {
            "enable-offline-chat": raw_config["processor"]["conversation"].get("enable-offline-chat", False),
            "chat-model": offline_chat_model,
        }

        # Delete old enable-offline-chat field from conversation processor schema
        if "enable-offline-chat" in raw_config["processor"]["conversation"]:
            del raw_config["processor"]["conversation"]["enable-offline-chat"]

        save_config_to_file(raw_config, args.config_file)
    return args
