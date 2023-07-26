"""
Current format of khoj.yml
---
app:
  should-log-telemetry: true
content-type:
    ...
processor:
  conversation:
    chat-model: gpt-3.5-turbo
    conversation-logfile: ~/.khoj/processor/conversation/conversation_logs.json
    model: text-davinci-003
    openai-api-key: sk-secret-key
search-type:
    ...

New format of khoj.yml
---
app:
  should-log-telemetry: true
content-type:
    ...
processor:
  conversation:
    openai:
        chat-model: gpt-3.5-turbo
        openai-api-key: sk-secret-key
    conversation-logfile: ~/.khoj/processor/conversation/conversation_logs.json
    enable-offline-chat: false
search-type:
    ...
"""
from khoj.utils.yaml import load_config_from_file, save_config_to_file


def migrate_processor_conversation_schema(args):
    raw_config = load_config_from_file(args.config_file)

    raw_config["version"] = args.version_no

    if "processor" not in raw_config:
        return args
    if raw_config["processor"] is None:
        return args
    if "conversation" not in raw_config["processor"]:
        return args

    # Add enable_offline_chat to khoj config schema
    if "enable-offline-chat" not in raw_config["processor"]["conversation"]:
        raw_config["processor"]["conversation"]["enable-offline-chat"] = False
        save_config_to_file(raw_config, args.config_file)

    current_openai_api_key = raw_config["processor"]["conversation"].get("openai-api-key", None)
    current_chat_model = raw_config["processor"]["conversation"].get("chat-model", None)
    if current_openai_api_key is None and current_chat_model is None:
        return args

    conversation_logfile = raw_config["processor"]["conversation"].get("conversation-logfile", None)

    raw_config["processor"]["conversation"] = {
        "openai": {"chat-model": current_chat_model, "api-key": current_openai_api_key},
        "conversation-logfile": conversation_logfile,
        "enable-offline-chat": False,
    }
    save_config_to_file(raw_config, args.config_file)
    return args
