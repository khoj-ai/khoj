"""
The application config currently looks like this:
app:
  should-log-telemetry: true
content-type:
    ...
processor:
  conversation:
    conversation-logfile: ~/.khoj/processor/conversation/conversation_logs.json
    max-prompt-size: null
    offline-chat:
      chat-model: mistral-7b-instruct-v0.1.Q4_0.gguf
      enable-offline-chat: false
    openai:
      api-key: sk-blah
      chat-model: gpt-3.5-turbo
    tokenizer: null
search-type:
  asymmetric:
    cross-encoder: cross-encoder/ms-marco-MiniLM-L-6-v2
    encoder: sentence-transformers/multi-qa-MiniLM-L6-cos-v1
    encoder-type: null
    model-directory: /Users/si/.khoj/search/asymmetric
  image:
    encoder: sentence-transformers/clip-ViT-B-32
    encoder-type: null
    model-directory: /Users/si/.khoj/search/image
  symmetric:
    cross-encoder: cross-encoder/ms-marco-MiniLM-L-6-v2
    encoder: sentence-transformers/all-MiniLM-L6-v2
    encoder-type: null
    model-directory: ~/.khoj/search/symmetric
version: 0.14.0


The new version will looks like this:
app:
  should-log-telemetry: true
processor:
  conversation:
    offline-chat:
      enabled: false
    openai:
      api-key: sk-blah
    chat-model-options:
      - chat-model: gpt-3.5-turbo
        tokenizer: null
        type: openai
      - chat-model: mistral-7b-instruct-v0.1.Q4_0.gguf
        tokenizer: null
        type: offline
search-type:
  asymmetric:
    cross-encoder: cross-encoder/ms-marco-MiniLM-L-6-v2
    encoder: sentence-transformers/multi-qa-MiniLM-L6-cos-v1
version: 0.15.0
"""

import logging

from packaging import version

from khoj.database.models import (
    ChatModelOptions,
    OpenAIProcessorConversationConfig,
    SearchModelConfig,
)
from khoj.utils.yaml import load_config_from_file, save_config_to_file

logger = logging.getLogger(__name__)


def migrate_server_pg(args):
    schema_version = "0.15.0"
    raw_config = load_config_from_file(args.config_file)
    previous_version = raw_config.get("version")

    if previous_version is None or version.parse(previous_version) < version.parse(schema_version):
        logger.info(
            f"Migrating configuration used for version {previous_version} to latest version for server with postgres in {args.version_no}"
        )
        raw_config["version"] = schema_version

        if raw_config is None:
            return args

        if "search-type" in raw_config and raw_config["search-type"]:
            if "asymmetric" in raw_config["search-type"]:
                # Delete all existing search models
                SearchModelConfig.objects.filter(model_type=SearchModelConfig.ModelType.TEXT).delete()
                # Create new search model from existing Khoj YAML config
                asymmetric_search = raw_config["search-type"]["asymmetric"]
                SearchModelConfig.objects.create(
                    name="default",
                    model_type=SearchModelConfig.ModelType.TEXT,
                    bi_encoder=asymmetric_search.get("encoder"),
                    cross_encoder=asymmetric_search.get("cross-encoder"),
                )

        if "processor" in raw_config and raw_config["processor"] and "conversation" in raw_config["processor"]:
            processor_conversation = raw_config["processor"]["conversation"]

            if "offline-chat" in raw_config["processor"]["conversation"]:
                offline_chat = raw_config["processor"]["conversation"]["offline-chat"]
                ChatModelOptions.objects.create(
                    chat_model=offline_chat.get("chat-model"),
                    tokenizer=processor_conversation.get("tokenizer"),
                    max_prompt_size=processor_conversation.get("max-prompt-size"),
                    model_type=ChatModelOptions.ModelType.OFFLINE,
                )

            if (
                "openai" in raw_config["processor"]["conversation"]
                and raw_config["processor"]["conversation"]["openai"]
            ):
                openai = raw_config["processor"]["conversation"]["openai"]

                if openai.get("api-key") is None:
                    logger.error("OpenAI API Key is not set. Will not be migrating OpenAI config.")
                else:
                    if openai.get("chat-model") is None:
                        openai["chat-model"] = "gpt-3.5-turbo"

                    openai_config = OpenAIProcessorConversationConfig.objects.create(
                        api_key=openai.get("api-key"), name="default"
                    )

                    ChatModelOptions.objects.create(
                        chat_model=openai.get("chat-model"),
                        tokenizer=processor_conversation.get("tokenizer"),
                        max_prompt_size=processor_conversation.get("max-prompt-size"),
                        model_type=ChatModelOptions.ModelType.OPENAI,
                        openai_config=openai_config,
                    )

        save_config_to_file(raw_config, args.config_file)

    return args
