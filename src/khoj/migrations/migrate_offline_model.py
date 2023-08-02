import os
import logging
from packaging import version

from khoj.utils.yaml import load_config_from_file, save_config_to_file

logger = logging.getLogger(__name__)


def migrate_offline_model(args):
    raw_config = load_config_from_file(args.config_file)
    version_no = raw_config.get("version")

    if version_no is None or version.parse(version_no) < version.parse("0.10.1"):
        logger.info(f"Migrating offline model used for version {version_no} to latest version for {args.version_no}")
        raw_config["version"] = "0.10.1"

        # If the user has downloaded the offline model, remove it from the cache.
        offline_model_path = os.path.expanduser("~/.cache/gpt4all/llama-2-7b-chat.ggmlv3.q4_K_S.bin")
        if os.path.exists(offline_model_path):
            os.remove(offline_model_path)

        save_config_to_file(raw_config, args.config_file)

    return args
