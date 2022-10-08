# Standard Packages
from pathlib import Path

# External Packages
import yaml

# Internal Packages
from src.utils.rawconfig import FullConfig


# Do not emit tags when dumping to YAML
yaml.emitter.Emitter.process_tag = lambda self, *args, **kwargs: None  # type: ignore[assignment]


def save_config_to_file(yaml_config: dict, yaml_config_file: Path):
    "Write config to YML file"
    # Create output directory, if it doesn't exist
    yaml_config_file.parent.mkdir(parents=True, exist_ok=True)

    with open(yaml_config_file, 'w', encoding='utf-8') as config_file:
        yaml.safe_dump(yaml_config, config_file, allow_unicode=True)


def load_config_from_file(yaml_config_file: Path) -> dict:
    "Read config from YML file"
    config_from_file = None
    with open(yaml_config_file, 'r', encoding='utf-8') as config_file:
        config_from_file = yaml.safe_load(config_file)
    return config_from_file


def parse_config_from_string(yaml_config: dict) -> FullConfig:
    "Parse and validate config in YML string"
    return FullConfig.parse_obj(yaml_config)


def parse_config_from_file(yaml_config_file):
    "Parse and validate config in YML file"
    return parse_config_from_string(load_config_from_file(yaml_config_file))
