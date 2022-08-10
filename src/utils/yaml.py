# External Packages
import yaml

# Internal Packages
from src.utils.helpers import get_absolute_path
from src.utils.rawconfig import FullConfig


def save_config_to_file(yaml_config, yaml_config_file):
    "Write config to YML file"
    with open(get_absolute_path(yaml_config_file), 'w', encoding='utf-8') as config_file:
        yaml.dump(yaml_config, config_file, allow_unicode=True)


def load_config_from_file(yaml_config_file):
    "Read config from YML file"
    config_from_file = None
    with open(get_absolute_path(yaml_config_file), 'r', encoding='utf-8') as config_file:
        config_from_file = yaml.safe_load(config_file)
    return config_from_file


def parse_config_from_string(yaml_config):
    "Parse and validate config in YML string"
    return FullConfig.parse_obj(yaml_config)


def parse_config_from_file(yaml_config_file):
    "Parse and validate config in YML file"
    return parse_config_from_string(load_config_from_file(yaml_config_file))
