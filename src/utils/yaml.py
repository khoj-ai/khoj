# External Packages
import yaml

# Internal Packages
from src.utils.helpers import get_absolute_path
from src.utils.rawconfig import FullConfig

def load_config_from_file(yaml_config_file):
    # Read Config from YML file
    config_from_file = None
    with open(get_absolute_path(yaml_config_file), 'r', encoding='utf-8') as config_file:
        config_from_file = yaml.safe_load(config_file)

    # Parse, Validate Config in YML file
    return FullConfig.parse_obj(config_from_file)
