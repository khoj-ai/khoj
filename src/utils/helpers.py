# Standard Packages
import pathlib
from os.path import join


def is_none_or_empty(item):
    return item == None or (hasattr(item, '__iter__') and len(item) == 0)


def to_snake_case_from_dash(item: str):
    return item.replace('_', '-')


def get_absolute_path(filepath):
    return str(pathlib.Path(filepath).expanduser().absolute())


def resolve_absolute_path(filepath, strict=False):
    return pathlib.Path(filepath).expanduser().absolute().resolve(strict=strict)


def get_from_dict(dictionary, *args):
    '''null-aware get from a nested dictionary
       Returns: dictionary[args[0]][args[1]]... or None if any keys missing'''
    current = dictionary
    for arg in args:
        if not hasattr(current, '__iter__') or not arg in current:
            return None
        current = current[arg]
    return current


def merge_dicts(priority_dict, default_dict):
    merged_dict = priority_dict.copy()
    for key, _ in default_dict.items():
        if key not in priority_dict:
            merged_dict[key] = default_dict[key]
    return merged_dict


def load_model(model_name, model_dir, model_type):
    "Load model from disk or huggingface"
    # Construct model path
    model_path = join(model_dir, model_name.replace("/", "_")) if model_dir is not None else None

    # Load model from model_path if it exists there
    if model_path is not None and resolve_absolute_path(model_path).exists():
        model = model_type(get_absolute_path(model_path))
    # Else load the model from the model_name
    else:
        model = model_type(model_name)
        if model_path is not None:
            model.save(model_path)

    return model