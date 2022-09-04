# Standard Packages
import pathlib
import sys
from os.path import join
from collections import OrderedDict


def is_none_or_empty(item):
    return item == None or (hasattr(item, '__iter__') and len(item) == 0) or item == ''


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


def merge_dicts(priority_dict: dict, default_dict: dict):
    merged_dict = priority_dict.copy()
    for key, _ in default_dict.items():
        if key not in priority_dict:
            merged_dict[key] = default_dict[key]
        elif isinstance(priority_dict[key], dict) and isinstance(default_dict[key], dict):
            merged_dict[key] = merge_dicts(priority_dict[key], default_dict[key])
    return merged_dict


def load_model(model_name, model_dir, model_type, device:str=None):
    "Load model from disk or huggingface"
    # Construct model path
    model_path = join(model_dir, model_name.replace("/", "_")) if model_dir is not None else None

    # Load model from model_path if it exists there
    if model_path is not None and resolve_absolute_path(model_path).exists():
        model = model_type(get_absolute_path(model_path), device=device)
    # Else load the model from the model_name
    else:
        model = model_type(model_name, device=device)
        if model_path is not None:
            model.save(model_path)

    return model


def is_pyinstaller_app():
    "Returns true if the app is running from Native GUI created by PyInstaller"
    return getattr(sys, 'frozen', False) and hasattr(sys, '_MEIPASS')


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
