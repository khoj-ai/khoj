# Standard Packages
from pathlib import Path
import sys
import time
import hashlib
from os.path import join
from collections import OrderedDict
from typing import Optional, Union


def is_none_or_empty(item):
    return item == None or (hasattr(item, '__iter__') and len(item) == 0) or item == ''


def to_snake_case_from_dash(item: str):
    return item.replace('_', '-')


def get_absolute_path(filepath: Union[str, Path]) -> str:
    return str(Path(filepath).expanduser().absolute())


def resolve_absolute_path(filepath: Union[str, Optional[Path]], strict=False) -> Path:
    return Path(filepath).expanduser().absolute().resolve(strict=strict)


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


def mark_entries_for_update(current_entries, previous_entries, key='compiled', logger=None):
    # Hash all current and previous entries to identify new entries
    start = time.time()
    current_entry_hashes = list(map(lambda e: hashlib.md5(bytes(e[key], encoding='utf-8')).hexdigest(), current_entries))
    previous_entry_hashes = list(map(lambda e: hashlib.md5(bytes(e[key], encoding='utf-8')).hexdigest(), previous_entries))
    end = time.time()
    logger.debug(f"Hash previous, current entries: {end - start} seconds")

    start = time.time()
    hash_to_current_entries = dict(zip(current_entry_hashes, current_entries))
    hash_to_previous_entries = dict(zip(previous_entry_hashes, previous_entries))

    # All entries that did not exist in the previous set are to be added
    new_entry_hashes = set(current_entry_hashes) - set(previous_entry_hashes)
    # All entries that exist in both current and previous sets are kept
    existing_entry_hashes = set(current_entry_hashes) & set(previous_entry_hashes)

    # Mark new entries with no ids for later embeddings generation
    new_entries = [
        (None, hash_to_current_entries[entry_hash])
        for entry_hash in new_entry_hashes
    ]
    # Set id of existing entries to their previous ids to reuse their existing encoded embeddings
    existing_entries = [
        (previous_entry_hashes.index(entry_hash), hash_to_previous_entries[entry_hash])
        for entry_hash in existing_entry_hashes
    ]

    existing_entries_sorted = sorted(existing_entries, key=lambda e: e[0])
    entries_with_ids = existing_entries_sorted + new_entries
    end = time.time()
    logger.debug(f"Identify, Mark, Combine new, existing entries: {end - start} seconds")

    return entries_with_ids