import pathlib


def is_none_or_empty(item):
    return item == None or (hasattr(item, '__iter__') and len(item) == 0)


def get_absolute_path(filepath):
    return str(pathlib.Path(filepath).expanduser().absolute())


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
    for k, v in default_dict.items():
        if k not in priority_dict:
            merged_dict[k] = default_dict[k]
    return merged_dict
