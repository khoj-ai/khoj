import pathlib


def is_none_or_empty(item):
    return item == None or (hasattr(item, '__iter__') and len(item) == 0)


def get_absolute_path(filepath):
    return str(pathlib.Path(filepath).expanduser().absolute())
