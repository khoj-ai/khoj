from src.utils import helpers

def test_get_from_null_dict():
    # null handling
    assert helpers.get_from_dict(dict()) == dict()
    assert helpers.get_from_dict(dict(), None) == None

    # key present in nested dictionary
    # 1-level dictionary
    assert helpers.get_from_dict({'a': 1, 'b': 2}, 'a') == 1
    assert helpers.get_from_dict({'a': 1, 'b': 2}, 'c') == None

    # 2-level dictionary
    assert helpers.get_from_dict({'a': {'a_a': 1}, 'b': 2}, 'a') == {'a_a': 1}
    assert helpers.get_from_dict({'a': {'a_a': 1}, 'b': 2}, 'a', 'a_a') == 1

    # key not present in nested dictionary
    # 2-level_dictionary
    assert helpers.get_from_dict({'a': {'a_a': 1}, 'b': 2}, 'b', 'b_a') == None


def test_merge_dicts():
    # basic merge of dicts with non-overlapping keys
    assert helpers.merge_dicts(priority_dict={'a': 1}, default_dict={'b': 2}) == {'a': 1, 'b': 2}

    # use default dict items when not present in priority dict
    assert helpers.merge_dicts(priority_dict={}, default_dict={'b': 2}) == {'b': 2}

    # do not override existing key in priority_dict with default dict
    assert helpers.merge_dicts(priority_dict={'a': 1}, default_dict={'a': 2}) == {'a': 1}
