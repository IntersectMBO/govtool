from typing import _TypedDictMeta


def assert_data_type(typed_dict: _TypedDictMeta, result_data):
    for key, val in typed_dict.__annotations__.items():
        if typed_dict.__annotations__[key] == float:
            assert isinstance(result_data[key], float) or isinstance(
                result_data[key], int
            ), f"{dir(typed_dict.__name__)}.{key} should be of type {typed_dict.__annotations__[key]} got {type(result_data[key])}"

        else:
            assert isinstance(
                result_data[key], typed_dict.__annotations__[key]
            ), f"{dir(typed_dict.__name__)}.{key} should be of type {typed_dict.__annotations__[key]} got {type(result_data[key])}"
