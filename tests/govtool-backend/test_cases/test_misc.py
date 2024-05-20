import allure

from models.TestData import EpochParam, NetworkMetrics, TxStatus


def validate_epoch_param(epoch_param):
    for key, val in EpochParam.__annotations__.items():
        assert isinstance(
            epoch_param[key], EpochParam.__annotations__[key]
        ), f"epochParam.{key} should be of type {EpochParam.__annotations__[key]} got {type(epoch_param[key])}"


def validate_network_metrics(network_metrics):
    for key, val in NetworkMetrics.__annotations__.items():
        assert isinstance(
            network_metrics[key], NetworkMetrics.__annotations__[key]
        ), f"epochParam.{key} should be of type {NetworkMetrics.__annotations__[key]} got {type(network_metrics[key])}"


def validate_model(model, item):
    for key, val in model.__annotations__.items():
        assert isinstance(item[key], val), f"{model.__name__}.{key} should be of type {val} got {type(item[key])}"


@allure.story("Misc")
def test_get_epoch_param(govtool_api):
    epoch_param: EpochParam = govtool_api.epoch_params().json()
    validate_epoch_param(epoch_param)


@allure.story("Misc")
def test_get_network_metrics(govtool_api):
    network_metrics = govtool_api.network_metrics().json()
    validate_network_metrics(network_metrics)


@allure.story("Misc")
def test_get_transaction_status(govtool_api):
    tx_status = govtool_api.get_transaction_status("ff" * 32).json()
    validate_model(TxStatus, tx_status)
