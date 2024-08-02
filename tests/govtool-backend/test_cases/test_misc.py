import allure

from lib.assertions import assert_data_type
from models.TestData import EpochParam, NetworkMetrics, TxStatus


def validate_epoch_param(epoch_param):
    assert_data_type(EpochParam, epoch_param)


def validate_network_metrics(network_metrics):
    assert_data_type(NetworkMetrics, network_metrics)


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
    assert_data_type(TxStatus, tx_status)
