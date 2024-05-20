import pytest
from test_data import drep_data, ada_holders
from models.TestData import Delegation, Drep, AdaHolder


@pytest.fixture(scope="module", params=ada_holders)
def ada_holder_delegate_to_drep(request, govtool_api):
    ada_holder: AdaHolder = request.param

    delegation_data = Delegation(stakeKey=ada_holder["stakeKey"], dRepId=ada_holder["drepId"])

    yield delegation_data
