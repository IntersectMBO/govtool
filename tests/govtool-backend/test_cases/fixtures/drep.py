import pytest
from test_data import drep_data
from models.TestData import Drep


@pytest.fixture(scope="module", params=drep_data)
def registered_drep(request):
    drep_datum: Drep = request.param
    yield drep_datum