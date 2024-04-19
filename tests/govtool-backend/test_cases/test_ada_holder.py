from models.TestData import AdaHolder, Delegation
import allure

@allure.story("AdaHolder")
def test_ada_delegation(govtool_api, ada_holder_delegate_to_drep):
    print(ada_holder_delegate_to_drep)
    response = govtool_api.ada_holder_get_current_delegation(ada_holder_delegate_to_drep["stakeKey"])
    resp = response.json()
    if resp:
        assert ada_holder_delegate_to_drep["drepId"] in resp

@allure.story("Drep")
def test_check_voting_power(govtool_api, ada_holder_delegate_to_drep):
    response = govtool_api.ada_holder_get_voting_power(ada_holder_delegate_to_drep["stakeKey"])
    ada_holder_voting_power = response.json()
    assert isinstance(ada_holder_voting_power, int)
