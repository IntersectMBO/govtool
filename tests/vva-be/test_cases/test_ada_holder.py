from models.TestData import AdaHolder, Delegation

def test_ada_delegation(vva_api, ada_holder_delegate_to_drep):
    print(ada_holder_delegate_to_drep)
    response = vva_api.ada_holder_get_current_delegation(ada_holder_delegate_to_drep["stakeKey"])
    resp = response.json()
    if resp:
        assert ada_holder_delegate_to_drep["drepId"] in resp


def test_check_voting_power(vva_api, ada_holder_delegate_to_drep):
    response = vva_api.ada_holder_get_voting_power(ada_holder_delegate_to_drep["stakeKey"])
    ada_holder_voting_power = response.json()
    assert isinstance(ada_holder_voting_power, int)
