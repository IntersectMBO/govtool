from models.TestData import Drep, VoteonProposal, Vote, Proposal

def validate_drep_list(drep_list: [Drep]) -> bool:
    for item in drep_list:
        if not isinstance(item, dict):
            return False
        if not all(key in item for key in Drep.__annotations__):
            return False
        if not all(isinstance(item[key], Drep.__annotations__[key]) for key in Drep.__annotations__):
            return False
    return True

def validate_voteonproposal_list(voteonproposal_list: [VoteonProposal]) -> bool:
    for item in voteonproposal_list:
        if not isinstance(item, dict):
            return False

        # Validate the 'vote' key against the Vote type
        if 'vote' not in item or not isinstance(item['vote'], dict):
            return False
        if not all(key in item['vote'] for key in Vote.__annotations__):
            return False
        if not all(isinstance(item['vote'][key], Vote.__annotations__[key]) for key in Vote.__annotations__):
            return False

        # Validate the 'proposal' key against the Proposal type
        if 'proposal' not in item or not isinstance(item['proposal'], dict):
            return False
        if not all(key in item['proposal'] for key in Proposal.__annotations__):
            return False
        if not all(isinstance(item['proposal'][key], Proposal.__annotations__[key]) for key in Proposal.__annotations__):
            return False

    return True


def test_list_drep(vva_api):
    response = vva_api.drep_list()
    drep_list = response.json()
    validate_drep_list(drep_list)


def test_initialized_getVotes( vva_api, registered_drep):
    response = vva_api.drep_getVotes(registered_drep["drepId"])
    validate_voteonproposal_list(response.json())
    votes = response.json()
    proposals = map(lambda x: x["vote"]["proposalId"], votes)
    proposals = list(proposals)
    assert len(proposals)==0


def test_initialized_getVotingPower(vva_api, registered_drep):
    response = vva_api.drep_get_voting_power(registered_drep["drepId"])
    assert isinstance(response.json(), int)