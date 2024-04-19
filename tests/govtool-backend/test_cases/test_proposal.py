from models.TestData import Proposal
import allure

@allure.story("Proposal")
def validate_proposal_list(proposal_list: [Proposal]) -> bool:
    for item in proposal_list:
        if not isinstance(item, dict):
            return False
        if not all(key in item for key in Proposal.__annotations__):
            return False
        if not all(isinstance(item[key], Proposal.__annotations__[key]) for key in Proposal.__annotations__):
            return False
        if not all(isinstance(item[key], int) for key in ['yesVotes', 'noVotes', 'abstainVotes']):
            return False
    return True


@allure.story("Proposal")
def test_list_proposal(govtool_api):
    response = govtool_api.proposal_list()
    proposal_list = response.json()
    assert validate_proposal_list(proposal_list)
