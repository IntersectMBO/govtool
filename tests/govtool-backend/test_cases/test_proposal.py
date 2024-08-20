from models.TestData import Proposal, ProposalListResponse, GetProposalResponse
import allure


def validate_proposal(proposal: Proposal) -> bool:
    assert isinstance(proposal, dict), f"Expected Proposal to be of type dict, got {type(proposal)}"

    for key in Proposal.__annotations__:
        assert key in proposal, f"Expected Proposal.{key} to be present"
        assert isinstance(
            proposal[key], Proposal.__annotations__[key]
        ), f"proposal.{key} should be of type {Proposal.__annotations__[key]} got {type(proposal[key])}"
    return True


@allure.story("Proposal")
def test_list_proposal(govtool_api):
    response = govtool_api.proposal_list()
    proposal_list = response.json()
    for proposal in proposal_list["elements"]:
        assert validate_proposal(proposal)


@allure.story("Proposal")
def test_get_proposal(govtool_api):
    response: ProposalListResponse = govtool_api.proposal_list().json()
    for proposal in response["elements"]:
        proposal_get: GetProposalResponse = govtool_api.get_proposal(
            proposal["txHash"] + "%23" + str(proposal["index"])
        ).json()
        assert validate_proposal(proposal_get["proposal"])
