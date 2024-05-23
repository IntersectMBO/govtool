from models.TestData import Drep, VoteonProposal, Vote, Proposal, DrepInfo
import allure


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
        if "vote" not in item or not isinstance(item["vote"], dict):
            return False
        if not all(key in item["vote"] for key in Vote.__annotations__):
            return False
        if not all(isinstance(item["vote"][key], Vote.__annotations__[key]) for key in Vote.__annotations__):
            return False

        # Validate the 'proposal' key against the Proposal type
        if "proposal" not in item or not isinstance(item["proposal"], dict):
            return False
        if not all(key in item["proposal"] for key in Proposal.__annotations__):
            return False
        if not all(
            isinstance(item["proposal"][key], Proposal.__annotations__[key]) for key in Proposal.__annotations__
        ):
            return False

    return True


def validate_drep_info(drep):
    for key, val in DrepInfo.__annotations__.items():
        assert isinstance(
            drep[key], DrepInfo.__annotations__[key]
        ), f"drepInfo.{key} should be of type {DrepInfo.__annotations__[key]} got {type(drep[key])}"


@allure.story("Drep")
def test_list_drep(govtool_api):
    response = govtool_api.drep_list()
    drep_list = response.json()
    validate_drep_list(drep_list)


@allure.story("Drep")
def test_drep_getVotes(govtool_api, registered_drep):
    response = govtool_api.drep_getVotes(registered_drep["drepId"])
    validate_voteonproposal_list(response.json())
    votes = response.json()
    proposals = map(lambda x: x["vote"]["proposalId"], votes)
    proposals = list(proposals)
    assert len(proposals) == 0


@allure.story("Drep")
def test_drep_voting_power(govtool_api, registered_drep):
    response = govtool_api.drep_get_voting_power(registered_drep["drepId"])
    assert isinstance(response.json(), int)


@allure.story("Drep")
def test_drep_get_info(govtool_api, registered_drep):
    response = govtool_api.drep_info(registered_drep["drepId"])
    validate_drep_info(response.json())
