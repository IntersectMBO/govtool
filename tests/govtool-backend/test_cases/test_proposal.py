from models.TestData import Proposal, ProposalListResponse, GetProposalResponse
from datetime import datetime, timedelta
import allure
from lib.kuber_api import KuberApi
kuber_api = KuberApi.from_env()

def validate_proposal(proposal: Proposal) -> bool:
    assert isinstance(proposal, dict), f"Expected Proposal to be of type dict, got {type(proposal)}"

    for key in Proposal.__annotations__:
        assert key in proposal, f"Expected Proposal.{key} to be present"
        assert isinstance(
            proposal[key], Proposal.__annotations__[key]
        ), f"proposal.{key} should be of type {Proposal.__annotations__[key]} got {type(proposal[key])}"
    return True

def validate_expiry_epoch(proposal,gov_action_lifetime) -> bool:
    created_epoch_no = proposal['createdEpochNo']
    actual_expiry_epoch_no = proposal['expiryEpochNo']
    expected_expiry_epoch_no = created_epoch_no + gov_action_lifetime + 1
    assert actual_expiry_epoch_no == expected_expiry_epoch_no, (
            f"Expected expiry epoch {expected_expiry_epoch_no}, but got {actual_expiry_epoch_no}"
        )
    return True

def validate_expiry_date(proposal, gov_action_lifetime, query_system) -> bool:
    actual_expiry_date = proposal['expiryDate']
    actual_expiry_date_in_datetime_format = datetime.strptime(actual_expiry_date, "%Y-%m-%dT%H:%M:%SZ")

    # Calculate the expected expiry date
    epoch_length = query_system['epochLength']
    created_epoch_no = proposal['createdEpochNo']
    start_time_of_0_epoch = query_system['systemStartTime']
    added_seconds = (created_epoch_no + 1 + gov_action_lifetime) * epoch_length

    # Parse the start time of epoch 0 and add the calculated seconds
    expected_expiry_date = datetime.strptime(start_time_of_0_epoch, "%Y-%m-%dT%H:%M:%SZ") + timedelta(seconds=added_seconds)

    max_time_difference = timedelta(minutes=10)
    time_difference = abs(expected_expiry_date - actual_expiry_date_in_datetime_format)

    # Assert that the difference is not greater than 10 minutes
    assert time_difference <= max_time_difference, f"Time difference is greater than 10 minutes: {time_difference}"

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

@allure.story("Proposal")
def test_epoch_boundary(govtool_api):
    query_system = kuber_api.get_query_system_start()
    epochParams = govtool_api.epoch_params().json()
    gov_action_lifetime = epochParams['gov_action_lifetime']

    response = govtool_api.proposal_list()
    proposal_list = response.json()
    for proposal in proposal_list["elements"]:
        assert validate_expiry_epoch(proposal,gov_action_lifetime)
        assert validate_expiry_date(proposal,gov_action_lifetime,query_system)