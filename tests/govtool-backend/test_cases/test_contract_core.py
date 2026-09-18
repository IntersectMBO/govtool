import pytest

from test_data import ada_holders, drep_data


def assert_int(value):
    assert isinstance(value, int) and not isinstance(value, bool)


def assert_nullable(value, expected_type):
    if value is None:
        return
    if expected_type is int:
        assert_int(value)
        return
    assert isinstance(value, expected_type)


def assert_page_contract(data, page, page_size):
    assert set(data) >= {"page", "pageSize", "total", "elements"}
    assert data["page"] == page
    assert data["pageSize"] == page_size
    assert_int(data["total"])
    assert isinstance(data["elements"], list)
    assert len(data["elements"]) <= page_size


def assert_drep_contract(drep):
    required_fields = {
        "isScriptBased",
        "drepId",
        "view",
        "url",
        "metadataHash",
        "deposit",
        "votingPower",
        "status",
        "type",
        "latestTxHash",
        "latestRegistrationDate",
    }
    assert set(drep) >= required_fields
    assert isinstance(drep["isScriptBased"], bool)
    assert isinstance(drep["drepId"], str)
    assert isinstance(drep["view"], str)
    assert_nullable(drep["url"], str)
    assert_nullable(drep["metadataHash"], str)
    assert_int(drep["deposit"])
    assert_nullable(drep["votingPower"], int)
    assert drep["status"] in {"Active", "Inactive", "Retired"}
    assert drep["type"] in {"DRep", "SoleVoter"}
    assert_nullable(drep["latestTxHash"], str)
    assert isinstance(drep["latestRegistrationDate"], str)


def assert_proposal_contract(proposal):
    required_fields = {
        "id",
        "txHash",
        "index",
        "type",
        "details",
        "expiryDate",
        "expiryEpochNo",
        "createdDate",
        "createdEpochNo",
        "url",
        "metadataHash",
        "protocolParams",
        "title",
        "abstract",
        "motivation",
        "rationale",
        "dRepYesVotes",
        "dRepNoVotes",
        "dRepAbstainVotes",
        "ccYesVotes",
        "ccNoVotes",
        "ccAbstainVotes",
        "poolYesVotes",
        "poolNoVotes",
        "poolAbstainVotes",
        "prevGovActionIndex",
        "prevGovActionTxHash",
    }
    assert set(proposal) >= required_fields
    assert isinstance(proposal["id"], str)
    assert isinstance(proposal["txHash"], str)
    assert_int(proposal["index"])
    assert proposal["type"] in {
        "ParameterChange",
        "HardForkInitiation",
        "TreasuryWithdrawals",
        "NoConfidence",
        "NewCommittee",
        "NewConstitution",
        "InfoAction",
    }
    assert_nullable(proposal["expiryDate"], str)
    assert_nullable(proposal["expiryEpochNo"], int)
    assert isinstance(proposal["createdDate"], str)
    assert_int(proposal["createdEpochNo"])
    assert isinstance(proposal["url"], str)
    assert isinstance(proposal["metadataHash"], str)
    assert_nullable(proposal["title"], str)
    assert_nullable(proposal["abstract"], str)
    assert_nullable(proposal["motivation"], str)
    assert_nullable(proposal["rationale"], str)
    assert_int(proposal["dRepYesVotes"])
    assert_int(proposal["dRepNoVotes"])
    assert_int(proposal["dRepAbstainVotes"])
    assert_int(proposal["ccYesVotes"])
    assert_int(proposal["ccNoVotes"])
    assert_int(proposal["ccAbstainVotes"])
    assert_int(proposal["poolYesVotes"])
    assert_int(proposal["poolNoVotes"])
    assert_int(proposal["poolAbstainVotes"])
    assert_nullable(proposal["prevGovActionIndex"], int)
    assert_nullable(proposal["prevGovActionTxHash"], str)


def test_drep_list_pagination_and_status_contract(govtool_api):
    response = govtool_api.drep_list(
        {"page": 0, "pageSize": 2, "status": "Active", "sort": "VotingPower"}
    )
    drep_page = response.json()

    assert_page_contract(drep_page, page=0, page_size=2)
    for drep in drep_page["elements"]:
        assert_drep_contract(drep)
        assert drep["status"] == "Active"


def test_proposal_list_and_get_contract(govtool_api):
    response = govtool_api.proposal_list({"page": 0, "pageSize": 1, "sort": "NewestCreated"})
    proposal_page = response.json()

    assert_page_contract(proposal_page, page=0, page_size=1)
    if not proposal_page["elements"]:
        pytest.skip("No proposals available in this backend environment")

    listed_proposal = proposal_page["elements"][0]
    assert_proposal_contract(listed_proposal)

    proposal_id = f'{listed_proposal["txHash"]}%23{listed_proposal["index"]}'
    get_response = govtool_api.get_proposal(proposal_id).json()

    assert set(get_response) >= {"proposal", "vote"}
    assert_proposal_contract(get_response["proposal"])
    assert get_response["proposal"]["txHash"] == listed_proposal["txHash"]
    assert get_response["proposal"]["index"] == listed_proposal["index"]


def test_drep_voting_power_list_contract(govtool_api):
    drep_id = drep_data[0]["drepId"]
    response = govtool_api.drep_voting_power_list([drep_id])
    voting_power_rows = response.json()

    assert isinstance(voting_power_rows, list)
    assert voting_power_rows
    assert any(row["hashRaw"] == drep_id for row in voting_power_rows)
    for row in voting_power_rows:
        assert set(row) >= {"view", "hashRaw", "votingPower", "givenName"}
        assert isinstance(row["view"], str)
        assert isinstance(row["hashRaw"], str)
        assert_int(row["votingPower"])
        assert_nullable(row["givenName"], str)


def test_account_contract(govtool_api):
    stake_key = ada_holders[0]["stakeKey"]
    response = govtool_api.raw_get("/account", stake_key)

    if response.status_code == 500:
        assert response.json() == {
            "errorType": "CriticalError",
            "message": "Could not query the account info.",
        }
        return

    assert 200 <= response.status_code <= 299
    account = response.json()

    assert set(account) >= {"id", "view", "isRegistered", "isScriptBased"}
    assert_int(account["id"])
    assert isinstance(account["view"], str)
    assert isinstance(account["isRegistered"], bool)
    assert isinstance(account["isScriptBased"], bool)


def test_enacted_details_contract(govtool_api):
    response = govtool_api.proposal_enacted_details("HardForkInitiation")
    enacted_details = response.json()

    if enacted_details is None:
        return

    assert set(enacted_details) >= {"id", "txId", "index", "description", "hash"}
    assert_int(enacted_details["id"])
    assert_int(enacted_details["txId"])
    assert_int(enacted_details["index"])
    assert_nullable(enacted_details["description"], dict)
    assert isinstance(enacted_details["hash"], str)


def test_invalid_hex_and_unknown_route_errors(govtool_api):
    invalid_hex = govtool_api.raw_get("/drep/get-voting-power/not-a-hex-value")
    assert invalid_hex.status_code == 400

    missing_route = govtool_api.raw_get("/__missing_backend_contract_route__")
    assert missing_route.status_code == 404
