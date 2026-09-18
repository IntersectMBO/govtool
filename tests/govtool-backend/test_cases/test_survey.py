import os
import re

import pytest


# This shared suite also runs against Haskell deployments, which have no survey API.
pytestmark = pytest.mark.skipif(
    os.environ.get("RUN_SURVEY_TESTS") != "1",
    reason="Set RUN_SURVEY_TESTS=1 when targeting the TypeScript backend",
)

CACHE_CONTROL = "public, max-age=31536000, immutable"


def assert_not_immutable(response):
    assert "immutable" not in response.headers.get("Cache-Control", "").lower()


@pytest.fixture(scope="session")
def survey_reference():
    tx_id = os.environ.get("SURVEY_TX_ID", "")
    payload = os.environ.get("SURVEY_PAYLOAD_CBOR_HEX", "")
    if not tx_id and not payload:
        pytest.skip("Set SURVEY_TX_ID and independently verified SURVEY_PAYLOAD_CBOR_HEX")
    if not re.fullmatch(r"[0-9a-fA-F]{64}", tx_id):
        pytest.fail("SURVEY_TX_ID must be a real survey-definition transaction hash")
    if not re.fullmatch(r"(?:[0-9a-fA-F]{2})+", payload):
        pytest.fail("SURVEY_PAYLOAD_CBOR_HEX must contain the independently verified payload bytes")
    return {"txId": tx_id.lower(), "surveyIndex": 0, "metadataLabel": 17, "payloadCborHex": payload.lower()}


@pytest.mark.parametrize("index", [0, 1, 65535])
def test_survey_matches_expected_payload(govtool_api, survey_reference, index):
    tx_id = survey_reference["txId"]
    response = govtool_api.raw_get(f"/survey/definition/{tx_id}/{index}")

    assert response.status_code == 200, response.text
    assert response.json() == {
        **survey_reference,
        "surveyIndex": index,
    }

    data = response.json()
    assert type(data["surveyIndex"]) is int
    assert type(data["metadataLabel"]) is int
    assert response.headers.get("Cache-Control") == CACHE_CONTROL


def test_uppercase_survey_hash(govtool_api, survey_reference):
    tx_id = survey_reference["txId"].upper()
    response = govtool_api.raw_get(f"/survey/definition/{tx_id}/0")

    assert response.status_code == 200, response.text
    assert response.json() == survey_reference


@pytest.mark.parametrize(
    "tx_id",
    [
        "not-a-hex-value",
        "ab" * 31,
        "ab" * 33,
        "g" * 64,
        "a" * 63,
    ],
)
def test_invalid_survey_hash(govtool_api, tx_id):
    response = govtool_api.raw_get(f"/survey/definition/{tx_id}/0")

    assert response.status_code == 400, response.text
    assert response.json()["errorType"] == "ValidationError"
    assert_not_immutable(response)


@pytest.mark.parametrize(
    "index",
    ["-1", "1.5", "65536", "9007199254740993", "abc", "1e2"],
)
def test_invalid_survey_index(govtool_api, index):
    response = govtool_api.raw_get(
        f"/survey/definition/{'ab' * 32}/{index}"
    )

    assert response.status_code == 400, response.text
    assert response.json()["errorType"] == "ValidationError"
    assert_not_immutable(response)


def test_missing_survey_metadata(govtool_api):
    tx_id = "00" * 32
    response = govtool_api.raw_get(f"/survey/definition/{tx_id}/0")

    assert response.status_code == 404, response.text
    assert response.json() == {
        "errorType": "NotFoundError",
        "message": f"No metadata label 17 found for transaction {tx_id}",
    }
    assert_not_immutable(response)