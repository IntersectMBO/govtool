import os
import re

import pytest
import requests


CACHE_CONTROL = "public, max-age=31536000, immutable"


def assert_not_immutable(response):
    assert "immutable" not in response.headers.get("Cache-Control", "").lower()


@pytest.fixture(scope="session")
def survey_reference():
    tx_id = os.environ.get("SURVEY_TX_ID", "")
    reference_url = os.environ.get("REFERENCE_BASE_URL", "").rstrip("/")

    if not re.fullmatch(r"[0-9a-fA-F]{64}", tx_id):
        pytest.fail("Set SURVEY_TX_ID to a real survey-definition transaction hash")

    if not reference_url:
        pytest.fail("Set REFERENCE_BASE_URL to the Haskell backend API URL")

    tx_id = tx_id.lower()
    response = requests.get(
        f"{reference_url}/survey/definition/{tx_id}/0",
        timeout=30,
    )

    assert response.status_code == 200, response.text
    data = response.json()

    assert data["txId"] == tx_id
    assert data["surveyIndex"] == 0
    assert data["metadataLabel"] == 17
    assert re.fullmatch(r"(?:[0-9a-f]{2})+", data["payloadCborHex"])

    return data


@pytest.mark.parametrize("index", [0, 1, 65535])
def test_survey_matches_haskell(govtool_api, survey_reference, index):
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