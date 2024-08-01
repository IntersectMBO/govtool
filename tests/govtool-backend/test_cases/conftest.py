import json
import os
import sys
import re

# import the fixtures.
from test_cases.fixtures import *
import pytest
import requests

from models.TestResult import Metrics

from lib.govtool_api import GovToolApi

from config import CURRENT_GIT_HASH
from config import BUILD_ID
from config import METRICS_API_SECRET


@pytest.fixture(scope="session")
def govtool_api():
    base_url: (str| None) = os.environ.get("BASE_URL")
    metrics_url: (str| None) = os.environ.get("METRICS_URL")

    if base_url is not None:
        base_url = base_url[:-1] if base_url.endswith("/") else base_url
        print(f"BASE_URL: {base_url}")
    else:
        print("BASE_URL environment variable is not set.", file=sys.stderr)
        sys.exit(1)

    if metrics_url is not None:
        metrics_url = metrics_url[:-1] if metrics_url.endswith("/") else metrics_url
        print(f"METRICS_URL: {metrics_url}")
    else:
        print("METRICS_URL environment variable is not set.", file=sys.stderr)
        print("Proceeding without METRICS_URL: Metrics will not be posted")

    api = GovToolApi(base_url)
    yield api

    if metrics_url:
        endpoint_record_url = metrics_url + "/metrics/api-endpoints"
        test_record_url = metrics_url + "/metrics/test-results"
        print()
        print("Uploading API endpoint metrics ...")
        for request_log in api.requests_log:
            response = requests.post(
                url=endpoint_record_url, data=request_log, headers={"secret-token": METRICS_API_SECRET}
            )
            if response.status_code != 200:
                print(response.json())
                print(
                    "Error Uploading API metrics:[ statuscode=",
                    response.status_code,
                    "]",
                    "endpoint=" + request_log["endpoint"],
                    "duration=" + str(request_log["response_time"] / 1000) + "ms",
                )

        print("Uploading Test results ...")
        for test_log in api.tests_log:
            response = requests.post(url=test_record_url, data=test_log, headers={"secret-token": METRICS_API_SECRET})
            if response.status_code != 200:
                print(
                    "Error Uploading Test result:[ statuscode=",
                    response.status_code,
                    "]",
                    "test=" + test_log["test_name"],
                    "result=" + test_log["outcome"],
                )


@pytest.hookimpl(wrapper=True, tryfirst=True)
def pytest_runtest_makereport(item):
    rep = yield
    govtool_api_object = item.funcargs.get("govtool_api")

    if rep.when == "call":

        test_func_name = re.search(r"(?<=::)(.*?)*(?=\[|$)", rep.nodeid).group()

        govtool_api_object.add_test_metrics(
            Metrics(
                outcome=rep.outcome,
                test_name=test_func_name,
                build_id=BUILD_ID,
                commit_hash=CURRENT_GIT_HASH,
                start_date=int(rep.start * 1000000),
                end_date=int(rep.stop * 1000000),
            )
        )
    return rep
