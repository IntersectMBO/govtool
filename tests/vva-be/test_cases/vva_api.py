import json
import time
from typing import Any

import requests
from requests import Response

from models.TestResult import Metrics
from config import BUILD_ID


class VVAApi():

    def __init__(self, base_url: str):
        self._base_url = base_url
        self._session = requests.Session()
        self._session.headers.update({"Accept": "application/json", "content-type": "application/json"})
        self.requests_log = []
        self.tests_log = []

    def __request(self, method: str, endpoint: str, param: Any | None = None,
                  body: Any | None = None) -> Response:
        endpoint = endpoint if endpoint.startswith('/') else '/' + endpoint
        full_url = self._base_url + endpoint
        full_url = full_url + "/" + param if param else full_url
        start_time = int(time.time()*1000000)

        response = self._session.request(method, full_url, json=body)

        end_time = int(time.time()*1000000)
        response_time = end_time - start_time

        try:
            response_json = json.dumps(response.json())
            response_json_str = response_json[:200]
        except:
            response_json_str = "Something went wrong"
        
        request_info = {
            "method": method,
            "endpoint": endpoint,
            "path_param": param,
            "json": json.dumps(body),
            "status_code": response.status_code,
            "response_json": response_json_str,
            "response_time": response_time,
            "start_date": int(start_time),
            "build_id": BUILD_ID
        }

        self.requests_log.append(request_info)

        assert 200 >= response.status_code <= 299, f"Expected {method}{endpoint} to succeed but got statusCode:{response.status_code} : body:{response.text}"
        return response

    def __get(self, endpoint: str, param: str | None = None) -> Response:
        return self.__request('GET', endpoint, param)

    def drep_list(self) -> Response:
        return self.__get('/drep/list')

    def drep_getVotes(self, drep_id) -> Response:
        return self.__get('/drep/getVotes', drep_id)

    def drep_get_voting_power(self, drep_id) -> Response:
        return self.__get('/drep/get-voting-power', drep_id)

    def proposal_list(self) -> Response:
        return self.__get('/proposal/list')

    def ada_holder_get_current_delegation(self, stake_key: str) -> Response:
        return self.__get('/ada-holder/get-current-delegation', stake_key)

    def ada_holder_get_voting_power(self, stake_key) -> Response:
        return self.__get('/ada-holder/get-voting-power', stake_key)
  
    def add_test_metrics(self, metrics: Metrics):
        self.tests_log.append(metrics)
