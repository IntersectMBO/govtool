import json
import sys
import time
from typing import Any, Optional

import requests
from requests import Response

from models.TestResult import Metrics
from config import BUILD_ID
import os


class KuberApi:

    def __init__(self, base_url: str, api_key: Optional[str]):
        self._base_url = base_url
        self._session = requests.Session()
        headers = {"Accept": "application/json", "content-type": "application/json"}
        if api_key:
            headers["api-key"] = api_key

        self._session.headers.update(headers)
        self.requests_log = []
        self.tests_log = []

    def __request(self, method: str, endpoint: str, param: Any | None = None, body: Any | None = None) -> Response:
        endpoint = endpoint if endpoint.startswith("/") else "/" + endpoint
        full_url = self._base_url + endpoint
        full_url = full_url + "/" + param if param else full_url

        return self._session.request(method, full_url, json=body)

    def __get(self, endpoint: str, param: str | None = None) -> Response:
        return self.__request("GET", endpoint, param)

    def __post(self, endpoint: str, param: str | None = None, body=None) -> Response:
        return self.__request("POST", endpoint, param, body)

    def build_tx(self, tx_builer: dict, submit: bool = False) -> Response:
        return self.__post("/api/v1/tx" + ("?submit=true" if submit else ""), body=tx_builer)

    def get_utxos(self, address: str):
        return self.__get("/api/v3/utxo?address=" + address)

    def get_balance(self, address: str) -> int:
        utxos = self.__get("/api/v3/utxo?address=" + address)
        utxos_json = utxos.json()
        lovelaces = [utxo["value"]["lovelace"] for utxo in utxos_json]
        return sum(lovelaces)

    def get_protocol_params(self):
        return self.__get("api/v3/protocol-params").json()

    def wait_for_txout(self, txin: str, timeout=400, log: bool = False):
        end = time.time() + timeout
        while time.time() < end:
            result = self.__get("/api/v3/utxo?txin=" + txin.replace("#", "%23")).json()
            if len(result) > 0:
                return True
            if log:
                print("Waiting for transaction confirmation  : " + txin + "  ...")
            time.sleep(5)  # wait

        raise TimeoutError("Timed out waiting for transaction confirmation " + txin)

    @staticmethod
    def from_env() -> "KuberApi":
        api_url = os.environ.get("KUBER_API_URL")
        if api_url is not None:
            api_url = api_url[:-1] if api_url.endswith("/") else api_url
            print(f"KUBER_API_URL: {api_url}")
            api_key = os.environ.get("KUBER_API_KEY")
            return KuberApi(api_url, api_key)
        else:
            print("KUBER_API_URL environment variable is not set.", file=sys.stderr)
            raise ValueError("KUBER_API_URL environment variable is not")
