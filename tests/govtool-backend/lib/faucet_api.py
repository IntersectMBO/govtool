from config import FAUCET_API_URL,FACUET_API_KEY
import os
from typing import TypedDict

import requests


class FaucetAmount(TypedDict):
    lovelace: int


class Transaction(TypedDict):
    amount: FaucetAmount
    txid: str
    txin: str


class CardanoFaucet:
    def __init__(self, api_key: str, base_url: str = "https://faucet.sanchonet.world.dev.cardano.org"):
        self.api_key = api_key
        self.base_url = base_url

    @staticmethod
    def from_env():
        api_key = FACUET_API_KEY
        base_url = FAUCET_API_URL
        if not api_key:
            raise ValueError("FAUCET_API_KEY environment variable not set. Please set this variable or fund missing balance to the wallet address: addr_test1qzc97zml9xzhm7xcsmqretkk7ztzyehj3dpd7ph7h0s40wp0ea9f3e353pmmr7yxv7m2dj09rn44m7pvd0m4cylusn8szlm75t")
        return CardanoFaucet(api_key, base_url)

    def send_money(self, address: str, tx_type: str = "default") -> Transaction:
        endpoint = f"{self.base_url}/send-money"
        params = {"address": address, "api_key": self.api_key, "type": tx_type}
        response = requests.get(endpoint, params=params)

        if response.status_code == 200:
            return response.json()
        else:
            response.raise_for_status()


""
