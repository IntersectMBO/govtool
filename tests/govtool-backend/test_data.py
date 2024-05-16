import os
import random
import json
from typing import List
from models.TestData import Drep, AdaHolder

file_path = "test_data.json"
alternative_file_path = "../test_data.json"

if os.path.exists(file_path):
    with open(file_path, "r") as file:
        data = json.load(file)
elif os.path.exists(alternative_file_path):
    with open(alternative_file_path, "r") as file:
        data = json.load(file)
else:
    raise FileNotFoundError(f"Neither '{file_path}' nor '{alternative_file_path}' could be found.")

drep_data = list(
    map(
        lambda drep_wallet: {
            "drepId": drep_wallet["stake-vkey"],
            "url": drep_wallet["url"],
            "metadataHash": drep_wallet["data_hash"],
        },
        data["drep_wallets"],
    )
)
ada_holders = list(
    map(
        lambda wallets: {"drepId": wallets[0]["stake-vkey"], "stakeKey": wallets[1]["stake-vkey"]},
        list(zip(data["drep_wallets"], data["ada_holder_wallets"])),
    )
)
