import random
import json
from typing import List
from models.TestData import Drep, AdaHolder

with open("test_data.json", "r") as file:
    data = json.load(file)

drep_data = list(map(lambda drep_wallet: {"drepId":drep_wallet["stake-vkey"], "url":drep_wallet["url"], "metadataHash":drep_wallet["data_hash"]}  ,data["drep_wallets"]))
ada_holders = list(map(lambda wallets: {"drepId":wallets[0]["stake-vkey"], "stakeKey":wallets[1]["stake-vkey"]}  ,list(zip(data["drep_wallets"], data["ada_holder_wallets"]))))
