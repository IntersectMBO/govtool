import collections.abc
import json
import os
import random
import secrets
import string
import subprocess
import time
from typing import Any, Dict

from fastapi import HTTPException

from app.network import get_api_url
from app.settings import settings

main_wallet = {
    "skey": {
        "type": "PaymentSigningKeyShelley_ed25519",
        "description": "Payment Signing Key",
        "cborHex": "5820ab863c2e6c2e0837d1929b872c43dbe485c326a29527bf267da4cde498731f02",
    },
    "address": "addr_test1qrd3hs7rlxwwdzthe6hj026dmyt3y0heuulctscyydh2kguh4xfmpjqkd25vfq69hcvj27jqyk4hvnyxu7vma2c4kvps8eh2m3",
}

default_proposal_deposit_ada = 0


def get_base_proposal():
    return {
        "refundAccount": {
            "network": "Testnet",
            "credential": {
                "key hash": "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23"
            },
        },
    }


def get_base_proposal_for_multiple():
    base_proposal = get_base_proposal()
    base_proposal["anchor"] = {
        "url": "http://bit.ly/3QFMhii",
        "dataHash": "1111111111111111111111111111111111111111111111111111111111111112",
    }
    base_proposal["deposit"] = default_proposal_deposit_ada * 1000000
    return base_proposal


def get_default_transaction():
    return {
        "selections": [main_wallet["address"], main_wallet["skey"]],
        "proposals": [get_base_proposal()],
    }


def get_random_url(base_url, path_length=None):
    characters = string.ascii_letters + string.digits
    path_length = path_length if path_length else 5
    random_path = "".join(secrets.choice(characters) for _ in range(path_length))
    return f"{base_url} / {random_path}"


def generate_raw_address():
    return "e0" + secrets.token_hex(28)


def generate_bytes(length):
    return secrets.token_hex(length)


def generate_withdraw(number):
    stake_addresses = [generate_raw_address() for _ in range(number)]
    amounts = [
        random.choice([10000000, 20000000, 30000000, 40000000, 50000000])
        for _ in range(number)
    ]
    return dict(zip(stake_addresses, amounts))


def generate_update(number):
    stake_addresses = [generate_raw_address() for _ in range(number)]
    current_epoch = int((int(time.time()) - 1506635071) / (5 * 24 * 60 * 60))
    maximum_epoch = 10000
    epochs = [random.randint(current_epoch + 2, maximum_epoch) for _ in range(number)]
    return dict(zip(stake_addresses, epochs))


def generate_quorom():
    numerator = random.randint(1, 10)
    denomintor = random.randint(numerator + 1, 20)
    return {"numerator": numerator, "denominator": denomintor}


def generate_hardfork():
    majorProtocolNum = random.randint(1, 9)
    minorProtocolNum = random.randint(1, 9)
    return {
        "hardfork": {
            "protocolVersion": {"major": majorProtocolNum, "minor": minorProtocolNum}
        }
    }


def change_pp_value(random_parameter):
    if isinstance(random_parameter, dict):
        for k, v in random_parameter.items():
            random_parameter[k] = change_pp_value(v)
    elif isinstance(random_parameter, collections.abc.Sequence):
        # get random index from array and change it
        random_index = random.randint(0, len(random_parameter))
        random_parameter[random_index] += 1
    elif isinstance(random_parameter, int):
        random_parameter += 2
    else:
        random_parameter += 0.01
    return random_parameter


def get_gov_script():
    with open("app/data/gov-script.plutus", "r") as script_file:
        script_str = script_file.read()
        script_json = json.loads(script_str)
        return script_json


def get_proposal_data_from_type(proposal_type, current_pParams):
    match proposal_type:
        case "constitution":
            return {
                "newconstitution": {
                    "url": get_random_url("http://bit.ly"),
                    "dataHash": generate_bytes(32),
                }
            }
        case "withdrawal":
            number_of_addresses = random.randint(1, 5)
            return {
                "script": get_gov_script(),
                "withdraw": generate_withdraw(number_of_addresses),
            }
        case "no-confidence":
            return {"noconfidence": True}
        case "update-committee":
            number_of_addresses = random.randint(1, 5)
            return {
                "updatecommittee": {
                    "add": generate_update(number_of_addresses),
                    "qourum": generate_quorom(),
                }
            }
        case "hardfork":
            return generate_hardfork()
        case "update-parameters":
            # read current protocol parameters from json
            # get one of the keys of the pp
            pParams = json.loads(json.dumps(current_pParams))
            keys = pParams.keys()
            rand_key = random.choice(filter_updatable_paramKeys(list(keys)))
            # recurse into the innermost element of that key and change it by +-1, all protocol parameter value is either float or int
            return {
                "script": get_gov_script(),
                "parameterupdate": {rand_key: change_pp_value(pParams[rand_key])},
            }
        # added_proposal =
        case "info":
            return {}
        case _:
            raise HTTPException(
                400,
                "Unsupported propsal type. Supported types are : "
                "constitution, info, withdrawal, no-confidence, "
                "update-committee, hardfork, update-parameters",
            )


def filter_updatable_paramKeys(keys):
    updatable_keys = {
        "maxBlockSize",
        "maxBBSize",
        "maxTxSize",
        "maxBHSize",
        "keyDeposit",
        "poolDeposit",
        "eMax",
        "nOpt",
        "a0",
        "rho",
        "tau",
        "minPoolCost",
        "coinsPerUTxOByte",
        "costModels",
        "prices",
        "maxTxExUnits",
        "maxBlockExUnits",
        "maxValSize",
        "collateralPercentage",
        "maxCollateralInputs",
        "poolVotingThresholds",
        "dRepVotingThresholds",
        "committeeMinSize",
        "committeeMaxTermLength",
        "govActionLifetime",
        "govActionDeposit",
        "dRepDeposit",
        "dRepActivity",
    }
    return [x for x in keys if x in updatable_keys]


async def submit_tx(tx, client, network, submit=True):
    submit_query = "?submit=true" if submit else ""
    tx_url = get_api_url(network) + "/api/v1/tx" + submit_query
    response = await client.post(
        tx_url,
        json=tx,
        headers={"api-key": settings.kuber_api_key},
    )
    if response.status_code == 200:
        tx = response.json()
        print("Transaction suceessfully submitted", tx["hash"])
        tx_id = tx["hash"]
        return tx_id
    else:
        print("Transaction submit error")
        print(response.text)
        raise HTTPException(status_code=response.status_code, detail=response.text)


async def submit_proposal_tx(wallet, proposal, proposal_numbers, client, network):
    proposals = [
        {
            **proposal,
            "anchor": {
                **proposal["anchor"],
                "url": proposal["anchor"]["url"] + f"?index={i}",
            },
        }
        for i in range(proposal_numbers)
    ]
    tx = {
        "selections": [wallet["address"], wallet["skey"]],
        "proposals": proposals,
    }
    return await submit_tx(tx, client, network)
