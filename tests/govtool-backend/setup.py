import math
import sys
import json
import time
from lib.faucet_api import CardanoFaucet
from lib.kuber_api import KuberApi

kuber_api = KuberApi.from_env()

# check fund for the main wallet
main_wallet = {
    "skey": {
        "type": "PaymentSigningKeyShelley_ed25519",
        "description": "Payment Signing Key",
        "cborHex": "58200a2e6f6f040636a56b61276487459b9e9f309d09d1d57319b41ed32f3053af8d",
    },
    "address": "addr_test1qzc97zml9xzhm7xcsmqretkk7ztzyehj3dpd7ph7h0s40wp0ea9f3e353pmmr7yxv7m2dj09rn44m7pvd0m4cylusn8szlm75t",
}
drep_wallets = [
    {
        "pay-skey": {
            "type": "PaymentSigningKeyShelley_ed25519",
            "description": "Payment Signing Key",
            "cborHex": "58207da324397a403f89972ba63f2853c6c6043fd96dac3bdcc452f27c9ad5c75c83",
        },
        "address": "addr_test1qzh73vyy0mtu5xfahdswmaclzcs9lrm8hsvq0n799ufhp53htvec6kdtxqls04v5ldacx342v5rsflxlep93s6t5k2hs70m6n2",
        "stake-skey": {
            "type": "StakeSigningKeyShelley_ed25519",
            "description": "Stake Signing Key",
            "cborHex": "582036742f9246e355e75318894cb31f7058510f827c6820f40f56cce9bbdab8ef08",
        },
        "drep-id": "drep1xadn8r2e4vcr7p74jnahhq6x4fjswp8umlyykxrfwje2707cqh9",
        "stake-vkey": "375b338d59ab303f07d594fb7b8346aa650704fcdfc84b186974b2af",
        "url": "https://bit.ly/3zCH2HL",
        "data_hash": "1111111111111111111111111111111111111111111111111111111111111111",
    },
    {
        "pay-skey": {
            "type": "PaymentSigningKeyShelley_ed25519",
            "description": "Payment Signing Key",
            "cborHex": "58205db2e13ca102a6bcfea2d4651d24559ee933ab6c355796307cace0bd23584b17",
        },
        "address": "addr_test1qqu3ny5xjfhg9hqg3yfdf9arftg20dv92u3r8hkc94833xlv4tvazt5672duf338dx5zf0stl05zgc8g08qy0asathfs8fewtx",
        "stake-skey": {
            "type": "StakeSigningKeyShelley_ed25519",
            "description": "Stake Signing Key",
            "cborHex": "582042ab191b40e5b1364beaa4b0d27fea48156d89c92ba749b738bf7891e27fbb6a",
        },
        "drep-id": "drep1aj4dn5fwntefh3xxya56sf97p0a7sfrqapuuq3lkr4waxeelmwd",
        "stake-vkey": "ecaad9d12e9af29bc4c62769a824be0bfbe82460e879c047f61d5dd3",
        "url": "https://bit.ly/3zCH2HL",
        "data_hash": "1111111111111111111111111111111111111111111111111111111111111111",
    },
]
ada_wallets = [
    {
        "address": "addr_test1qrqwl94r7zhxqwq8n26p6ql9dzylmzupln8vwaake9njg6wlrxfdmq43utplzwyuaqq8q8xyjvqdul88rda02l95lm9qpauf3k",
        "pay-skey": {
            "type": "PaymentSigningKeyShelley_ed25519",
            "description": "Payment Signing Key",
            "cborHex": "5820c5b5ad023d8eb7ddc67b271d79705522b65740b9c249e205e39fa30dec775deb",
        },
        "stake-skey": {
            "type": "PaymentSigningKeyShelley_ed25519",
            "description": "Stake Signing Key",
            "cborHex": "5820ea031c372c0617cf7137e7cfbfb821d63e61aa3277af993f84d2b4cdb9199dd6",
        },
        "drep-id": "drep1muve9hvzk83v8ufcnn5qququcjfsphnuuudh4atuknlv5kh84lc",
        "stake-vkey": "df1992dd82b1e2c3f1389ce800701cc49300de7ce71b7af57cb4feca",
    },
    {
        "pay-skey": {
            "type": "PaymentSigningKeyShelley_ed25519",
            "description": "Payment Signing Key",
            "cborHex": "5820e6b1bac201091179f8ef213b2dc0f63c72b23de45bc26a7b68eccdc718f65c83",
        },
        "address": "addr_test1qrz8rz38rv37cx4hsgsneavsx4e84ppupwysxp6xp6mv6c9nny8znayz56vw8rfyt0gyyftg6pt5umr9njeey8fjekhqwkrrew",
        "stake-skey": {
            "type": "PaymentSigningKeyShelley_ed25519",
            "description": "Stake Signing Key",
            "cborHex": "5820826d043e62e04259ffb24c24994e8c8ffd2272eda6e8a610a65977c233077b6d",
        },
        "drep-id": "drep1kwvsu205s2nf3cudy3daqs39drg9wnnvvkwt8ysaxtx6up8cy06",
        "stake-vkey": "b3990e29f482a698e38d245bd0422568d0574e6c659cb3921d32cdae",
    },
]


# Create transaction using the main wallet to register 2 out of 4 wallets as drep,
# register other 2 wallets' stake keys create one proposal
def main():
    kuber_json = {
        "selections": [
            main_wallet["address"],
            main_wallet["skey"],
            ada_wallets[0]["stake-skey"],
            ada_wallets[0]["pay-skey"],
            ada_wallets[1]["stake-skey"],
            ada_wallets[1]["pay-skey"],
            {
                "type": "PaymentSigningKeyShelley_ed25519",
                "description": "Payment Signing Key",
                "cborHex": drep_wallets[0]["stake-skey"]["cborHex"],
            },
            {
                "type": "PaymentSigningKeyShelley_ed25519",
                "description": "Payment Signing Key",
                "cborHex": drep_wallets[1]["stake-skey"]["cborHex"],
            },
        ],
        "certificates": [
            {
                "type": "registerdrep",
                "key": drep_wallets[0]["drep-id"],
                "anchor": {
                    "url": drep_wallets[0]["url"],
                    "dataHash": drep_wallets[0]["data_hash"],
                },
            },
            {"type": "registerstake", "key": ada_wallets[0]["stake-vkey"]},
            {"type": "registerstake", "key": ada_wallets[1]["stake-vkey"]},
            {
                "type": "registerdrep",
                "key": drep_wallets[1]["drep-id"],
                "anchor": {
                    "url": drep_wallets[1]["url"],
                    "dataHash": drep_wallets[1]["data_hash"],
                },
            },
        ],
        "proposals": [
            {
                "refundAccount": {
                    "network": "Testnet",
                    "credential": {"key hash": ada_wallets[0]["stake-vkey"]},
                },
                "anchor": {
                    "url": "https://bit.ly/3zCH2HL",
                    "dataHash": "1111111111111111111111111111111111111111111111111111111111111111",
                },
                "noconfidence": True,
            }
        ],
    }

    protocol_params = kuber_api.get_protocol_params()
    total_locked = (
        protocol_params["dRepDeposit"] * 2
        + protocol_params["stakeAddressDeposit"] * 2
        + protocol_params["govActionDeposit"]
    )

    # To make sure the balance is available with some extra change
    min_required_balance = total_locked + 5 * 1000000
    load_balance(main_wallet["address"], min_required_balance)

    print(json.dumps(kuber_json, indent=2))
    print("Submitting the above registration transaction..")
    response = kuber_api.build_tx(kuber_json, submit=True)

    if response.status_code == 200:
        print("Transaction submitted", response.text)
        data = response.json()
        kuber_api.wait_for_txout(data["hash"] + "#0", log=True)
    else:
        print("Server Replied with Error [ StatusCode=", response.status_code, "]", response.reason, response.text)
        if ("DRepAlreadyRegistered" in response.text) or ("StakeKeyRegisteredDELEG" in response.text):
            print("-----")
            print("This probably means that you have already run the setup script.")
            print("-----")
            sys.exit(0)
        sys.exit(1)

    # vote from one of the dreps to the proposal
    kuber_json = {
        "selections": [
            main_wallet["address"],
            main_wallet["skey"],
            ada_wallets[0]["stake-skey"],
            ada_wallets[1]["stake-skey"],
        ],
        "certificates": [
            {
                "type": "delegate",
                "key": ada_wallets[0]["stake-vkey"],
                "drep": drep_wallets[0]["drep-id"],
            },
            {
                "type": "delegate",
                "key": ada_wallets[1]["stake-vkey"],
                "drep": drep_wallets[1]["drep-id"],
            },
        ],
    }
    print(json.dumps(kuber_json, indent=2))
    response = kuber_api.build_tx(kuber_json, submit=True)
    if response.status_code == 200:
        print("Transaction submitted", response.text)
        data = response.json()
        kuber_api.wait_for_txout(data["hash"] + "#0", log=True)
    else:
        if "AlreadyRegistered" in response.text:
            print("Server Replied with Error [ StatusCode=", response.status_code, "]", response.reason, response.text)
            print("")
        sys.exit(1)


def load_balance(address, required_balance):
    balance = kuber_api.get_balance(address)
    print("Current Balance of Bootstrap Wallet: {} Ada".format(balance / 1e6))
    print("Missing balance : " + str((required_balance - balance) / 1e6) + " Ada")
    faucet = CardanoFaucet.from_env()

    while balance < required_balance:
        print("========== Requesting Faucet ==========")
        result = faucet.send_money(address)
        start_time = time.time()
        if "error" in result:
            error = result["error"]
            if "tag" in error and error["tag"] == "FaucetWebErrorRateLimitExeeeded":
                if "contents" in error and isinstance(error["contents"], list) and len(error["contents"]) > 0:
                    waitTime = error["contents"][0]
                    if isinstance(waitTime, (int, float)):
                        wait_secs = math.ceil(waitTime) + 5
                        print(result)
                        print("Faucet Ratelimit: Waiting for " + str(wait_secs) + " secs")
                        time.sleep(waitTime)
                        continue
            print(result)
            raise Exception("Failed to load balance from faucet")
        kuber_api.wait_for_txout(result["txid"] + "#0", log=True)
        balance = kuber_api.get_balance(address)
        print("New balance is  : " + str(balance / 1e6) + " Ada")
        if balance < required_balance:
            print("Missing balance : " + str((required_balance - balance) / 1e6) + " Ada")

            print("Waiting for faucet rate limit of 60s ..")
            time_diff = 65 - (time.time() - start_time)
            if time_diff > 0:
                time.sleep(time_diff)

main()
