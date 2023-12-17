import json
import os
import subprocess


def create_wallets(no_of_wallets):
    try:
        with open("wallets.json", "r") as file:
            wallets = json.load(file)
    except FileNotFoundError:
        wallets = []

    print("No of wallets to create", no_of_wallets)
    print("No of wallets in wallets.json", len(wallets))

    while len(wallets) < no_of_wallets:
        pay_wallet_command = """
        cardano-cli address key-gen \\
        --signing-key-file payment.skey \\
        --verification-key-file payment.vkey
            """
        subprocess.call(["bash", "-c", pay_wallet_command])
        stake_wallet_command = """
        cardano-cli stake-address key-gen \\
        --signing-key-file stake.skey \\
        --verification-key-file stake.vkey
            """
        subprocess.call(["bash", "-c", stake_wallet_command])
        address_command = """
        cardano-cli address build \\
        --payment-verification-key-file payment.vkey \\
        --stake-verification-key-file stake.vkey \\
        --out-file payment.addr \\
        --testnet-magic 4
            """
        subprocess.call(["bash", "-c", address_command])

        wallet = {}
        with open("payment.skey", "r") as file:
            wallet["skey"] = json.load(file)
        with open("payment.addr", "r") as file:
            wallet["address"] = file.read()
        wallets.append(wallet)
        os.remove("payment.skey")
        os.remove("payment.vkey")
        os.remove("stake.skey")
        os.remove("stake.vkey")
        os.remove("payment.addr")

    with open("wallets.json", "w") as file:
        json.dump(wallets, file)


create_wallets(100)
