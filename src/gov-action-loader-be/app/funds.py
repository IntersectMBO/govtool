import asyncio

from fastapi import HTTPException

from app.settings import settings
from app.transaction import (default_proposal_deposit_ada, main_wallet,
                             submit_tx)


async def get_ada_balance(address, client):
    utxo_url = settings.kuber_api_url + "/api/v3/utxo"
    kuber_response = await client.get(
        utxo_url,
        params={"address": address},
        headers={"api-key": settings.kuber_api_key},
    )
    if kuber_response.status_code == 200:
        utxos = kuber_response.json()
        total_ada = sum(utxo["value"]["lovelace"] for utxo in utxos)
        return {"address": address, "totalValue": total_ada / 1000000}
    else:
        print(utxo_url, kuber_response.text)
        raise HTTPException(
            status_code=kuber_response.status_code, detail=kuber_response.text
        )


async def get_protocol_params(client):
    pParamsQuery = settings.kuber_api_url + "/api/v3/protocol-params"
    kuber_response = await client.get(
        pParamsQuery,
        headers={"api-key": settings.kuber_api_key},
    )
    if kuber_response.status_code == 200:
        return kuber_response.json()
    else:
        print(pParamsQuery, kuber_response.text)
        raise HTTPException(
            status_code=kuber_response.status_code, detail=kuber_response.text
        )


async def check_balance_and_fund_wallets(
    wallets, supported_proposals_in_single_tx, per_proposal_deposit, client
):
    wallets_with_balance = await asyncio.gather(
        *[get_ada_balance(wallet["address"], client) for wallet in wallets]
    )
    total_ada_require_for_proposal = supported_proposals_in_single_tx * (
        per_proposal_deposit + 150000
    )
    low_balance_wallets = [
        wallet
        for wallet in wallets_with_balance
        if (wallet["totalValue"] * 1000000) < total_ada_require_for_proposal
    ]

    if low_balance_wallets:
        print("Wallets are on low fund.Funding...")

        for wallet in low_balance_wallets:
            print(
                "-    from: " + main_wallet["address"],
                "       to: " + wallet["address"],
                "  balance: " + str(round(wallet["totalValue"] * 1000000)),
                "  sending: "
                + str(
                    round(
                        total_ada_require_for_proposal
                        - (wallet["totalValue"] * 1000000)
                    )
                ),
                sep="\n",
            )
        fund_from_main_tx = {
            "selections": [main_wallet["address"], main_wallet["skey"]],
            "outputs": [
                {
                    "address": wallet["address"],
                    "value": f"{ round(total_ada_require_for_proposal-(wallet['totalValue'] * 1000000))}",
                }
                for wallet in low_balance_wallets
            ],
        }
        tx = await submit_tx(fund_from_main_tx, client)
        raise HTTPException(
            status_code=412,
            detail="This action required multiple transaction and wallet setup  TX:"
            + tx
            + " has been submitted. Pls wait until the transaction gets confirmed.",
        )

    else:
        print("Wallets are fully funded.")
