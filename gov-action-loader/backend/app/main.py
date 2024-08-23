import asyncio
import json
import math
from typing import Any, Dict

import httpx
from fastapi import Depends, FastAPI, HTTPException

from app.cors import add_cors
from app.funds import (check_balance_and_fund_wallets, get_ada_balance,
                       get_protocol_params)
from app.http_utils import get_client
from app.models import MultipleProposal
from app.settings import settings
from app.transaction import (get_base_proposal_for_multiple,
                             get_default_transaction,
                             get_proposal_data_from_type,
                             main_wallet, submit_proposal_tx,
                             get_gov_script
                             )

app = FastAPI()
add_cors(app)


@app.post("/api/load/multiple")
async def submit_multiple_proposals(
    multi_proposal: MultipleProposal,
    client: httpx.AsyncClient = Depends(get_client),
):
    required_proposals = multi_proposal.no_of_proposals
    base_proposal = get_base_proposal_for_multiple()

    supported_proposals_in_single_tx = 50
    maximum_supported_proposals = 10000
    pparams = await get_protocol_params(client)

    if required_proposals <= supported_proposals_in_single_tx:
        tx = await submit_proposal_tx(
            main_wallet,
            base_proposal
            | get_proposal_data_from_type(multi_proposal.proposal_type, pparams),
            required_proposals,
            client,
        )
        return [{"proposal_count": required_proposals, "tx_hash": tx}]
    elif required_proposals <= maximum_supported_proposals:
        per_proposal_deposit = pparams["govActionDeposit"]
        required_wallets_number = math.ceil(
            required_proposals / supported_proposals_in_single_tx
        )
        required_wallets = []
        with open("wallets.json", "r") as file:
            wallets = json.load(file)
            required_wallets = wallets[:required_wallets_number]
        availableBalance = (await get_ada_balance(main_wallet["address"], client))[
            "totalValue"
        ]
        required_total_deposit = (
            required_proposals * (per_proposal_deposit + 150000)
        ) / 1000000

        if availableBalance < required_total_deposit:
            raise HTTPException(
                status_code=400,
                detail=str(required_proposals)
                + " proposals will require about "
                + str(round(required_total_deposit))
                + " Ada, But master wallet has only "
                + str(round(availableBalance))
                + " Ada",
            )
        await check_balance_and_fund_wallets(
            required_wallets,
            supported_proposals_in_single_tx,
            per_proposal_deposit,
            client,
        )

        proposals_numbers_in_last_tx = (
            required_proposals % supported_proposals_in_single_tx
        )
        if proposals_numbers_in_last_tx == 0:
            proposals_numbers_in_last_tx = supported_proposals_in_single_tx

        results = await asyncio.gather(
            *[
                submit_proposal_tx(
                    wallet,
                    base_proposal
                    | get_proposal_data_from_type(
                        multi_proposal.proposal_type, pparams
                    ),
                    supported_proposals_in_single_tx
                    if wallet != required_wallets[-1]
                    else proposals_numbers_in_last_tx,
                    client,
                )
                for wallet in required_wallets
            ]
        )
        response = [
            {"proposal_count": supported_proposals_in_single_tx, "tx_hash": hash}
            for hash in results
        ]
        response[-1]["proposal_count"] = proposals_numbers_in_last_tx
        return response
    else:
        raise HTTPException(
            status_code=400,
            detail="No of proposals greater than "+str(maximum_supported_proposals)+" not supported yet.",
        )


@app.get("/api/balance")
async def getWalletBalance(client: httpx.AsyncClient = Depends(get_client)):
    return await get_ada_balance(main_wallet["address"], client)


@app.post("/api/load/single")
async def submit_single_proposal(
    proposal: Dict[str, Any],
    client: httpx.AsyncClient = Depends(get_client),
):
    default_transaction = get_default_transaction()
    default_proposal_data = default_transaction["proposals"][0]
    combined_proposal = default_proposal_data | proposal
    default_transaction["proposals"][0] = combined_proposal

    if "withdraw" in combined_proposal or "parameterupdate" in combined_proposal:
            combined_proposal["script"] = get_gov_script()

    tx_url = settings.kuber_api_url + "/api/v1/tx?submit=true"
    kuber_response = await client.post(
        tx_url,
        json=default_transaction,
        headers={"api-key": settings.kuber_api_key},
    )
    if kuber_response.status_code == 200:
        tx = kuber_response.json()
        tx["type"] = "Witnessed Tx ConwayEra"
        tx_id = tx['hash']
        return tx | {"txId": tx_id}
    else:
        print(kuber_response.text)
        raise HTTPException(
            status_code=kuber_response.status_code, detail=kuber_response.text
        )
