import {  bech32 } from "libcardano";
import { valid } from "../_mock/index";
import { AuthResponse } from "lib/types";
import { Cip30, Cip30Provider, ShelleyWallet } from "libcardano-wallet";
import { Cip8SignResult } from "libcardano/cardano/cip8";

async function addUsername(jsonResponse: AuthResponse) {
  const username = valid.username();
  await fetch(process.env.PDF_API_URL + "/users/edit", {
    method: "PUT",
    body: JSON.stringify({
      govtoolUsername: username,
    }),
    headers: {
      "content-type": "application/json",
      Authorization: "Bearer " + jsonResponse.jwt,
    },
  });
}

export async function authenticate(
  wallet: Cip30Provider,
  max_retries: number=2
) {
  const stakeAddress=(await wallet.getRewardAddresses())[0]
  const stakeAddressHex = stakeAddress.toBytes().toString('hex')
  const challengeResponse=await fetch( process.env.PDF_API_URL +"/api/auth/challenge?identifier="+stakeAddressHex, {
    "method": "GET",
    "mode": "cors",
  });
  const challengeJson: {message: string} = await challengeResponse.json()
  console.log("Challenge",stakeAddressHex,challengeJson)
  const signature = await wallet.signData(stakeAddressHex,Buffer.from(challengeJson.message).toString('hex'))
  const signatureJson: Cip8SignResult & {expectedSignedMessage: string} = {...signature.toCip8Json(),expectedSignedMessage:challengeJson.message}

  const signDataResponse = await fetch(
    process.env.PDF_API_URL + "/api/auth/local",
    {
      method: "POST",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({
        identifier: stakeAddressHex,
        signedMessage: signatureJson,
      }),
    }
  );

  if (signDataResponse.status == 200) {
    const jsonResponse = await signDataResponse.json();
    await addUsername(jsonResponse);
    return jsonResponse;
  }
  if (signDataResponse.status === 429) {
    let remainingRateLimitTime =
      parseInt(signDataResponse.headers.get("x-ratelimit-reset")) -
      Math.floor(new Date().getTime() / 1000);

    while (remainingRateLimitTime > 0) {
      await new Promise((resolve) => setTimeout(resolve, 1000)); // Wait for 1 second
      remainingRateLimitTime =
        parseInt(signDataResponse.headers.get("x-ratelimit-reset")) -
        Math.floor(new Date().getTime() / 1000);
    }

    console.log("Retrying now...");
    return await authenticate(wallet,max_retries-1);
  }else{
    console.log("Authentication failed. Status:", signDataResponse.status);
    console.log(signDataResponse.url,signDataResponse.url)
    console.log("Response body:", await signDataResponse.text());
  }
}
