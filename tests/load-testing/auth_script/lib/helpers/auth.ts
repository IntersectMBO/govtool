import { ShelleyWallet, bech32 } from "libcardano";
import { valid } from "../_mock/index";
import { AuthResponse } from "lib/types";
import { cip008Sign } from "libcardano/cardano/cip8";

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

export async function signData(
  address: string,
  payload: string,
  wallet: ShelleyWallet
) {
  const signedData = await cip008Sign(
    Buffer.from(address, "hex"),
    wallet.stakeKey,
    Buffer.from(payload, "hex")
  );

  const signDataResponse = await fetch(
    process.env.PDF_API_URL + "/auth/local",
    {
      method: "POST",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({
        identifier: bech32.decode(address).data.toString("hex"),
        signedData,
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
    return await signData(address, payload, wallet);
  }
  console.log("failed to sign data", await signDataResponse.text());
  return null;
}
