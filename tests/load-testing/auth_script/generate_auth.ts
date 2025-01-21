import { writeFile } from "fs";
import {  ShelleyWallet } from "libcardano";
import { AuthResponse } from "./lib/types";
import { generateWallets } from "./lib/helpers/generateWallets";
import { signData } from "./lib/helpers/auth";

async function saveAuths(wallets: ShelleyWallet[]): Promise<void> {
  const jsonWallets: AuthResponse[] = [];
  for (let i = 0; i < wallets.length; i++) {
    const payload =
      "To proceed, please sign this data to verify your identity. This ensures that the action is secure and confirms your identity. ";
    const stakeAddress = wallets[i].rewardAddressBech32(0);
    let authResponse = await signData(stakeAddress, payload, wallets[i]);
    if (authResponse) {
      jsonWallets.push({ ...authResponse, stakeAddress });
    }
  }
  const jsonString = JSON.stringify(jsonWallets, null, 2);
  writeFile("../src/test/resources/auth.json", jsonString, "utf-8", (err) => {
    if (err) {
      throw new Error("Failed to write auth into file");
    }
  });
}

(async () => {
  const wallets = await generateWallets();
  await saveAuths(wallets);
})();
