import { writeFile } from "fs";
import {  Cip30ShelleyWallet, ShelleyWallet } from "libcardano-wallet";
import { AuthResponse } from "./lib/types";
import { generateWallets } from "./lib/helpers/generateWallets";
import { authenticate } from "./lib/helpers/auth";
import { loadCrypto } from "libcardano";

async function saveAuths(wallets: ShelleyWallet[]): Promise<void> {
  
  const jsonWallets: AuthResponse[] = [];
  for (let i = 0; i < wallets.length; i++) {
    const cip30 = new Cip30ShelleyWallet({} as any,{} as any,wallets[i],0)

    const stakeAddress = wallets[i].stakeAddrechBech32(0);
    
    let authResponse = await authenticate(cip30);
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

setTimeout(async () => {
  await loadCrypto()
  console.log("crypto loaded")
  const wallets = await generateWallets();
  await saveAuths(wallets);
},3000)
