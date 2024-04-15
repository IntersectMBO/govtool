import { expect, test as setup } from "@playwright/test";
import { ShelleyWallet } from "@mock/cardano-test-wallet/crypto";
import { writeFile } from "fs";
import environments from "lib/constants/environments";
import generateShellyWallets from "@helpers/generateShellyWallets";
import setupWallets from "@helpers/setupWallets";
import { loadAmountFromFaucet } from "@services/faucetService";
import {
  adaHolderWallets,
  dRep01Wallet,
  dRepWallets,
  faucetWallet,
} from "@constants/staticWallets";
import pollTransaction from "@helpers/pollTransaction";
import kuberService from "@services/kuberService";
import extractDRepsFromStakePubKey from "@helpers/extractDRepsFromStakePubkey";

setup.describe.configure({ mode: "serial" });

setup("Setup mock wallets", async () => {
  setup.skip(!environments.oneTimeWalletSetup);

  const wallets = await generateShellyWallets(50);
  await setupWallets(wallets);
  saveWallets(wallets);
});

setup("Fund faucet wallet", async () => {
  const balance = await kuberService.getBalance(faucetWallet.address);
  if (balance > 200) return;

  const res = await loadAmountFromFaucet(faucetWallet.address);
  await pollTransaction(res.txid);
});

setup("Fund static wallets", async () => {
  const addresses = [...adaHolderWallets, ...dRepWallets].map((e) => e.address);
  const res = await kuberService.transferADA(
    faucetWallet.address,
    addresses,
    faucetWallet.payment.private
  );
  await pollTransaction(res.txId);
});

[...adaHolderWallets, ...dRepWallets].forEach((wallet) => {
  setup(`Register stake of static wallet: ${wallet.address}`, async () => {
    try {
      const { txId } = await kuberService.registerStake(
        wallet.stake.private,
        wallet.stake.pkh,
        wallet.payment.private,
        wallet.address
      );
      await pollTransaction(txId);
    } catch (err) {
      if (err.status === 400) {
        expect(true, "Stake already registered").toBeTruthy();
      } else {
        throw Error(err);
      }
    }
  });
});

function saveWallets(wallets: ShelleyWallet[]) {
  const jsonWallets = [];
  for (let i = 0; i < wallets.length; i++) {
    const stakePublicKey = Buffer.from(wallets[i].stakeKey.public).toString(
      "hex"
    );
    const { dRepIdBech32 } = extractDRepsFromStakePubKey(stakePublicKey);

    jsonWallets.push({
      ...wallets[i].json(),
      address: wallets[i].addressBech32(environments.networkId),
      dRepId: dRepIdBech32,
    });
  }
  const jsonString = JSON.stringify(jsonWallets, null, 2);
  writeFile("src/_mock/wallets.json", jsonString, "utf-8", (err) => {
    if (err) {
      throw Error("Failed to write wallets into file");
    }
  });
}
