import { adaHolderWallets, dRepWallets } from "@constants/staticWallets";
import { ShelleyWallet } from "@helpers/crypto";
import extractDRepsFromStakePubKey from "@helpers/extractDRepsFromStakePubkey";
import generateShellyWallets from "@helpers/generateShellyWallets";
import setupWallets from "@helpers/setupWallets";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import { writeFile } from "fs";
import environments from "lib/constants/environments";

setup.describe.configure({ mode: "serial", timeout: environments.txTimeOut });

setup("Setup mock wallets", async () => {
  setup.skip(!environments.oneTimeWalletSetup);

  const wallets = await generateShellyWallets(6);
  await setupWallets(wallets);
  saveWallets(wallets);
});

setup("Fund static wallets", async () => {
  const addresses = [...adaHolderWallets, ...dRepWallets].map((e) => e.address);
  const res = await kuberService.transferADA(addresses);
  await pollTransaction(res.txId);
});

for (const wallet of [...adaHolderWallets, ...dRepWallets]) {
  setup(`Register stake of static wallet: ${wallet.address}`, async () => {
    try {
      const { txId, lockInfo } = await kuberService.registerStake(
        wallet.stake.private,
        wallet.stake.pkh,
        wallet.payment.private,
        wallet.address,
      );
      await pollTransaction(txId, lockInfo);
    } catch (err) {
      if (err.status === 400) {
        expect(true, "Stake already registered").toBeTruthy();
      } else {
        throw Error(err);
      }
    }
  });
}

function saveWallets(wallets: ShelleyWallet[]) {
  const jsonWallets = [];
  for (let i = 0; i < wallets.length; i++) {
    const stakePublicKey = Buffer.from(wallets[i].stakeKey.public).toString(
      "hex",
    );
    const { dRepIdBech32 } = extractDRepsFromStakePubKey(stakePublicKey);

    jsonWallets.push({
      ...wallets[i].json(),
      address: wallets[i].addressBech32(environments.networkId),
      dRepId: dRepIdBech32,
    });
  }
  const jsonString = JSON.stringify(jsonWallets, null, 2);
  writeFile("lib/_mock/wallets.json", jsonString, "utf-8", (err) => {
    if (err) {
      throw Error("Failed to write wallets into file");
    }
  });
}
