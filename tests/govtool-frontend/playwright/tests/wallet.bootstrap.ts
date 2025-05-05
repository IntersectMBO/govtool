import { adaHolderWallets, dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { expect } from "@playwright/test";
import { test as setup } from "@fixtures/walletExtension";
import kuberService from "@services/kuberService";
import environments from "lib/constants/environments";

const totalWalletsToInitialize = [...adaHolderWallets, ...dRepWallets];

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Wallet bootstrap");
  await skipIfMainnet();
  await skipIfBalanceIsInsufficient(22 * totalWalletsToInitialize.length);
});

setup("Initialize static wallets", async () => {
  setup.setTimeout(environments.txTimeOut);
  try {
    const res = await kuberService.initializeWallets(totalWalletsToInitialize);
    await pollTransaction(res.txId);
  } catch (err) {
    if (err.status === 400 && err.message.includes("StakeKeyRegisteredDELEG")) {
      expect(true, "Wallets already initialized").toBeTruthy();
    } else {
      throw Error(err);
    }
  }
});
