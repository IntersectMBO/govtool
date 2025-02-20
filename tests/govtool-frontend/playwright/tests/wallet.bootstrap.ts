import { adaHolderWallets, dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { expect } from "@playwright/test";
import { test as setup } from "@fixtures/walletExtension";
import kuberService from "@services/kuberService";
import environments from "lib/constants/environments";

setup.describe.configure({ mode: "serial", timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Wallet bootstrap");
  await skipIfMainnet();
});

setup("Initialize static wallets", async () => {
  try {
    const wallets = [...adaHolderWallets, ...dRepWallets];
    const res = await kuberService.initializeWallets(wallets);
    await pollTransaction(res.txId);
  } catch (err) {
    if (err.status === 400 && err.message.includes("StakeKeyRegisteredDELEG")) {
      expect(true, "Wallets already initialized").toBeTruthy();
    } else {
      throw Error(err);
    }
  }
});
