import { adaHolderWallets, dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import environments from "lib/constants/environments";

setup.describe.configure({ mode: "serial", timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Wallet bootstrap");
  setup.skip(environments.networkId === 1);
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
