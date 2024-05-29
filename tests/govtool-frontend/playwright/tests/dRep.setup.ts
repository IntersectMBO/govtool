import environments from "@constants/environments";
import { dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";

setup.describe.configure({ timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Register DRep");
});

setup("Register DRep of static wallets", async () => {
  try {
    const res = await kuberService.multipleDRepRegistration(dRepWallets);

    await pollTransaction(res.txId, res.lockInfo);
  } catch (err) {
    if (
      err.status === 400 &&
      err.message.includes("ConwayDRepAlreadyRegistered")
    ) {
      expect(true, "DRep already registered").toBeTruthy();
    } else {
      throw err;
    }
  }
});
