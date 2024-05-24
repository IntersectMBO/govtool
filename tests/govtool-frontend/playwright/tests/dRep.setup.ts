import environments from "@constants/environments";
import { dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import fetch = require("node-fetch");

setup.describe.configure({ timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Register DRep");
});

dRepWallets.forEach((wallet) => {
  setup(`Register DRep of wallet: ${wallet.address}`, async () => {
    try {
      const res = await kuberService.dRepRegistration(
        wallet.stake.private,
        wallet.stake.pkh
      );

      await pollTransaction(res.txId, res.lockInfo);
    } catch (err) {
      if (err.status === 400) {
        expect(true, "DRep already registered").toBeTruthy();
      } else {
        throw err;
      }
    }
  });
});
