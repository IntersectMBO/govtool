import environments from "@constants/environments";
import { dRepWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { pollTransaction } from "@helpers/transaction";
import { test as cleanup, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { StaticWallet } from "@types";

cleanup.describe.configure({ timeout: environments.txTimeOut });
cleanup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Cleanup");
});

const registerDRep: StaticWallet[] = require("../lib/_mock/registerDRepCopyWallets.json");
const registeredDRep: StaticWallet[] = require("../lib/_mock/registeredDRepCopyWallets.json");

cleanup("DRep de-registration", async () => {
  const registeredDRepWallets = [
    ...dRepWallets,
    ...registerDRep,
    ...registeredDRep,
  ];
  try {
    const { txId, lockInfo } = await kuberService.multipleDRepDeRegistration(
      registeredDRepWallets
    );
    await pollTransaction(txId, lockInfo);
  } catch (err) {
    console.log(err);
    if (err.status === 400) {
      expect(true, "DRep not registered").toBeTruthy();
    } else {
      throw Error(err);
    }
  }
});
