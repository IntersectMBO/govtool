import environments from "@constants/environments";
import { allStaticWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { test as cleanup, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { StaticWallet } from "@types";
import walletManager from "lib/walletManager";

cleanup.describe.configure({ timeout: environments.txTimeOut });
cleanup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Cleanup");
  await skipIfMainnet();
});

cleanup("Refund faucet", async () => {
  const registerDRepWallets: StaticWallet[] =
    await walletManager.readWallets("registerDRepCopy");
  const registeredDRepWallets: StaticWallet[] =
    await walletManager.readWallets("registeredDRepCopy");
  try {
    const { txId, lockInfo } = await kuberService.mergeUtXos([
      ...allStaticWallets,
      ...registerDRepWallets,
      ...registeredDRepWallets,
    ]);
    await pollTransaction(txId, lockInfo);
  } catch (err) {
    console.log(err);
    if (err.status === 400) {
      expect(true, "Failed to trasfer Ada").toBeTruthy();
    } else {
      throw Error(err);
    }
  }
});
