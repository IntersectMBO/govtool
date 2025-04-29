import environments from "@constants/environments";
import {
  budgetProposalDRepWallets,
  dRepWallets,
} from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { expect } from "@playwright/test";
import { test as cleanup } from "@fixtures/walletExtension";
import kuberService from "@services/kuberService";
import { StaticWallet } from "@types";
import walletManager from "lib/walletManager";

cleanup.describe.configure({ timeout: environments.txTimeOut });
cleanup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Cleanup");
  await skipIfMainnet();
});

cleanup("DRep de-registration", async () => {
  const registerDRep: StaticWallet[] =
    await walletManager.readWallets("registerDRepCopy");
  const registeredDRep: StaticWallet[] =
    await walletManager.readWallets("registeredDRepCopy");

  const registeredDRepWallets = [
    ...dRepWallets,
    ...registerDRep,
    ...registeredDRep,
    ...budgetProposalDRepWallets,
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
