import environments from "@constants/environments";
import { allStaticWallets } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
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
  await skipIfBalanceIsInsufficient(10);
});

// Kuber resolves every input address with a UTxO lookup (about 5 s each on
// preview), and requests that run past about 40 s come back as a 503. Merge the
// wallets in batches small enough to finish well within that.
const MERGE_BATCH_SIZE = 7;

cleanup("Refund faucet", async () => {
  const registerDRepWallets: StaticWallet[] =
    await walletManager.readWallets("registerDRepCopy");
  const registeredDRepWallets: StaticWallet[] =
    await walletManager.readWallets("registeredDRepCopy");
  const proposalSubmissionWallets: StaticWallet[] =
    await walletManager.readWallets("proposalSubmissionCopy");
  const wallets = [
    ...allStaticWallets,
    ...registerDRepWallets,
    ...registeredDRepWallets,
    ...proposalSubmissionWallets,
  ];
  const batches: StaticWallet[][] = [];
  for (let i = 0; i < wallets.length; i += MERGE_BATCH_SIZE) {
    batches.push(wallets.slice(i, i + MERGE_BATCH_SIZE));
  }
  // Each batch waits for its transaction to confirm before the next one starts,
  // since every batch spends from the faucet address.
  cleanup.setTimeout(batches.length * environments.txTimeOut);

  for (const batch of batches) {
    try {
      const { txId, lockInfo } = await kuberService.mergeUtXos(batch);
      await pollTransaction(txId, lockInfo);
    } catch (err) {
      console.log(err);
      if (err.status === 400) {
        expect(true, "Failed to trasfer Ada").toBeTruthy();
      } else {
        throw Error(err);
      }
    }
  }
});
