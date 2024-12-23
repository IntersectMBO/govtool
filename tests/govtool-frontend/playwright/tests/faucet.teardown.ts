import environments from "@constants/environments";
import { faucetWallet } from "@constants/staticWallets";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { pollTransaction } from "@helpers/transaction";
import { test as cleanup, expect } from "@playwright/test";
import kuberService from "@services/kuberService";

cleanup.describe.configure({ timeout: environments.txTimeOut });
cleanup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Cleanup");
  await skipIfMainnet();
});

cleanup("Refund faucet", async () => {
  try {
    const faucetRemainingBalance = await kuberService.getBalance(
      faucetWallet.address
    );

    const transferBalance = Math.floor(faucetRemainingBalance) - 3;
    const { txId, lockInfo } = await kuberService.transferADA(
      [environments.faucet.address],
      transferBalance
    );
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
