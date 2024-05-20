import environments from "@constants/environments";
import { adaHolderWallets } from "@constants/staticWallets";
import { setAllureStory, setAllureEpic } from "@helpers/allure";
import { pollTransaction } from "@helpers/transaction";
import { test as cleanup } from "@playwright/test";
import kuberService from "@services/kuberService";

cleanup.describe.configure({ timeout: environments.txTimeOut });
cleanup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Cleanup");
});
cleanup(`Abstain delegation`, async () => {
  const stakePrivKeys = adaHolderWallets.map((wallet) => wallet.stake.private);
  const stakePkhs = adaHolderWallets.map((wallet) => wallet.stake.pkh);

  const { txId, lockInfo } = await kuberService.abstainDelegations(
    stakePrivKeys,
    stakePkhs
  );
  await pollTransaction(txId, lockInfo);
});
