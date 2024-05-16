import environments from "@constants/environments";
import { adaHolderWallets } from "@constants/staticWallets";
import { pollTransaction } from "@helpers/transaction";
import { test as cleanup } from "@playwright/test";
import kuberService from "@services/kuberService";

cleanup.describe.configure({ timeout: environments.txTimeOut });

cleanup(`Abstain delegation`, async () => {
  const stakePrivKeys = adaHolderWallets.map((wallet) => wallet.stake.private);
  const stakePkhs = adaHolderWallets.map((wallet) => wallet.stake.pkh);

  const { txId, lockInfo } = await kuberService.abstainDelegations(
    stakePrivKeys,
    stakePkhs,
  );
  await pollTransaction(txId, lockInfo);
});
