import { adaHolderWallets } from "@constants/staticWallets";
import { pollTransaction } from "@helpers/transaction";
import { test as cleanup } from "@playwright/test";
import kuberService from "@services/kuberService";

cleanup(`Abstain delegation`, async () => {
  const stakePrivKeys = adaHolderWallets.map((wallet) => wallet.stake.private);
  const stakePkhs = adaHolderWallets.map((wallet) => wallet.stake.pkh);

  const { txId, address } = await kuberService.abstainDelegations(
    stakePrivKeys,
    stakePkhs
  );
  await pollTransaction(txId, address);
});
