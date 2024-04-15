import pollTransaction from "@helpers/pollTransaction";
import { test as cleanup } from "@playwright/test";
import kuberService from "@services/kuberService";

cleanup(`Abstain delegation`, async () => {
  const { txId } = await kuberService.abstainDelegations();
  await pollTransaction(txId);
});
