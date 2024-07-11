import environments from "@constants/environments";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";

const PROPOSAL_SUBMISSIONS_WALLETS_COUNT = 1;

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("proposal");
});

async function generateWallets(num: number) {
  return await Promise.all(
    Array.from({ length: num }, () =>
      ShelleyWallet.generate().then((wallet) => wallet.json())
    )
  );
}

setup("Setup temporary proposal wallets", async () => {
  setup.setTimeout(2 * environments.txTimeOut);

  const proposalSubmissionsWallets = await generateWallets(
    PROPOSAL_SUBMISSIONS_WALLETS_COUNT
  );

  // initialize wallets
  const initializeRes = await kuberService.initializeWallets([
    ...proposalSubmissionsWallets,
  ]);
  await pollTransaction(initializeRes.txId, initializeRes.lockInfo);

  // transfer 51_000 ADA for dRep registration
  const amountOutputs = proposalSubmissionsWallets.map((wallet) => {
    return { address: wallet.address, value: `${51_000}A` };
  });
  const transferRes = await kuberService.multipleTransferADA(amountOutputs);
  await pollTransaction(transferRes.txId, transferRes.lockInfo);

  // save to file
  await walletManager.writeWallets(
    proposalSubmissionsWallets,
    "proposalSubmission"
  );
});
