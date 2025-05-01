import environments from "@constants/environments";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
import { generateWallets } from "@helpers/shellyWallet";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@fixtures/walletExtension";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import { getWalletConfigForFaucet } from "@helpers/index";

const PROPOSAL_WALLETS_COUNT = 4;

let govActionDeposit: number;

setup.beforeAll(async () => {
  await functionWaitedAssert(
    async () => {
      const res = await kuberService.queryProtocolParams();
      govActionDeposit = res.govActionDeposit;
    },
    { name: "queryProtocolParams" }
  );
});

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Proposal");
  await skipIfMainnet();
  const totalRequiredBalanceForWallets =
    (govActionDeposit / 1000000) * PROPOSAL_WALLETS_COUNT +
    22 * PROPOSAL_WALLETS_COUNT;
  await skipIfBalanceIsInsufficient(totalRequiredBalanceForWallets);
});

setup("Setup temporary proposal wallets", async () => {
  setup.setTimeout(2 * environments.txTimeOut);

  const proposalWallets = await generateWallets(PROPOSAL_WALLETS_COUNT);

  // initialize wallets
  const initializeRes = await kuberService.initializeWallets(
    [...proposalWallets],
    getWalletConfigForFaucet().address,
    getWalletConfigForFaucet().payment.private
  );
  await pollTransaction(initializeRes.txId, initializeRes.lockInfo);

  const amountOutputs = proposalWallets.map((wallet) => {
    return { address: wallet.address, value: govActionDeposit };
  });
  const transferRes = await kuberService.multipleTransferADA(
    amountOutputs,
    getWalletConfigForFaucet().address,
    getWalletConfigForFaucet().payment.private
  );
  // save to file
  await walletManager.writeWallets(proposalWallets, "proposalSubmission");
  await walletManager.writeWallets(proposalWallets, "proposalSubmissionCopy");

  await pollTransaction(transferRes.txId, transferRes.lockInfo);
});
