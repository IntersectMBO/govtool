import environments from "@constants/environments";
import { proposalFaucetWallet } from "@constants/proposalFaucetWallet";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfMainnet, skipIfNotHardFork } from "@helpers/cardano";
import { generateWallets } from "@helpers/shellyWallet";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@fixtures/walletExtension";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";

const PROPOSAL_WALLETS_COUNT = 4;

let govActionDeposit: number;

setup.beforeAll(async () => {
  const res = await kuberService.queryProtocolParams();
  govActionDeposit = res.govActionDeposit;
});

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Proposal");
  await skipIfNotHardFork();
  await skipIfMainnet();
});

setup("Setup temporary proposal wallets", async () => {
  setup.setTimeout(2 * environments.txTimeOut);

  const proposalWallets = await generateWallets(PROPOSAL_WALLETS_COUNT);

  // initialize wallets
  const initializeRes = await kuberService.initializeWallets(
    [...proposalWallets],
    proposalFaucetWallet.address,
    proposalFaucetWallet.payment.private
  );
  await pollTransaction(initializeRes.txId, initializeRes.lockInfo);

  const amountOutputs = proposalWallets.map((wallet) => {
    return { address: wallet.address, value: govActionDeposit };
  });
  const transferRes = await kuberService.multipleTransferADA(
    amountOutputs,
    proposalFaucetWallet.address,
    proposalFaucetWallet.payment.private
  );
  // save to file
  await walletManager.writeWallets(proposalWallets, "proposalSubmission");
  await walletManager.writeWallets(proposalWallets, "proposalSubmissionCopy");

  await pollTransaction(transferRes.txId, transferRes.lockInfo);
});
