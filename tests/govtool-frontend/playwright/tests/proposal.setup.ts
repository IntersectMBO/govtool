import environments from "@constants/environments";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfBalanceIsInsufficient, skipIfMainnet } from "@helpers/cardano";
import { addressBech32, generateWallets } from "@helpers/shellyWallet";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@fixtures/walletExtension";
import kuberService from "@services/kuberService";
import walletManager from "lib/walletManager";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import {
  getProposalWalletCount,
  getWalletConfigForFaucet,
} from "@helpers/index";
import { createKeyFromPrivateKeyHex } from "@helpers/crypto";

const PROPOSAL_WALLETS_COUNT = getProposalWalletCount();

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
    (govActionDeposit / 1000000 + 22) * PROPOSAL_WALLETS_COUNT;
  await skipIfBalanceIsInsufficient(totalRequiredBalanceForWallets);
});

setup("Setup temporary proposal wallets", async () => {
  setup.setTimeout(environments.txTimeOut);

  const proposalWallets = await generateWallets(PROPOSAL_WALLETS_COUNT);
  const stakeKeys = await createKeyFromPrivateKeyHex(
    environments.faucet.stake.private || ""
  );
  const { pkh: stakePkh, public: stakePublic } = stakeKeys.json();

  const enrichedProposalWallets = proposalWallets.map((wallet) => {
    const stake = {
      pkh: stakePkh,
      private: environments.faucet.stake.private,
      public: stakePublic,
    };

    const walletAddress = addressBech32(
      environments.networkId,
      wallet.payment.pkh,
      stakePkh
    );

    return {
      ...wallet,
      address: walletAddress,
      stake,
    };
  });

  proposalWallets.splice(0, proposalWallets.length, ...enrichedProposalWallets);

  const amountOutputs = proposalWallets.map((wallet) => {
    return { address: wallet.address, value: govActionDeposit + 22000000 };
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
