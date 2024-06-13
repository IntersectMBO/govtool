import environments from "@constants/environments";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import proposalDiscussionService from "@services/proposalDiscussionService";
import { AddPollPayload, ProposalCreationResponse } from "@types";
import walletManager from "lib/walletManager";
import { mockProposalCreationPayload } from "@mock/index";
import proposalManager from "lib/proposalManager";

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

setup("Create temporary proposal", async () => {
  const response: ProposalCreationResponse =
    await proposalDiscussionService.createProposal(mockProposalCreationPayload);

  const mockAddPollPayload: AddPollPayload = {
    data: {
      proposal_id: response.data.attributes.proposal_id.toString(),
      poll_start_dt: new Date().toISOString(),
      is_poll_active: true,
    },
  };
  await proposalDiscussionService.addPoll(mockAddPollPayload);

  await proposalManager.writeProposal({
    payload: mockProposalCreationPayload,
    response: response,
  });
});
