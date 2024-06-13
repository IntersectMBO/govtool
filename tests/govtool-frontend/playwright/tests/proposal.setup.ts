import environments from "@constants/environments";
import { faker } from "@faker-js/faker";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { generateWalletAddress } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { createFile } from "@helpers/file";
import { pollTransaction } from "@helpers/transaction";
import { test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import {
  postAddComment,
  postAddPoll,
  postCreateProposal,
} from "@services/proposalDiscussion";
import {
  CommentRequest,
  ProposalCreateRequest,
} from "@services/proposalDiscussion/types";
import { StaticProposal } from "@types";
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

setup("Create temporary proposal", async () => {
  const receivingAddr = generateWalletAddress();
  const proposalRequest: ProposalCreateRequest = {
    proposal_links: [
      {
        prop_link: faker.internet.url(),
        prop_link_text: faker.internet.displayName(),
      },
    ],
    gov_action_type_id: 1,
    prop_name: faker.company.name(),
    prop_abstract: faker.lorem.paragraph(2),
    prop_motivation: faker.lorem.paragraph(2),
    prop_rationale: faker.lorem.paragraph(2),
    prop_receiving_address: receivingAddr,
    prop_amount: faker.number.int({ min: 100, max: 1000 }).toString(),
    is_draft: false,
  };

  const createProposalRes = await postCreateProposal(proposalRequest);

  await postAddPoll({
    proposal_id: createProposalRes.data.attributes.proposal_id.toString(),
    poll_start_dt: new Date().toISOString(),
    is_poll_active: true,
  });

  const comments: CommentRequest[] = [];

  for (let i = 0; i < 4; i++) {
    const comment: CommentRequest = {
      proposal_id: createProposalRes.data.attributes.proposal_id.toString(),
      comment_text: faker.lorem.paragraph(2),
    };
    comments.push(comment);

    await postAddComment(comment);
  }

  const staticProposal: StaticProposal = {
    id: createProposalRes.data.attributes.proposal_id,
    comments,
    title: proposalRequest.prop_name,
  };
  await createFile("proposals.json", [staticProposal]);
});
