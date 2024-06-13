import { faker } from "@faker-js/faker";
import { test as base } from "@fixtures/walletExtension";
import { generateWalletAddress } from "@helpers/cardano";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import {
  deleteProposal,
  postCreateProposal,
} from "@services/proposalDiscussion";
import { ProposalCreateRequest } from "@services/proposalDiscussion/types";

export const test = base.extend<{
  proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;
}>({
  // setup
  proposalDiscussionDetailsPage: async ({ page }, use) => {
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

    const proposalId = createProposalRes.data.attributes.proposal_id;
    const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
      page
    );
    await proposalDiscussionDetailsPage.goto(proposalId);

    await use(proposalDiscussionDetailsPage);

    // cleanup
    await deleteProposal(proposalId);
  },
});
