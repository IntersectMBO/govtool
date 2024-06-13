import { mockProposalCreationPayload } from "@mock/index";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import { test as base } from "@fixtures/walletExtension";
import proposalDiscussionService from "@services/proposalDiscussionService";
import { ProposalCreationResponse } from "@types";

export const test = base.extend<{
  proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;
}>({
  proposalDiscussionDetailsPage: async ({ page }, use) => {
    const response: ProposalCreationResponse =
      await proposalDiscussionService.createProposal(
        mockProposalCreationPayload
      );

    const proposalId = response.data.attributes.proposal_id;
    const proposalDiscussionDetailPage = new ProposalDiscussionDetailsPage(
      page,
      proposalId
    );

    await use(proposalDiscussionDetailPage);

    await proposalDiscussionService.deleteProposal(proposalId);
  },
});
