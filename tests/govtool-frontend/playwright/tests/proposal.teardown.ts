import { test as cleanup } from "@playwright/test";
import proposalDiscussionService from "@services/proposalDiscussionService";
import proposalManager from "lib/proposalManager";

cleanup("Delete Proposal", async () => {
  const proposalId = await proposalManager.getProposalId();
  await proposalDiscussionService.deleteProposal(proposalId);
});
