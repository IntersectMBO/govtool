import { test as cleanup } from "@playwright/test";
import { deleteProposal } from "@services/proposalDiscussion";
import proposalManager from "lib/proposalManager";

cleanup("Delete Proposal", async () => {
  const proposalId = await proposalManager.getProposalId();
  await deleteProposal(proposalId);
});
