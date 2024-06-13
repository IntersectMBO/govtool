import { test as cleanup } from "@playwright/test";
import { deleteProposal } from "@services/proposalDiscussion";
import { StaticProposal } from "@types";

const staticProposals: StaticProposal[] = require("../lib/_mock/proposals.json");

cleanup("Delete Proposal", async () => {
  for (let i = 0; i < staticProposals.length; i++) {
    await deleteProposal(staticProposals[i].id);
  }
});
