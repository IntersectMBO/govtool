import { test } from "@fixtures/proposalDiscussionDetailsPage";
import { setAllureEpic } from "@helpers/allure";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

test.beforeEach(() => {
  setAllureEpic("Proposal Discussion Forum");
});

test("8A. Should access proposed governance actions in disconnected state", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await expect(page.getByText(/Proposed Governance Actions/i)).toHaveCount(2);
});
