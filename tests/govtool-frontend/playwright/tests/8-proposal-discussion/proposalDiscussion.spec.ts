import { test } from "@fixtures/proposalDiscussionDetailsPage";
import { setAllureEpic } from "@helpers/allure";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

test.beforeEach(() => {
  setAllureEpic("Proposal Discussion Forum");
});

test("8A. Should access the list of proposed governance actions in a disconnected state.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await expect(proposalDiscussionPage.searchInput).toBeVisible();
  await expect(page.getByText(/Proposal Discussion/i)).toHaveCount(2);
});

test("8E.Should share proposed governance actions.", async ({ page }) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();
  
  const proposalCard  = await proposalDiscussionPage.getFirstProposal();

  await proposalCard.getByTestId("share-button").click();
});
