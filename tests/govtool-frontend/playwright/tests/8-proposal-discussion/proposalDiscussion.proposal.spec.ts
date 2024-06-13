import { proposal01 } from "@constants/staticProposals";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

test("8E. Should share proposed governance action", async ({
  page,
  context,
}) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);

  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.searchInput.fill(proposal01.title);

  const proposalCard = page.getByTestId(`proposal-${proposal01.id}`);
  await proposalCard.getByTestId("share-button").click();
  await expect(page.getByText("Copied to clipboard")).toBeVisible();
  const copiedTextDRepDirectory = await page.evaluate(() =>
    navigator.clipboard.readText()
  );
  expect(copiedTextDRepDirectory).toEqual(proposal01.id);
});

test("8I. Should disable poll voting functionality.", async ({ page }) => {
  const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
  await proposalDiscussionDetailsPage.goto(proposal01.id);

  await expect(proposalDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
  await expect(
    proposalDiscussionDetailsPage.pollYesVoteCount
  ).not.toBeVisible();

  await expect(proposalDiscussionDetailsPage.pollNoVoteCount).not.toBeVisible();
});

test("8J. Should sort the proposed governance action comments.", async ({
  page,
}) => {
  const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
  await proposalDiscussionDetailsPage.goto(proposal01.id);

  await proposalDiscussionDetailsPage.sortAndValidate(
    "asc",
    (date1, date2) => new Date(date1) <= new Date(date2)
  );
});
