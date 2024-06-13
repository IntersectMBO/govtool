import { proposal01 } from "@constants/staticProposals";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

test("8E. Should share proposed governance action", async ({
  page,
  context,
}) => {
  const proposalId = proposal01.data.attributes.proposal_id;
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);

  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  const proposalCard = page.getByTestId(`proposal-${proposalId}`);
  await proposalCard.getByTestId("share-button").click();
  await expect(page.getByText("Copied to clipboard")).toBeVisible();
  const copiedTextDRepDirectory = await page.evaluate(() =>
    navigator.clipboard.readText()
  );
  expect(copiedTextDRepDirectory).toEqual(proposalId);
});
