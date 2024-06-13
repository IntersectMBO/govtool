import { proposal01 } from "@constants/staticProposals";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

test("8E. Should share proposed governance action", async ({
  page,
  context,
}) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);

  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  const proposalCard = page.getByTestId(`proposal-${proposal01.id}`);
  await proposalCard.getByTestId("share-button").click();
  await expect(page.getByText("Copied to clipboard")).toBeVisible();
  const copiedTextDRepDirectory = await page.evaluate(() =>
    navigator.clipboard.readText()
  );
  expect(copiedTextDRepDirectory).toEqual(proposal01.id);
});
