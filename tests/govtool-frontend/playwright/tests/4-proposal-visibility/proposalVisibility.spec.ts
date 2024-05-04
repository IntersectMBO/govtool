import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect, test } from "@playwright/test";

test("4A.2: Should access Governance Actions page without connecting wallet @smoke @fast", async ({
  page,
}) => {
  await page.goto("/");
  await page.getByTestId("move-to-governance-actions-button").click();

  await expect(page.getByText(/Governance actions/i)).toHaveCount(2);
});

test("4B.2: Should restrict voting for users who are not registered as DReps (without wallet connected)  @flaky @fast", async ({
  page,
}) => {
  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const govActionDetailsPage = await govActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});
