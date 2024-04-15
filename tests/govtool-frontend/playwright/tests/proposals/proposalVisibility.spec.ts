import { expect, test } from "@playwright/test";
import { IProposal } from "@types";
import GovernanceActionsPage from "@pages/governanceActionsPage";

const mockProposals: IProposal[] = require("../../lib/_mock/proposals.json");

test("4A.2: Should access Governance Actions page without connecting wallet @smoke @fast", async ({
  page,
}) => {
  await page.getByTestId("move-to governance-actions-button").click();

  await expect(page.getByText(/Governance actions/i)).toHaveCount(2);
});

test("4B.2: Should restrict voting for users who are not registered as DReps (without wallet connected) @critical @flaky @fast", async ({
  page,
}) => {
  await page.route("**/proposal/list", async (route) => {
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(mockProposals),
    });
  });

  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const govActionDetailsPage = await govActionsPage.viewProposal(
    mockProposals[0],
  );
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});
