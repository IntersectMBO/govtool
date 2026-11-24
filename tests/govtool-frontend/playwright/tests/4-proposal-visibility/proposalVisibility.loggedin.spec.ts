import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";

test.use({ walletName: "user01", walletFundsAda: 0 });

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
});

test("4A_1. Should access Governance Actions page with connecting wallet", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    await openDrawer(page);
  }

  // The connected nav's Governance Actions item opens Live Voting, whose
  // page tab keeps the old dropdown entry's id.
  await page.getByTestId("governance-actions-link").click();
  await expect(page).toHaveURL(/\/actions(\?|$)/);
  await expect(
    page.getByTestId("governance-actions-live-voting-link")
  ).toHaveAttribute("aria-current", "page");
});

test("4B_1. Should restrict voting for users who are not registered as DReps (with wallet connected)", async ({
  page,
}) => {
  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const govActionDetailsPage = await govActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});
