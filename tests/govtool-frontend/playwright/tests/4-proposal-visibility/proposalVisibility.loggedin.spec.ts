import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { isMobile, openDrawer } from "@helpers/mobile";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
  await skipIfNotHardFork();
});

test("4A_1. Should access Governance Actions page with connecting wallet", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    await openDrawer(page);
  }

  await page.getByTestId("governance-actions-link").click();
  await expect(page.getByText(/Governance Actions/i)).toHaveCount(2);
});

test("4B_1. Should restrict voting for users who are not registered as DReps (with wallet connected)", async ({
  page,
}) => {
  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const govActionDetailsPage = await govActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});
