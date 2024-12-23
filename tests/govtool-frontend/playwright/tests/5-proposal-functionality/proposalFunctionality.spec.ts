import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";
const invalidInfinityProposals = require("../../lib/_mock/invalidInfinityProposals.json");

test.beforeEach(async () => {
  await setAllureEpic("5. Proposal functionality");
  await skipIfNotHardFork();
});
test.describe("Bad Proposals", () => {
  let govActionsPage: GovernanceActionsPage;

  test.beforeEach(async ({ page }) => {
    await page.route("**/proposal/list?**", async (route) =>
      route.fulfill({
        body: JSON.stringify(invalidInfinityProposals),
      })
    );

    govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();
  });

  test("5G. Should show warning in bad governance action proposal to the users to visit the site at their own risk, when external url is opened", async () => {
    const govActionDetailsPage = await govActionsPage.viewFirstProposal();

    await govActionDetailsPage.externalModalBtn.click();

    await expect(govActionDetailsPage.externalLinkModal).toBeVisible();
    await expect(
      govActionDetailsPage.currentPage.getByText("Be careful", {
        exact: false,
      })
    ).toBeVisible();
  });

  test("5H. Should open a new tab in Bad governance action proposal, when external URL is opened", async ({
    page,
  }) => {
    const govActionDetailsPage = await govActionsPage.viewFirstProposal();

    await govActionDetailsPage.externalModalBtn.click();
    await govActionDetailsPage.continueModalBtn.click();
    const existingPages = page.context().pages();
    expect(existingPages).toHaveLength(1);
  });
});
