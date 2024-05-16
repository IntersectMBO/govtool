import { dRep01Wallet } from "@constants/staticWallets";
import DRepDirectoryPage, { dRepFilterOptions } from "@pages/dRepDirectoryPage";
import { expect, test } from "@playwright/test";

test("2C. Verify DRep Behavior in Disconnected State", async ({ page }) => {
  await page.goto("/");

  await page.getByTestId("view-drep-directory-button").click();
  await page
    .locator('[data-testid$="-connect-to-delegate-button"]')
    .first()
    .click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});

test("2J. Should search by DRep id", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await expect(dRepDirectory.getDRepCard(dRep01Wallet.dRepId)).toHaveText(
    dRep01Wallet.dRepId
  );
});

test("2K. Should filter DReps", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.filterBtn.click();

  // Single filter
  for (const option of dRepFilterOptions) {
    await dRepDirectory.filterDRepByNames([option]);
    await dRepDirectory.validateFilters([option]);
    await dRepDirectory.unFilterDRepByNames([option]);
  }

  // Multiple filters
  const multipleFilterOptionNames = [...dRepFilterOptions];
  while (multipleFilterOptionNames.length > 1) {
    await dRepDirectory.filterDRepByNames(multipleFilterOptionNames);
    await dRepDirectory.validateFilters(multipleFilterOptionNames);
    await dRepDirectory.unFilterDRepByNames(multipleFilterOptionNames);
    multipleFilterOptionNames.pop();
  }
});
