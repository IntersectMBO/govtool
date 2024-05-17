import { dRep01Wallet } from "@constants/staticWallets";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect, test } from "@playwright/test";
import { DRepStatus } from "@types";

test("2C. Should open wallet connection popup on delegate in disconnect state", async ({
  page,
}) => {
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
  const dRepFilterOptions: DRepStatus[] = ["Active", "Inactive", "Retired"];

  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.filterBtn.click();

  // Single filter
  for (const option of dRepFilterOptions) {
    await dRepDirectory.filterDReps([option]);
    await dRepDirectory.validateFilters([option], dRepFilterOptions);
    await dRepDirectory.unFilterDReps([option]);
  }

  // Multiple filters
  const multipleFilterOptionNames = [...dRepFilterOptions];
  while (multipleFilterOptionNames.length > 1) {
    await dRepDirectory.filterDReps(multipleFilterOptionNames);
    await dRepDirectory.validateFilters(
      multipleFilterOptionNames,
      dRepFilterOptions
    );
    await dRepDirectory.unFilterDReps(multipleFilterOptionNames);
    multipleFilterOptionNames.pop();
  }
});

test("2N. Should sort DReps", async ({ page }) => {
  test.slow();

  enum SortOption {
    RegistrationDate = "RegistrationDate",
    VotingPower = "VotingPower",
    Status = "Status",
  }

  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.sortBtn.click();

  await dRepDirectory.sortAndValidate(
    SortOption.RegistrationDate,
    (d1, d2) => d1.latestRegistrationDate >= d2.latestRegistrationDate
  );

  await dRepDirectory.sortAndValidate(
    SortOption.VotingPower,
    (d1, d2) => d1.votingPower >= d2.votingPower
  );

  await dRepDirectory.sortAndValidate(
    SortOption.Status,
    (d1, d2) => d1.status >= d2.status
  );
});
