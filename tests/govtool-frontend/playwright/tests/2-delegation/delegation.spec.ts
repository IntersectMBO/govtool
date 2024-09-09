import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect, test } from "@playwright/test";
import { DRepStatus } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfNotHardFork();
});

test("2K_2. Should sort DReps", async ({ page }) => {
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

test("2O. Should load more DReps on show more", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  const dRepIdsBefore = await dRepDirectory.getAllListedDRepIds();
  await dRepDirectory.showMoreBtn.click();

  const dRepIdsAfter = await dRepDirectory.getAllListedDRepIds();
  expect(dRepIdsAfter.length).toBeGreaterThanOrEqual(dRepIdsBefore.length);

  if (dRepIdsAfter.length > dRepIdsBefore.length) {
    await expect(dRepDirectory.showMoreBtn).toBeVisible();
    expect(true).toBeTruthy();
  } else {
    await expect(dRepDirectory.showMoreBtn).not.toBeVisible();
  }
});

test("2K_1. Should filter DReps", async ({ page }) => {
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
