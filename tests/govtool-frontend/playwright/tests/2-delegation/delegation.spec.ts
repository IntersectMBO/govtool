import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect, test } from "@playwright/test";
import { DRepStatus, IDRep } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfNotHardFork();
});

enum SortOption {
  Random = "Random",
  RegistrationDate = "RegistrationDate",
  VotingPower = "VotingPower",
  Status = "Status",
}

const statusRank: Record<DRepStatus, number> = {
  Active: 1,
  Inactive: 2,
  Retired: 3,
};

test("2K_2. Should sort DReps", async ({ page }) => {
  test.slow();

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
    (d1, d2) => statusRank[d1.status] <= statusRank[d2.status]
  );
});

test("2K_3. Should sort DReps randomly", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.sortBtn.click();

  await page.getByTestId(`${SortOption.RegistrationDate}-radio`).click();

  const dRepList1: IDRep[] = await dRepDirectory.getDRepsResponseFromApi(
    SortOption.Random
  );

  await page.getByTestId(`${SortOption.RegistrationDate}-radio`).click();

  const dRepList2: IDRep[] = await dRepDirectory.getDRepsResponseFromApi(
    SortOption.Random
  );

  // Extract dRepIds from both lists
  const dRepIdsList1 = dRepList1.map((dRep) => dRep.drepId);
  const dRepIdsList2 = dRepList2.map((dRep) => dRep.drepId);

  expect(dRepList1.length).toEqual(dRepList2.length);

  const isOrderDifferent = dRepIdsList1.some(
    (id, index) => id !== dRepIdsList2[index]
  );

  expect(isOrderDifferent).toBe(true);
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
