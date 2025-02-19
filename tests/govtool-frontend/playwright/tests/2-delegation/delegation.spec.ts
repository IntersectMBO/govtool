import environments from "@constants/environments";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { fetchFirstActiveDRepDetails } from "@helpers/dRep";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import DRepDetailsPage from "@pages/dRepDetailsPage";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect, Locator, test } from "@playwright/test";
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
  const responsePromise = page.waitForResponse((response) =>
    response
      .url()
      .includes(`drep/list?page=1&pageSize=10&sort=${SortOption.Random}`)
  );
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  let dRepIdsBefore: Locator[];
  let dRepIdsAfter: Locator[];

  await functionWaitedAssert(
    async () => {
      dRepIdsBefore = await dRepDirectory.getAllListedCIP105DRepIds();
      await dRepDirectory.showMoreBtn.click();
    },
    { message: "Show more button not visible" }
  );

  const response = await responsePromise;
  const json = await response.json();
  const dRepListAfter = json.elements;

  await functionWaitedAssert(
    async () => {
      dRepIdsAfter = await dRepDirectory.getAllListedCIP105DRepIds();
      expect(dRepIdsAfter.length).toBeGreaterThanOrEqual(dRepIdsBefore.length);
    },
    { message: "DReps not loaded after clicking show more" }
  );

  if (dRepListAfter.length >= dRepIdsBefore.length) {
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

test("2M. Should access dRep directory page on disconnected state", async ({
  page,
}) => {
  const dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();

  const dRepCards = await dRepDirectoryPage.getAllListedDReps();
  expect(dRepCards.length).toBeGreaterThan(1);
});

test.describe("DRep dependent tests", () => {
  let dRepId: string;
  let dRepDirectoryPage: DRepDirectoryPage;

  test.beforeEach(async ({ page }) => {
    ({ dRepDirectoryPage, dRepId } = await fetchFirstActiveDRepDetails(page));
  });

  test("2P. Should enable sharing of DRep details", async ({
    page,
    context,
  }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);

    const dRepDetailsPage = new DRepDetailsPage(page);
    await dRepDetailsPage.goto(dRepId);

    await dRepDetailsPage.shareLink();
    await expect(page.getByText("Copied to clipboard")).toBeVisible();

    const copiedText = await page.evaluate(() =>
      navigator.clipboard.readText()
    );
    expect(copiedText).toEqual(
      `${environments.frontendUrl}/drep_directory/${dRepId}`
    );
  });

  test("2Q. Should include DRep status and voting power on the DRep card", async ({
    page,
  }) => {
    await dRepDirectoryPage.searchInput.fill(dRepId);
    const dRepCard = dRepDirectoryPage.getDRepCard(dRepId);

    await expect(dRepCard.getByTestId(`${dRepId}-voting-power`)).toBeVisible();
    await expect(
      dRepCard.locator(`[data-testid^="${dRepId}-"][data-testid$="-pill"]`)
    ).toBeVisible();
  });

  test("2C. Should open wallet connection popup on delegate in disconnected state", async ({
    page,
  }) => {
    await page.getByTestId("search-input").fill(dRepId);
    await page.getByTestId(`${dRepId}-connect-to-delegate-button`).click();
    await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
  });

  test("2L. Should copy DRepId", async ({ page, context }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);

    await dRepDirectoryPage.searchInput.fill(dRepId);
    await page.getByTestId(`${dRepId}-copy-id-button`).click();
    await expect(page.getByText("Copied to clipboard")).toBeVisible({
      timeout: 10_000,
    });
    const copiedTextDRepDirectory = await page.evaluate(() =>
      navigator.clipboard.readText()
    );
    expect(copiedTextDRepDirectory).toEqual(dRepId);
  });
});
