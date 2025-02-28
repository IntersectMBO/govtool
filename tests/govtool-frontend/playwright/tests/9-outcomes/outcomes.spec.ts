import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import OutComesPage from "@pages/outcomesPage";
import { expect, Page } from "@playwright/test";
import { outcomeMetadata, outcomeProposal, outcomeType } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("9. Outcomes");
  await skipIfNotHardFork();
});

const status = ["Expired", "Ratified", "Enacted", "Live"];

enum SortOption {
  SoonToExpire = "Soon to expire",
  NewestFirst = "Newest first",
  OldestFirst = "Oldest first",
  HighestAmountYesVote = "Highest amount of yes votes",
}
test("9A. Should access Outcomes page in disconnect state", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("governance-actions-outcomes-link").click();

  await expect(page.getByText(/outcomes/i)).toHaveCount(2);
});

test.describe("outcome details dependent test", () => {
  let governanceActionId: string | undefined;
  let governanceActionTitle: string | undefined;
  let currentPage: Page;
  test.beforeEach(async ({ page }) => {
    // intercept outcomes data for id
    await page.route(
      "**/governance-actions?search=&filters=&sort=**",
      async (route) => {
        const response = await route.fetch();
        const data: outcomeProposal[] = await response.json();
        if (!governanceActionId) {
          if (data.length > 0) {
            const randomIndexForId = Math.floor(Math.random() * data.length);
            governanceActionId =
              data[randomIndexForId].tx_hash +
              "#" +
              data[randomIndexForId].index;
          }
        }
        await route.fulfill({
          status: 200,
          contentType: "application/json",
          body: JSON.stringify(data),
        });
      }
    );

    // intercept metadata for title
    await page.route("**/governance-actions/metadata?**", async (route) => {
      try {
        const response = await route.fetch();
        if (response.status() !== 200) {
          await route.continue();
          return;
        }
        const data: outcomeMetadata = await response.json();
        if (!governanceActionTitle && data.body.title != null) {
          governanceActionTitle = data.body.title;
        }
        await route.fulfill({
          status: 200,
          contentType: "application/json",
          body: JSON.stringify(data),
        });
      } catch (error) {
        return;
      }
    });

    const responsePromise = page.waitForResponse(
      "**/governance-actions?search=&filters=&sort=**"
    );
    const metadataResponsePromise = page.waitForResponse(
      "**/governance-actions/metadata?**"
    );

    const outcomesPage = new OutComesPage(page);
    await outcomesPage.goto();

    await responsePromise;
    await metadataResponsePromise;
    currentPage = page;
  });

  test("9B. Should search outcomes proposal by title and id", async ({}) => {
    const outcomesPage = new OutComesPage(currentPage);
    // search by id
    await outcomesPage.searchInput.fill(governanceActionId);
    await expect(
      currentPage.getByRole("progressbar").getByRole("img")
    ).toBeVisible();

    await functionWaitedAssert(
      async () => {
        const idSearchOutcomeCards = await outcomesPage.getAllOutcomes();
        for (const outcomeCard of idSearchOutcomeCards) {
          const id = await outcomeCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent();
          expect(id.replace(/^.*ID/, "")).toContain(governanceActionId);
        }
      },
      { name: "search by id" }
    );

    // search by title
    await outcomesPage.searchInput.fill(governanceActionTitle);
    await expect(
      currentPage.getByRole("progressbar").getByRole("img")
    ).toBeVisible();

    await functionWaitedAssert(
      async () => {
        const titleSearchOutcomeCards = await outcomesPage.getAllOutcomes();
        for (const outcomeCard of titleSearchOutcomeCards) {
          const title = await outcomeCard
            .locator('[data-testid$="-card-title"]')
            .textContent();
          expect(title).toContain(governanceActionTitle);
        }
      },
      { name: "search by title" }
    );
  });

  test("9D. Should copy governanceActionId", async ({ page, context }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);
    const outcomesPage = new OutComesPage(currentPage);

    await outcomesPage.searchInput.fill(governanceActionId);
    await expect(
      currentPage.getByRole("progressbar").getByRole("img")
    ).toBeVisible();
    await page
      .getByTestId(`${governanceActionId}-CIP-105-id`)
      .getByTestId("copy-button")
      .click();
    await expect(page.getByText("Copied to clipboard")).toBeVisible({
      timeout: 10_000,
    });
    const copiedTextDRepDirectory = await page.evaluate(() =>
      navigator.clipboard.readText()
    );
    expect(copiedTextDRepDirectory).toEqual(governanceActionId);
  });
});

test("9C_1. Should filter Governance Action Type on governance actions page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.filterBtn.click();
  const filterOptionNames = Object.values(outcomeType);

  // proposal type filter
  await outcomePage.applyAndValidateFilters(
    filterOptionNames,
    outcomePage._validateFiltersInOutcomeCard
  );

  // proposal status filter
  await outcomePage.applyAndValidateFilters(
    status,
    outcomePage._validateStatusFiltersInOutcomeCard
  );
});

test("9C_2. Should sort Governance Action Type on outcomes page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.sortBtn.click();

  await outcomePage.sortAndValidate(
    SortOption.OldestFirst,
    (p1, p2) => p1.expiry_date <= p2.expiry_date
  );

  await outcomePage.sortAndValidate(
    SortOption.NewestFirst,
    (p1, p2) => p1.expiry_date >= p2.time
  );

  await outcomePage.sortAndValidate(
    SortOption.HighestAmountYesVote,
    (p1, p2) => parseInt(p1.yes_votes) >= parseInt(p2.yes_votes)
  );
});

test("9C_3. Should filter and sort Governance Action Type on outcomes page", async ({
  page,
}) => {
  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.filterBtn.click();
  const filterOptionNames = Object.values(outcomeType);

  const choice = Math.floor(Math.random() * filterOptionNames.length);
  await outcomePage.filterProposalByNames([filterOptionNames[choice]]);

  await outcomePage.filterBtn.click({ force: true });
  await outcomePage.sortBtn.click();

  await outcomePage.sortAndValidate(
    SortOption.OldestFirst,
    (p1, p2) => p1.expiry_date <= p2.expiry_date
  );

  await outcomePage.validateFilters(
    [filterOptionNames[choice]],
    outcomePage._validateFiltersInOutcomeCard
  );
});
