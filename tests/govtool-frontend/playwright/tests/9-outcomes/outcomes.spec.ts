import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import OutComesPage from "@pages/outcomesPage";
import { expect } from "@playwright/test";
import { GovernanceActionType, outcomeProposal } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("9. Outcomes");
  await skipIfNotHardFork();
});

const filterOptionNames = [
  "Protocol Parameter Change",
  "Update Committee",
  "Hard-Fork Initiation",
  "Motion of no Confidence",
  "Info",
  "Treasury Withdrawals",
  "New Constitution",
];

const status = ["Expired", "Ratified", "Enacted"];

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

test("9B. Should search outcomes proposal by title and id", async ({
  page,
}) => {
  let governanceActionId: string | undefined;
  let governanceActionTitle: string | undefined;

  await page.route("**/api/governance-actions?search=&sort=", async (route) => {
    const response = await route.fetch();
    const data = await response.json();
    if (!governanceActionTitle) {
      const elementsWithTitle: outcomeProposal[] = data.filter(
        (element: outcomeProposal) => element.title != null
      );
      if (elementsWithTitle.length > 0) {
        const randomIndex = Math.floor(
          Math.random() * elementsWithTitle.length
        );
        governanceActionId = elementsWithTitle[randomIndex].tx_hash + "#";
        governanceActionTitle = elementsWithTitle[randomIndex].title;
      }
    }
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(data),
    });
  });

  const responsePromise = page.waitForResponse(
    "**/api/governance-actions?search=&sort="
  );

  const outcomesPage = new OutComesPage(page);
  await outcomesPage.goto();

  await responsePromise;
  // search by title
  await outcomesPage.searchInput.fill(governanceActionTitle);

  await expect(page.getByRole("progressbar").getByRole("img")).toBeVisible();

  await expect(outcomesPage.viewDetailsBtn.first()).toBeVisible();
  const titleContentListLength = (
    await page.getByText(governanceActionTitle).all()
  ).length;
  expect(titleContentListLength).toBeGreaterThanOrEqual(1);

  // search by id
  await outcomesPage.searchInput.fill(governanceActionId);
  await expect(page.getByRole("progressbar").getByRole("img")).toBeVisible();

  await expect(outcomesPage.viewDetailsBtn.first()).toBeVisible();
  const idContentListLength = (await page.getByText(governanceActionId).all())
    .length;
  expect(idContentListLength).toBeGreaterThanOrEqual(1);
});

test("9C_1. Should filter Governance Action Type on governance actions page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.filterBtn.click();

  // proposal type filter
  await outcomePage.applyAndValidateFilters(
    filterOptionNames,
    outcomePage._validateFiltersInOutcomeCard
  );

  // proposal status filter
  await outcomePage.applyAndValidateFilters(
    status,
    outcomePage._validateFiltersInOutcomeCard
  );
});

test("9C_2. Should sort Governance Action Type on outcomes page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.sortBtn.click();
  await outcomePage.clearBtn.click();

  await outcomePage.sortAndValidate(
    SortOption.NewestFirst,
    (p1, p2) => p1.expiry_date >= p2.time
  );

  await outcomePage.sortAndValidate(
    SortOption.OldestFirst,
    (p1, p2) => p1.expiry_date <= p2.expiry_date
  );

  await outcomePage.sortAndValidate(
    SortOption.HighestAmountYesVote,
    (p1, p2) => parseInt(p1.yes_votes) >= parseInt(p2.yes_votes)
  );
});

test("9C_3. Should filter and sort Governance Action Type on outcomes page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.filterBtn.click();

  const choice = Math.floor(Math.random() * filterOptionNames.length);
  await outcomePage.filterProposalByNames([filterOptionNames[choice]]);

  await outcomePage.sortBtn.click({ force: true });
  await outcomePage.clearBtn.click();

  await outcomePage.sortAndValidate(
    SortOption.NewestFirst,
    (p1, p2) => p1.expiry_date >= p2.expiry_date,
    Object.values(GovernanceActionType)[choice]
  );

  await outcomePage.validateFilters(
    [filterOptionNames[choice]],
    outcomePage._validateFiltersInOutcomeCard
  );
});
