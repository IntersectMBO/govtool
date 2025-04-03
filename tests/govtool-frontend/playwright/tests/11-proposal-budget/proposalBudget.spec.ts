import environments from "@constants/environments";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { injectLogger } from "@helpers/page";
import { extractProposalIdFromUrl } from "@helpers/string";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import BudgetDiscussionPage from "@pages/budgetDiscussionPage";
import { expect } from "@playwright/test";
import { BudgetProposalType } from "@types";

test.beforeEach(async ({}) => {
  await setAllureEpic("11. Proposal Budget");
});

test("11A. Should access budget proposal page in disconnect state", async ({
  page,
}) => {
  const budgetDiscussionPage = new BudgetDiscussionPage(page);
  await budgetDiscussionPage.goto();

  await expect(
    budgetDiscussionPage.currentPage.getByText("/Budget Proposals/i")
  ).toHaveCount(2);
});

test.describe("Budget proposal list manipulation", () => {
  test("11B_1. Should search for budget proposals by title", async ({
    page,
  }) => {
    let proposalName = "EchoFeed";
    let proposalNameSet = false;

    await page.route("**/api/bds?**", async (route) => {
      const response = await route.fetch();
      const json = await response.json();
      if (!proposalNameSet && "data" in json && json["data"].length > 0) {
        const randomIndex = Math.floor(Math.random() * json["data"].length);
        proposalName =
          json["data"][randomIndex]["attributes"]["bd_proposal_detail"]["data"][
            "attributes"
          ]["proposal_name"];
        proposalNameSet = true;
      }
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify(json),
      });
    });

    const responsePromise = page.waitForResponse("**/api/bds?**");
    const budgetDiscussionPage = new BudgetDiscussionPage(page);
    await budgetDiscussionPage.goto();

    await responsePromise;

    await budgetDiscussionPage.searchInput.fill(proposalName);

    await page.waitForTimeout(2000);

    await functionWaitedAssert(
      async () => {
        const proposalCards = await budgetDiscussionPage.getAllProposals();
        for (const proposalCard of proposalCards) {
          await expect(proposalCard).toBeVisible();
          const proposalTitle = await proposalCard
            .getByTestId("budget-discussion-title")
            .textContent();
          expect(proposalTitle.toLowerCase()).toContain(
            proposalName.toLowerCase()
          );
        }
      },
      {
        message: `A proposal card does not contain the search term ${proposalName}`,
      }
    );
  });

  test.describe("Filter and sort budget proposals", () => {
    let budgetDiscussionPage: BudgetDiscussionPage;

    test.beforeEach(async ({ page }) => {
      budgetDiscussionPage = new BudgetDiscussionPage(page);
      await budgetDiscussionPage.goto();
    });

    test("11B_2. Should filter budget proposals by categories", async () => {
      test.slow();
      await budgetDiscussionPage.filterBtn.click();

      // proposal type filter
      await budgetDiscussionPage.applyAndValidateFilters(
        Object.values(BudgetProposalType),
        budgetDiscussionPage._validateTypeFiltersInProposalCard
      );
    });

    test("11B_3. Should sort budget proposals", async () => {
      await budgetDiscussionPage.sortAndValidate(
        "asc",
        (p1, p2) => p1.attributes.createdAt <= p2.attributes.createdAt
      );

      await budgetDiscussionPage.sortAndValidate(
        "desc",
        (p1, p2) => p1.attributes.createdAt >= p2.attributes.createdAt
      );
    });
  });
});

test("11C. Should show view-all categorized budget proposal", async ({
  browser,
}) => {
  await Promise.all(
    Object.values(BudgetProposalType).map(async (proposalType: string) => {
      const context = await browser.newContext();
      const page = await context.newPage();
      injectLogger(page);

      const budgetDiscussionPage = new BudgetDiscussionPage(page);
      await budgetDiscussionPage.goto();
      const isShowAllButtonVisible = await page
        .waitForSelector(
          `[data-testid="${proposalType.toLowerCase().replace(/ /g, "-")}-show-all-button"]`,
          { timeout: 60_000 }
        )
        .then(() => true)
        .catch(() => false);

      if (isShowAllButtonVisible) {
        await page
          .getByTestId(
            proposalType.toLowerCase().replace(/ /g, "-") + "-show-all-button"
          )
          .click();

        const proposalCards = await budgetDiscussionPage.getAllProposals();

        for (const proposalCard of proposalCards) {
          await expect(
            proposalCard.getByTestId("budget-discussion-type")
          ).toHaveText(proposalType, { timeout: 60_000 });
        }
      } else {
        expect(true, `No ${proposalType} found`).toBeTruthy();
      }
    })
  );
});

test("11D. Should share budget proposal", async ({ page, context }) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  const budgetDiscussionPage = new BudgetDiscussionPage(page);
  await budgetDiscussionPage.goto();

  const budgetDiscussionDetailsPage =
    await budgetDiscussionPage.viewFirstProposal();

  const currentPageUrl = page.url();
  const proposalId = extractProposalIdFromUrl(currentPageUrl);

  await budgetDiscussionDetailsPage.shareBtn.click();
  await budgetDiscussionDetailsPage.copyLinkBtn.click();
  await expect(budgetDiscussionDetailsPage.copyLinkText).toBeVisible();

  const copiedTextDRepDirectory = await page.evaluate(() =>
    navigator.clipboard.readText()
  );
  const expectedCopyUrl = `${environments.frontendUrl}/budget_discussion/${proposalId}`;

  expect(copiedTextDRepDirectory).toEqual(expectedCopyUrl);
});

test("11E. Should view comments with count indications on a budget proposal", async () => {});

test.describe("Restricted access to interact budget proposal", () => {
  test("11F_1. Should restrict users without wallets from commenting", async () => {});
  test("11F_2. Should restrict users without wallets from voting", async () => {});
});

test("11G. Should sort the budget proposal comments", async ({}) => {});
