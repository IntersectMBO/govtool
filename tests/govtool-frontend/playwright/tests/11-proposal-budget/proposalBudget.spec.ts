import environments from "@constants/environments";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { injectLogger } from "@helpers/page";
import { extractProposalIdFromUrl } from "@helpers/string";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import BudgetDiscussionPage from "@pages/budgetDiscussionPage";
import { expect } from "@playwright/test";
import { BudgetDiscussionEnum, CommentResponse } from "@types";

const mockBudgetProposal = require("../../lib/_mock/budgetProposal.json");
const mockPoll = require("../../lib/_mock/budgetProposalPoll.json");
const mockComments = require("../../lib/_mock/budgetProposalComments.json");

test.beforeEach(async ({}) => {
  await setAllureEpic("11. Proposal Budget");
});

test("11A. Should access budget proposal page in disconnect state", async ({
  page,
}) => {
  const budgetDiscussionPage = new BudgetDiscussionPage(page);
  await budgetDiscussionPage.goto();

  await expect(
    budgetDiscussionPage.currentPage.getByText(/Budget Proposals/i)
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
        Object.values(BudgetDiscussionEnum),
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
    Object.values(BudgetDiscussionEnum).map(async (proposalType: string) => {
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
          const ExpectedProposalType =
            proposalType === BudgetDiscussionEnum.NoCategory
              ? "None of these"
              : proposalType;
          await expect(
            proposalCard.getByTestId("budget-discussion-type")
          ).toHaveText(ExpectedProposalType, { timeout: 60_000 });
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

test("11E. Should view comments with count indications on a budget proposal", async ({
  page,
}) => {
  let responsePromise = page.waitForResponse((response) =>
    response.url().includes(`/api/bds/`)
  );

  const budgetDiscussionPage = new BudgetDiscussionPage(page);
  await budgetDiscussionPage.goto();

  const budgetDiscussionDetailsPage =
    await budgetDiscussionPage.viewFirstProposal();
  const response = await responsePromise;

  const proposalResponse = await response.json();

  const actualTotalComments =
    await budgetDiscussionDetailsPage.totalComments.textContent();
  const expectedTotalComments =
    proposalResponse.data.attributes.prop_comments_number.toString();
  const isEqual = actualTotalComments === expectedTotalComments;

  const currentPageUrl = budgetDiscussionDetailsPage.currentPage.url();

  const proposalId = extractProposalIdFromUrl(currentPageUrl);

  await expect(
    budgetDiscussionDetailsPage.totalComments,
    !isEqual &&
      `Total comments do not match in ${environments.frontendUrl}/budget_discussion/${proposalId}`
  ).toHaveText(expectedTotalComments);
});

test.describe("Restricted access to interact budget proposal", () => {
  let budgetDiscussionDetailsPage: BudgetDiscussionDetailsPage;

  test.beforeEach(async ({ page }) => {
    await page.route("**/api/bds/**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockBudgetProposal),
      })
    );

    await page.route("**/api/bd-polls**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockPoll),
      })
    );

    await page.route("**/api/comments**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockComments),
      })
    );

    budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
    await budgetDiscussionDetailsPage.goto(mockBudgetProposal.data.id);
  });
  test("11F_1. Should restrict users without wallets from commenting", async () => {
    await budgetDiscussionDetailsPage.commentInput.fill(
      faker.lorem.paragraph()
    );

    await expect(budgetDiscussionDetailsPage.commentBtn).toBeDisabled();
  });
  test("11F_2. Should restrict users without wallets from voting", async () => {
    // wait for the page to load
    await budgetDiscussionDetailsPage.currentPage.waitForTimeout(5_000);
    await expect(budgetDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
    await expect(budgetDiscussionDetailsPage.pollYesBtn).not.toBeVisible();

    await expect(budgetDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
  });
});
