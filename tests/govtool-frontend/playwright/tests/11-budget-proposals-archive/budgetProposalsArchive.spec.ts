import environments from "@constants/environments";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { injectLogger } from "@helpers/page";
import { extractProposalIdFromUrl } from "@helpers/string";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import BudgetDiscussionPage, {
  BUDGET_ARCHIVE_PATH,
  BUDGET_DISCUSSION_API,
} from "@pages/budgetDiscussionPage";
import { expect, Locator, Page } from "@playwright/test";
import { BudgetArchiveListItem, BudgetDiscussionEnum } from "@types";

// The 2025 budget proposals are a read-only archive served from static files
// under /budget-proposals-2025; nothing here calls a backend for them.

test.beforeEach(async ({}) => {
  await setAllureEpic("11. Budget Proposals Archive");
});

const recordBudgetDiscussionApiCalls = (page: Page) => {
  const calls: string[] = [];
  page.on("request", (request) => {
    if (BUDGET_DISCUSSION_API.test(request.url())) calls.push(request.url());
  });
  return calls;
};

const pickDiscussion = (
  items: BudgetArchiveListItem[],
  predicate: (item: BudgetArchiveListItem) => boolean
) => {
  const item = items.find(predicate);
  expect(item, "No archived budget proposal matches").toBeTruthy();
  return item;
};

test("11A. Should show the budget proposals archive in disconnect state", async ({
  page,
}) => {
  const apiCalls = recordBudgetDiscussionApiCalls(page);
  const budgetDiscussionPage = new BudgetDiscussionPage(page);
  await budgetDiscussionPage.goto();

  await expect(budgetDiscussionPage.archiveBanner).toBeVisible();
  await expect(
    page.getByRole("heading", { name: "2025 Budget Proposals" })
  ).toBeVisible();
  await expect(
    page.getByTestId("propose-a-budget-discussion-button")
  ).toHaveCount(0);
  expect(apiCalls).toEqual([]);
});

test("11A_2. Should open the archive from Useful links", async ({ page }) => {
  await page.goto("/");
  await page.getByTestId("useful-link-budgetProposalsArchive").click();

  await expect(page).toHaveURL(/\/budget_discussion$/);
  await expect(new BudgetDiscussionPage(page).archiveBanner).toBeVisible();
});

test.describe("Budget proposals archive list", () => {
  let budgetDiscussionPage: BudgetDiscussionPage;

  test.beforeEach(async ({ page }) => {
    budgetDiscussionPage = new BudgetDiscussionPage(page);
    await budgetDiscussionPage.goto();
  });

  test("11B_1. Should search budget proposals by title", async () => {
    const { items } = await budgetDiscussionPage.fetchArchiveList();
    const proposalName =
      items[Math.floor(Math.random() * items.length)].attributes
        .bd_proposal_detail.data.attributes.proposal_name;

    await budgetDiscussionPage.searchInput.fill(proposalName);

    await expect(async () => {
      const proposalCards = await budgetDiscussionPage.getAllProposals();
      for (const proposalCard of proposalCards) {
        const title = await proposalCard
          .getByTestId("budget-discussion-title")
          .textContent();
        expect(title.toLowerCase()).toContain(proposalName.toLowerCase());
      }
    }).toPass();
  });

  test("11B_2. Should filter budget proposals by categories", async () => {
    test.slow();
    await budgetDiscussionPage.filterBtn.click();

    await budgetDiscussionPage.applyAndValidateFilters(
      Object.values(BudgetDiscussionEnum),
      budgetDiscussionPage._validateTypeFiltersInProposalCard
    );
  });

  test("11B_3. Should sort budget proposals", async () => {
    test.slow();
    await budgetDiscussionPage.showAll(BudgetDiscussionEnum.Core);

    const title = (card: Locator) =>
      card.getByTestId("budget-discussion-title").innerText();
    const proposer = async (card: Locator) =>
      (await card.getByTestId("budget-discussion-creator").innerText()).replace(
        /^@/,
        ""
      );
    const comments = async (card: Locator) =>
      Number(await card.locator('[data-testid$="-comment-count"]').innerText());
    const proposedOn = async (card: Locator) =>
      Date.parse(await card.getByTestId("proposed-date").innerText());
    const ascending = (a: string, b: string) =>
      a.localeCompare(b, undefined, { sensitivity: "base" }) <= 0;
    const descending = (a: string, b: string) => ascending(b, a);

    await budgetDiscussionPage.sortAndValidate(
      "Oldest",
      proposedOn,
      (a, b) => a <= b
    );
    await budgetDiscussionPage.sortAndValidate(
      "Newest",
      proposedOn,
      (a, b) => a >= b
    );
    await budgetDiscussionPage.sortAndValidate(
      "Most comments",
      comments,
      (a, b) => a >= b
    );
    await budgetDiscussionPage.sortAndValidate(
      "Least comments",
      comments,
      (a, b) => a <= b
    );
    await budgetDiscussionPage.sortAndValidate("Name A-Z", title, ascending);
    await budgetDiscussionPage.sortAndValidate("Name Z-A", title, descending);
    await budgetDiscussionPage.sortAndValidate(
      "Proposer A-Z",
      proposer,
      ascending
    );
    await budgetDiscussionPage.sortAndValidate(
      "Proposer Z-A",
      proposer,
      descending
    );
  });
});

test("11C. Should show every proposal of a category on its category page", async ({
  browser,
}) => {
  await Promise.all(
    Object.values(BudgetDiscussionEnum).map(async (category) => {
      const context = await browser.newContext();
      const page = await context.newPage();
      injectLogger(page);

      const budgetDiscussionPage = new BudgetDiscussionPage(page);
      await budgetDiscussionPage.goto();
      const { items } = await budgetDiscussionPage.fetchArchiveList();
      const expectedType =
        category === BudgetDiscussionEnum.NoCategory
          ? "None of these"
          : category;
      const expectedCount = items.filter(
        (item) =>
          item.attributes.bd_psapb.data.attributes.type_name.data.attributes
            .type_name === expectedType
      ).length;

      await budgetDiscussionPage.showAll(category);
      await expect(page).toHaveURL(/\/budget_discussion\/category\//);
      await expect(budgetDiscussionPage.backToListBtn).toBeVisible();

      const proposalCards = await budgetDiscussionPage.getAllProposals();
      expect(proposalCards).toHaveLength(expectedCount);
      for (const proposalCard of proposalCards) {
        await expect(
          proposalCard.getByTestId("budget-discussion-type")
        ).toHaveText(expectedType);
      }
      await context.close();
    })
  );
});

test("11D. Should share an archived budget proposal", async ({
  page,
  context,
}) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  const budgetDiscussionPage = new BudgetDiscussionPage(page);
  await budgetDiscussionPage.goto();

  const budgetDiscussionDetailsPage =
    await budgetDiscussionPage.viewFirstProposal();
  await budgetDiscussionDetailsPage.titleContent.waitFor();
  const proposalId = extractProposalIdFromUrl(page.url());

  await budgetDiscussionDetailsPage.shareBtn.click();
  await budgetDiscussionDetailsPage.copyLinkBtn.click();
  await expect(budgetDiscussionDetailsPage.copyLinkText).toBeVisible();

  const copiedText = await page.evaluate(() => navigator.clipboard.readText());
  expect(copiedText).toEqual(
    `${environments.frontendUrl}/budget_discussion/${proposalId}`
  );
});

test.describe("Archived budget proposal details", () => {
  let budgetDiscussionPage: BudgetDiscussionPage;
  let budgetDiscussionDetailsPage: BudgetDiscussionDetailsPage;
  let apiCalls: string[];

  test.beforeEach(async ({ page }) => {
    apiCalls = recordBudgetDiscussionApiCalls(page);
    budgetDiscussionPage = new BudgetDiscussionPage(page);
    budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
  });

  test.afterEach(() => {
    expect(apiCalls).toEqual([]);
  });

  test("11E. Should show the comment count and every comment thread", async ({
    page,
  }) => {
    const { items } = await budgetDiscussionPage.fetchArchiveList();
    const discussion = pickDiscussion(
      items,
      (item) => item.attributes.prop_comments_number > 25
    );
    const responsePromise = page.waitForResponse(
      `**${BUDGET_ARCHIVE_PATH}/${discussion.attributes.master_id}.json`
    );
    await budgetDiscussionDetailsPage.goto(discussion.attributes.master_id);
    const { comments } = await (await responsePromise).json();

    const total = comments.length;
    await expect(budgetDiscussionDetailsPage.totalComments).toHaveText(
      total > 99 ? "99+" : `${total}`
    );

    const threads = comments.filter(
      (comment) => comment.attributes.comment_parent_id === null
    ).length;
    await expect(budgetDiscussionDetailsPage.commentCards).toHaveCount(
      Math.min(threads, 25)
    );
    while (await budgetDiscussionDetailsPage.loadMoreCommentsBtn.isVisible()) {
      await budgetDiscussionDetailsPage.loadMoreCommentsBtn.click();
    }
    await expect(budgetDiscussionDetailsPage.commentCards).toHaveCount(threads);
  });

  test("11F. Should be read-only", async () => {
    const { items } = await budgetDiscussionPage.fetchArchiveList();
    const discussion = pickDiscussion(
      items,
      (item) => item.attributes.prop_comments_number > 0 && !!item.archive.poll
    );
    await budgetDiscussionDetailsPage.goto(discussion.attributes.master_id);
    await expect(
      budgetDiscussionDetailsPage.commentCards.first()
    ).toBeVisible();

    await expect(budgetDiscussionDetailsPage.commentInput).toHaveCount(0);
    await expect(budgetDiscussionDetailsPage.commentBtn).toHaveCount(0);
    await expect(budgetDiscussionDetailsPage.replyBtn).toHaveCount(0);
    await expect(budgetDiscussionDetailsPage.pollYesBtn).toHaveCount(0);
    await expect(budgetDiscussionDetailsPage.pollNoBtn).toHaveCount(0);
    await expect(budgetDiscussionDetailsPage.menuButton).toHaveCount(0);
  });

  test("11G. Should show the final poll totals", async () => {
    const { items } = await budgetDiscussionPage.fetchArchiveList();
    const discussion = pickDiscussion(
      items,
      (item) =>
        item.archive.poll && item.archive.poll.yes + item.archive.poll.no > 0
    );
    const { yes, no } = discussion.archive.poll;
    await budgetDiscussionDetailsPage.goto(discussion.attributes.master_id);

    await expect(budgetDiscussionDetailsPage.pollTotalVotes).toHaveText(
      `Total votes: ${yes + no}`
    );
    await expect(budgetDiscussionDetailsPage.pollYesCount).toContainText(
      `Yes: ${yes}`
    );
    await expect(budgetDiscussionDetailsPage.pollNoCount).toContainText(
      `No: ${no}`
    );
  });

  test("11H. Should list every version", async ({ isMobile }) => {
    // On a small screen the version list sits behind its own button.
    test.skip(isMobile, "Desktop version list only");
    const { items } = await budgetDiscussionPage.fetchArchiveList();
    const discussion = pickDiscussion(
      items,
      (item) => item.archive.versions > 2
    );
    await budgetDiscussionDetailsPage.goto(discussion.attributes.master_id);

    await budgetDiscussionDetailsPage.reviewVersionsBtn.click();
    const dialog = budgetDiscussionDetailsPage.reviewVersionsDialog;
    await expect(dialog).toBeVisible();
    const versions = dialog.getByRole("listitem");
    await expect(versions).toHaveCount(discussion.archive.versions);
    await expect(versions.filter({ hasText: "(Final)" })).toHaveCount(1);
  });
});

test.describe("Old budget discussion links", () => {
  test("11I_1. Should open a proposal from its old link", async ({ page }) => {
    const budgetDiscussionPage = new BudgetDiscussionPage(page);
    const { items } = await budgetDiscussionPage.fetchArchiveList();
    const discussion = items[0];

    const budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
    await budgetDiscussionDetailsPage.goto(discussion.attributes.master_id);
    await expect(budgetDiscussionDetailsPage.titleContent).toHaveText(
      discussion.attributes.bd_proposal_detail.data.attributes.proposal_name
    );
  });

  test("11I_2. Should redirect the propose page to the archive", async ({
    page,
  }) => {
    await page.goto(`${environments.frontendUrl}/budget_discussion/propose`);
    await expect(page).toHaveURL(/\/budget_discussion$/);
  });

  test("11I_3. Should redirect an unknown proposal to the archive", async ({
    page,
  }) => {
    await page.goto(`${environments.frontendUrl}/budget_discussion/99999999`);
    await expect(page).toHaveURL(/\/budget_discussion$/);
  });
});
