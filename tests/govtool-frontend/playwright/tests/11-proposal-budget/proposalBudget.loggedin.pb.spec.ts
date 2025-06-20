import { budgetProposal01AuthFile } from "@constants/auth";
import { budgetProposal01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import BudgetDiscussionPage from "@pages/budgetDiscussionPage";
import { expect } from "@playwright/test";

test.beforeEach(async () => {
  await setAllureEpic("11. Proposal Budget");
});

test.describe("Budget proposal logged in state", () => {
  test.use({
    storageState: budgetProposal01AuthFile,
    wallet: budgetProposal01Wallet,
  });

  let budgetDiscussionDetailsPage: BudgetDiscussionDetailsPage;

  test.beforeEach(async ({ page }) => {
    const budgetDiscussionPage = new BudgetDiscussionPage(page);
    await budgetDiscussionPage.goto();
    await budgetDiscussionPage.verifyUserLink.click();
    budgetDiscussionDetailsPage =
      await budgetDiscussionPage.viewFirstProposal();
  });

  test("11G. Should sort the budget proposal comments", async ({ page }) => {
    for (let i = 0; i < 4; i++) {
      const comment = faker.lorem.words(5);
      await budgetDiscussionDetailsPage.addComment(comment);
      await page.waitForTimeout(2_000);
    }
    await budgetDiscussionDetailsPage.sortAndValidate(
      "asc",
      (date1, date2) => new Date(date1) <= new Date(date2)
    );
  });

  test("11H. Should restrict non registered DRep users from voting", async () => {
    // wait for the page to load
    await budgetDiscussionDetailsPage.currentPage.waitForTimeout(5_000);

    await expect(budgetDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
    await expect(budgetDiscussionDetailsPage.pollYesBtn).not.toBeVisible();

    await expect(budgetDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
  });

  test("11I. Should comments on any proposal", async ({}) => {
    await skipIfMainnet();

    const comment = faker.lorem.words(5);
    await budgetDiscussionDetailsPage.addComment(comment);
    await expect(
      budgetDiscussionDetailsPage.currentPage
        .locator('[data-testid^="comment-"][data-testid$="-content"]')
        .first()
    ).toHaveText(comment);
  });

  test("11J. Should reply to any comments", async ({}) => {
    await skipIfMainnet();

    const randComment = faker.lorem.words(5);
    const randReply = faker.lorem.words(5);

    await budgetDiscussionDetailsPage.addComment(randComment);

    await budgetDiscussionDetailsPage.replyComment(randReply);
    const replyRendered = await budgetDiscussionDetailsPage.currentPage
      .locator(`[data-testid^="subcomment-"][data-testid$="-content"]`)
      .textContent();
    expect(replyRendered).toContain(randReply);
  });
});
