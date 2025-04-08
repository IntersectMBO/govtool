import { faker } from "@faker-js/faker";
import { test } from "@fixtures/budgetProposal";
import { setAllureEpic } from "@helpers/allure";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import { expect } from "@playwright/test";
import { dRep03Wallet } from "@constants/staticWallets";
import BudgetDiscussionPage from "@pages/budgetDiscussionPage";

test.beforeEach(async () => {
  await setAllureEpic("11. Proposal Budget");
});

test.describe("Budget proposal dRep behaviour", () => {
  test.use({
    storageState: ".auth/dRep03.json",
    wallet: dRep03Wallet,
  });

  test.describe("Budget proposal voting", () => {
    let budgetDiscussionDetailsPage: BudgetDiscussionDetailsPage;
    test.beforeEach(async ({ page, proposalId }) => {
      budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
      await budgetDiscussionDetailsPage.goto(proposalId);

      await budgetDiscussionDetailsPage.verifyIdentityBtn.click();
    });

    test("11K. Should allow registered DRep to vote on a proposal", async () => {
      const pollVotes = ["Yes", "No"];
      const choice = faker.helpers.arrayElement(pollVotes);

      await budgetDiscussionDetailsPage.voteOnPoll(choice);

      await expect(budgetDiscussionDetailsPage.pollYesBtn).not.toBeVisible();
      await expect(budgetDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
      await expect(
        budgetDiscussionDetailsPage.currentPage.getByTestId(
          `poll-${choice.toLowerCase()}-count`
        )
      ).toHaveText(`${choice}: (100%)`);
      // opposite of random choice vote
      const oppositeVote = pollVotes.filter((vote) => vote !== choice)[0];
      await expect(
        budgetDiscussionDetailsPage.currentPage.getByTestId(
          `poll-${oppositeVote.toLowerCase()}-count`
        )
      ).toHaveText(`${oppositeVote}: (0%)`);
    });

    test("11L. Should allow registered DRep to change vote on a proposal", async () => {
      test.slow();
      const pollVotes = ["Yes", "No"];
      const choice = faker.helpers.arrayElement(pollVotes);

      await budgetDiscussionDetailsPage.voteOnPoll(choice);
      await budgetDiscussionDetailsPage.changePollVote();

      await expect(budgetDiscussionDetailsPage.pollYesBtn).not.toBeVisible();
      await expect(budgetDiscussionDetailsPage.pollNoBtn).not.toBeVisible();

      // vote must be changed
      await expect(
        budgetDiscussionDetailsPage.currentPage.getByTestId(
          `poll-${choice.toLowerCase()}-count`
        )
      ).toHaveText(`${choice}: (0%)`, { timeout: 60_000 });
      // opposite of random choice vote
      const oppositeVote = pollVotes.filter((vote) => vote !== choice)[0];
      await expect(
        budgetDiscussionDetailsPage.currentPage.getByTestId(
          `poll-${oppositeVote.toLowerCase()}-count`
        )
      ).toHaveText(`${oppositeVote}: (100%)`);
    });
  });

  test("11M. Should display DRep tag, name and ID when a registered DRep comments on a proposal", async ({
    page,
  }) => {
    const comment = faker.lorem.paragraph(2);
    const budgetDiscussionPage = new BudgetDiscussionPage(page);
    await budgetDiscussionPage.goto();
    await budgetDiscussionPage.verifyIdentityBtn.click();
    const budgetDiscussionDetailsPage =
      await budgetDiscussionPage.viewFirstProposal();
    await budgetDiscussionDetailsPage.addComment(comment);

    await expect(
      budgetDiscussionDetailsPage.currentPage
        .locator('[data-testid^="comment-"][data-testid$="-content"]')
        .first()
    ).toHaveText(comment);

    const dRepCommentedCard = budgetDiscussionDetailsPage.currentPage
      .locator('[data-testid^="comment-"][data-testid$="-content-card"]')
      .first();

    await expect(
      dRepCommentedCard.getByText("DRep", { exact: true })
    ).toBeVisible();

    await expect(dRepCommentedCard.getByTestId("given-name")).toHaveText(
      dRep03Wallet.givenName
    );

    await expect(dRepCommentedCard.getByTestId("drep-id")).toHaveText(
      dRep03Wallet.dRepId
    );
  });
});
