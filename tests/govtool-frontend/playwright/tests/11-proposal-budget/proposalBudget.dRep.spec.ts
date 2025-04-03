import { createTempDRepAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import BudgetDiscussionPage from "@pages/budgetDiscussionPage";
import { expect, Page } from "@playwright/test";
import walletManager from "lib/walletManager";
import { valid as mockValid } from "@mock/index";

test.beforeEach(async () => {
  await setAllureEpic("11. Proposal Budget");
});

test.describe("Budget proposal dRep behaviour", () => {
  let budgetDiscussionDetailsPage: BudgetDiscussionDetailsPage;
  let dRepId: string;
  test.beforeEach(async ({ browser, page }) => {
    const wallet = await walletManager.popWallet("registeredDRep");
    dRepId = wallet.dRepId;

    const tempDRepAuth = await createTempDRepAuth(page, wallet);

    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet,
    });
    const budgetDiscussionPage = new BudgetDiscussionPage(dRepPage);
    await budgetDiscussionPage.goto();
    await budgetDiscussionPage.verifyIdentityBtn.click();
    await budgetDiscussionPage.setUsername(mockValid.username());
    budgetDiscussionDetailsPage =
      await budgetDiscussionPage.viewFirstProposal();
    await expect(budgetDiscussionDetailsPage.pollVoteCard).toBeVisible({
      timeout: 60_000,
    });
  });

  test("11K. Should allow registered DRep to vote on a proposal", async () => {
    const pollVotes = ["Yes", "No"];
    const choice = faker.helpers.arrayElement(pollVotes);
    console.log(choice);
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
    const pollVotes = ["Yes", "No"];
    const choice = faker.helpers.arrayElement(pollVotes);
    console.log(choice);
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
  test("11M. Should display DRep tag, name and ID when a registered DRep comments on a proposal", async ({}) => {});
});
