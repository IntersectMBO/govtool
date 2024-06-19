import { proposal01Wallet, user01Wallet } from "@constants/staticWallets";
import { createTempUserAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { Page, expect } from "@playwright/test";

test.describe("Proposal created logged in state", () => {
  test.use({
    storageState: ".auth/user01.json",
    wallet: user01Wallet,
  });

  let proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;

  test.beforeEach(async ({ page, proposalId }) => {
    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
    await proposalDiscussionDetailsPage.goto(proposalId);
    await proposalDiscussionDetailsPage.closeUsernamePrompt();
  });

  test("8G. Should display the proper likes and dislikes count", async ({
    page,
  }) => {
    await proposalDiscussionDetailsPage.likeBtn.click();
    await page.waitForTimeout(2_000);
    await expect(page.getByText("10")).toBeVisible();

    await proposalDiscussionDetailsPage.dislikeBtn.click();
    await page.waitForTimeout(2_000);
    await expect(page.getByText("01", { exact: true })).toBeVisible();
  });

  test("8J. Should sort the proposed governance action comments.", async ({
    page,
  }) => {
    for (let i = 0; i < 4; i++) {
      const comment = faker.lorem.paragraph(2);
      await proposalDiscussionDetailsPage.addComment(comment);
      await page.waitForTimeout(2_000);
    }

    await proposalDiscussionDetailsPage.sortAndValidate(
      "asc",
      (date1, date2) => new Date(date1) <= new Date(date2)
    );
  });

  test("8M. Should comment anonymously if a username is not set", async ({
    page,
  }) => {
    const randComment = faker.lorem.paragraph(2);
    await proposalDiscussionDetailsPage.addComment(randComment);

    await expect(page.getByText(randComment)).toBeVisible();
  });

  test("8N. Should reply to comments", async ({ page }) => {
    const randComment = faker.lorem.paragraph(2);
    const randReply = faker.lorem.paragraph(2);

    await proposalDiscussionDetailsPage.addComment(randComment);

    await proposalDiscussionDetailsPage.replyComment(randReply);
    await expect(page.getByText(randReply)).toBeVisible();
  });
});

test.describe("Proposal created with poll enabled (user auth)", () => {
  test.use({
    storageState: ".auth/user01.json",
    wallet: user01Wallet,
    pollEnabled: true,
  });

  let proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;

  test.beforeEach(async ({ page, proposalId, browser }) => {
    const proposalPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal01.json",
      wallet: proposal01Wallet,
    });

    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
    await proposalDiscussionDetailsPage.goto(proposalId);
    await proposalDiscussionDetailsPage.closeUsernamePrompt();
  });
  test("8Q. Should vote on poll.", async ({ page }) => {
    const pollVotes = ["Yes", "No"];
    const choice = Math.floor(Math.random() * pollVotes.length);
    const vote = pollVotes[choice];

    await proposalDiscussionDetailsPage.voteOnPoll(vote);

    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
    await expect(page.getByText(`${vote}: (100%)`)).toBeVisible();
    // opposite of random choice vote
    const oppositeVote = pollVotes[pollVotes.length - 1 - choice];
    await expect(page.getByText(`${oppositeVote}: (0%)`)).toBeVisible();
  });

  test("8T. Should change vote on poll.", async ({ page }) => {
    const pollVotes = ["Yes", "No"];
    const choice = Math.floor(Math.random() * pollVotes.length);
    const vote = pollVotes[choice];

    await proposalDiscussionDetailsPage.voteOnPoll(vote);

    await proposalDiscussionDetailsPage.changeVoteBtn.click();
    await page
      .getByRole("button", { name: "Yes, change my Poll Vote" })
      .click();

    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();

    // vote must be changed
    await expect(page.getByText(`${vote}: (0%)`)).toBeVisible();
    // opposite of random choice vote
    const oppositeVote = pollVotes[pollVotes.length - 1 - choice];
    await expect(page.getByText(`${oppositeVote}: (100%)`)).toBeVisible();
  });
});

test.describe("Proposal created logged out state", () => {
  let userPage: Page;

  test.beforeEach(async ({ page, browser }) => {
    const wallet = (await ShelleyWallet.generate()).json();
    const tempUserAuth = await createTempUserAuth(page, wallet);

    userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet,
    });
  });

  test("8O. Should update anonymous username to set username in comments", async ({
    proposalId,
  }) => {
    test.slow();

    const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
      userPage
    );
    await proposalDiscussionDetailsPage.goto(proposalId);
    await proposalDiscussionDetailsPage.closeUsernamePrompt();

    const randComment = faker.lorem.paragraph(2);
    await proposalDiscussionDetailsPage.addComment(randComment);

    await expect(userPage.getByText(/anonymous/i)).toBeVisible();

    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();

    const userName = faker.internet.userName();
    await proposalDiscussionPage.setUsername(userName);
    await proposalDiscussionDetailsPage.goto(proposalId);

    await expect(userPage.getByText(/anonymous/i)).not.toBeVisible();
    await expect(userPage.getByText(userName)).toBeVisible();
  });
});

test.describe("Proposal created with poll enabled (proposal auth)", () => {
  test.use({
    storageState: ".auth/user01.json",
    wallet: user01Wallet,
    pollEnabled: true,
  });

  let proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;

  test.beforeEach(async ({ browser, proposalId }) => {
    const proposalPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal01.json",
      wallet: proposal01Wallet,
    });
    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
      proposalPage
    );
    proposalDiscussionDetailsPage.goto(proposalId);
  });

  test("8P. Should add poll on own proposal", async ({}) => {
    await expect(proposalDiscussionDetailsPage.addPollBtn).not.toBeVisible();
  });

  test("8R. Should disable voting after cancelling the poll with the current poll result.", async ({
    page,
  }) => {
    await proposalDiscussionDetailsPage.closePollBtn.click();
    await proposalDiscussionDetailsPage.closePollYesBtn.click();
    await expect(proposalDiscussionDetailsPage.closePollBtn).not.toBeVisible();

    // user
    const userProposalDetailsPage = new ProposalDiscussionDetailsPage(page);
    await expect(userProposalDetailsPage.pollYesBtn).not.toBeVisible();
    await expect(userProposalDetailsPage.pollNoBtn).not.toBeVisible();
  });
});
