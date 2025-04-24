import {
  proposal01Wallet,
  proposal02Wallet,
  user01Wallet,
} from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import { Page, expect } from "@playwright/test";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import {
  proposal01AuthFile,
  proposal02AuthFile,
  user01AuthFile,
} from "@constants/auth";

test.beforeEach(async () => {
  await setAllureEpic("8. Proposal Discussion Forum");
  await skipIfNotHardFork();
});

test.describe("Proposal created logged in state", () => {
  test.use({
    storageState: proposal02AuthFile,
    wallet: proposal02Wallet,
  });

  let proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;

  test.beforeEach(async ({ page, proposalId }) => {
    test.slow();
    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
    await proposalDiscussionDetailsPage.goto(proposalId);

    await proposalDiscussionDetailsPage.verifyIdentityBtn.click();
  });

  test("8G. Should display the proper likes and dislikes count", async ({
    page,
  }) => {
    await proposalDiscussionDetailsPage.likeBtn.click();
    await page.waitForTimeout(2_000);
    await expect(proposalDiscussionDetailsPage.likeCount).toHaveText("1");

    await proposalDiscussionDetailsPage.dislikeBtn.click();
    await page.waitForTimeout(2_000);
    await expect(proposalDiscussionDetailsPage.dislikeCount).toHaveText("1");
  });

  test("8J. Should sort the proposed governance action comments.", async ({
    page,
  }) => {
    for (let i = 0; i < 4; i++) {
      const comment = faker.lorem.words(5);
      await proposalDiscussionDetailsPage.addComment(comment);
      await page.waitForTimeout(2_000);
    }

    await proposalDiscussionDetailsPage.sortAndValidate(
      "asc",
      (date1, date2) => new Date(date1) <= new Date(date2)
    );
  });

  test("8N. Should reply to comments", async ({ page }) => {
    const randComment = faker.lorem.words(5);
    const randReply = faker.lorem.words(5);

    await proposalDiscussionDetailsPage.addComment(randComment);

    await proposalDiscussionDetailsPage.replyComment(randReply);
    const replyRendered = await page
      .locator(`[data-testid^="subcomment-"][data-testid$="-content"]`)
      .textContent();
    expect(replyRendered).toContain(randReply);
  });
});

test.describe("Proposal created with poll enabled (user auth)", () => {
  test.use({
    storageState: proposal02AuthFile,
    wallet: proposal02Wallet,
    pollEnabled: true,
  });

  let proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;

  test.beforeEach(async ({ page, proposalId }) => {
    test.slow();
    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
    await proposalDiscussionDetailsPage.goto(proposalId);
    await proposalDiscussionDetailsPage.verifyIdentityBtn.click();
  });

  test("8Q. Should vote on poll.", async ({ page }) => {
    const pollVotes = ["Yes", "No"];
    const choice = Math.floor(Math.random() * pollVotes.length);
    const vote = pollVotes[choice];

    await proposalDiscussionDetailsPage.voteOnPoll(vote);

    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
    await expect(
      page.getByTestId(`poll-${vote.toLowerCase()}-count`)
    ).toHaveText(`${vote}: (100%)`);
    // opposite of random choice vote
    const oppositeVote = pollVotes[pollVotes.length - 1 - choice];
    await expect(
      page.getByTestId(`poll-${oppositeVote.toLowerCase()}-count`)
    ).toHaveText(`${oppositeVote}: (0%)`);
  });

  test("8T. Should change vote on poll.", async ({ page }) => {
    const pollVotes = ["Yes", "No"];
    const choice = Math.floor(Math.random() * pollVotes.length);
    const vote = pollVotes[choice];

    await proposalDiscussionDetailsPage.voteOnPoll(vote);

    await proposalDiscussionDetailsPage.changeVoteBtn.click();
    await page.getByTestId("change-poll-vote-yes-button").click();

    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();

    // vote must be changed
    await expect(
      page.getByTestId(`poll-${vote.toLowerCase()}-count`)
    ).toHaveText(`${vote}: (0%)`, { timeout: 60_000 });
    // opposite of random choice vote
    const oppositeVote = pollVotes[pollVotes.length - 1 - choice];
    await expect(
      page.getByTestId(`poll-${oppositeVote.toLowerCase()}-count`)
    ).toHaveText(`${oppositeVote}: (100%)`);
  });
});

test.describe("Proposal created with poll enabled (proposal auth)", () => {
  test.use({
    storageState: user01AuthFile,
    wallet: user01Wallet,
    pollEnabled: true,
  });

  let ownerProposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;
  let proposalPage: Page;

  test.beforeEach(async ({ browser, proposalId }) => {
    test.slow();
    proposalPage = await createNewPageWithWallet(browser, {
      storageState: proposal01AuthFile,
      wallet: proposal01Wallet,
    });
    ownerProposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
      proposalPage
    );
    ownerProposalDiscussionDetailsPage.goto(proposalId);
    await ownerProposalDiscussionDetailsPage.verifyIdentityBtn.click();
  });

  test("8P. Should add poll on own proposal", async ({}) => {
    await expect(
      ownerProposalDiscussionDetailsPage.addPollBtn
    ).not.toBeVisible();
  });

  test("8R. Should disable voting after cancelling the poll with the current poll result.", async ({
    page,
  }) => {
    await ownerProposalDiscussionDetailsPage.closePollBtn.click();
    await ownerProposalDiscussionDetailsPage.closePollYesBtn.click();
    await expect(
      ownerProposalDiscussionDetailsPage.closePollBtn
    ).not.toBeVisible();

    // user
    const userProposalDetailsPage = new ProposalDiscussionDetailsPage(page);
    await expect(userProposalDetailsPage.pollYesBtn).not.toBeVisible();
    await expect(userProposalDetailsPage.pollNoBtn).not.toBeVisible();
  });

  test("8U. Should navigate to the edit proposal page when 'goto data edit screen' is selected if data does not match the anchor URL", async () => {
    const invalidMetadataAnchorUrl = "https://www.google.com";
    await ownerProposalDiscussionDetailsPage.submitAsGABtn.click();

    const proposalSubmissionPage = new ProposalSubmissionPage(proposalPage);
    await proposalPage.getByTestId("agree-checkbox").click();
    await proposalSubmissionPage.continueBtn.click();
    await proposalSubmissionPage.metadataUrlInput.fill(
      invalidMetadataAnchorUrl
    );
    await proposalSubmissionPage.submitBtn.click();

    await expect(
      proposalPage.getByTestId("data-not-match-modal")
    ).toBeVisible();
    await expect(
      proposalPage.getByTestId("data-not-match-modal-go-to-data-button")
    ).toBeVisible();

    await proposalPage
      .getByTestId("data-not-match-modal-go-to-data-button")
      .click();

    await expect(
      proposalPage.getByTestId("governance-action-type")
    ).toBeVisible();
    await expect(proposalPage.getByTestId("title-input")).toBeVisible();
    await expect(proposalPage.getByTestId("abstract-input")).toBeVisible();
    await expect(proposalPage.getByTestId("motivation-input")).toBeVisible();
    await expect(proposalPage.getByTestId("rationale-input")).toBeVisible();
  });
});
