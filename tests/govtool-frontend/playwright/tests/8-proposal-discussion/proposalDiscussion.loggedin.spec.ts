import { user01Wallet } from "@constants/staticWallets";
import { createTempUserAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { Page, expect } from "@playwright/test";

test.describe("Proposal created logged in state", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

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
