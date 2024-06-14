import { user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import { expect } from "@playwright/test";

test.describe("Proposal created state", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

  test("8G. Should display the proper likes and dislikes count", async ({
    proposalDiscussionDetailsPage,
  }) => {
    await proposalDiscussionDetailsPage.likeBtn.click();
    await proposalDiscussionDetailsPage.currentPage.waitForTimeout(2_000);
    await expect(
      proposalDiscussionDetailsPage.currentPage.getByText("10")
    ).toBeVisible();

    await proposalDiscussionDetailsPage.dislikeBtn.click();
    await proposalDiscussionDetailsPage.currentPage.waitForTimeout(2_000);
    await expect(
      proposalDiscussionDetailsPage.currentPage.getByText("01", { exact: true })
    ).toBeVisible();
  });

  test("8M. Should comment anonymously if a username is not set", async ({
    proposalDiscussionDetailsPage,
    page,
  }) => {
    const randComment = faker.lorem.paragraph(2);
    await proposalDiscussionDetailsPage.addComment(randComment);

    await expect(page.getByText(randComment)).toBeVisible();
  });

  test("8N. Should reply to comments", async ({
    proposalDiscussionDetailsPage,
    page,
  }) => {
    const randComment = faker.lorem.paragraph(2);
    const randReply = faker.lorem.paragraph(2);

    await proposalDiscussionDetailsPage.addComment(randComment);

    await proposalDiscussionDetailsPage.replyComment(randReply);
    await expect(page.getByText(randReply)).toBeVisible();
  });
});
