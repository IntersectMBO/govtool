import { user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test("8G. Should display the proper likes and dislikes count", async ({
  proposalDiscussionDetailsPage,
}) => {
  await proposalDiscussionDetailsPage.likeBtn.click();

  await expect(proposalDiscussionDetailsPage.likesCounts).toHaveValue("1");
  await expect(proposalDiscussionDetailsPage.dislikesCounts).toHaveValue("0");

  await proposalDiscussionDetailsPage.dislikeBtn.click();

  await expect(proposalDiscussionDetailsPage.likesCounts).toHaveValue("0");
  await expect(proposalDiscussionDetailsPage.dislikesCounts).toHaveValue("1");
});

test("8M. Should comment anonymously if a username is not set", async ({
  proposalDiscussionDetailsPage,
}) => {
  const randomComment = faker.lorem.paragraph(2);
  await proposalDiscussionDetailsPage.commentInput.fill(randomComment);
  await proposalDiscussionDetailsPage.commentBtn.click();

  await expect(
    await proposalDiscussionDetailsPage.getFirstComment()
  ).toHaveText(/anonymous/i);
});

test("8N. Should reply to comments", async ({
  proposalDiscussionDetailsPage,
}) => {
  const randomComment = faker.lorem.paragraph(2);

  await proposalDiscussionDetailsPage.replyBtn.click();
  await proposalDiscussionDetailsPage.replyInput.fill(randomComment);

  await proposalDiscussionDetailsPage.showReplyButton.click();

  await expect(
    await proposalDiscussionDetailsPage.getFirstComment()
  ).toHaveText(randomComment);
});
