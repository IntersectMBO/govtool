import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });


test("8G. Should display the proper likes and dislikes count.", async ({
  proposalDiscussionDetailsPage,
}) => {
  await proposalDiscussionDetailsPage.goto();

  await proposalDiscussionDetailsPage.likeBtn.click();

  await expect(proposalDiscussionDetailsPage.likesCounts).toHaveValue("1");
  await expect(proposalDiscussionDetailsPage.dislikesCounts).toHaveValue("0");

  await proposalDiscussionDetailsPage.dislikeBtn.click();

  await expect(proposalDiscussionDetailsPage.likesCounts).toHaveValue("0");
  await expect(proposalDiscussionDetailsPage.dislikesCounts).toHaveValue("1");
});
