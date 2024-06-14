import { user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
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
  }) => {
    const randomComment = faker.lorem.paragraph(2);
    await proposalDiscussionDetailsPage.commentInput.fill(randomComment);
    await proposalDiscussionDetailsPage.commentBtn.click();

    await expect(
      await proposalDiscussionDetailsPage.getFirstComment()
    ).toHaveText(/anonymous/i);
  });

  test("8N. Should reply to comments", async ({ page }) => {
    const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
      page
    );

    const randomComment = faker.lorem.paragraph(2);

    await proposalDiscussionDetailsPage.replyBtn.click();
    await proposalDiscussionDetailsPage.replyInput.fill(randomComment);

    await proposalDiscussionDetailsPage.showReplyButton.click();

    await expect(
      await proposalDiscussionDetailsPage.getFirstComment()
    ).toHaveText(randomComment);
  });
});
