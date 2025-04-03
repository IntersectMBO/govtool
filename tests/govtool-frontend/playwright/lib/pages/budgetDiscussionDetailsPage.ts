import { expect, Page } from "@playwright/test";
import { CommentResponse } from "@types";
import environments from "lib/constants/environments";

export default class BudgetDiscussionDetailsPage {
  // buttons
  readonly shareBtn = this.page.getByTestId("share-button");
  readonly copyLinkBtn = this.page.getByTestId("copy-link");
  readonly commentBtn = this.page.getByTestId("comment-button");
  readonly replyBtn = this.page.getByTestId("reply-button");
  readonly replyCommentBtn = this.page.getByTestId("reply-comment-button");
  readonly pollYesBtn = this.page.getByTestId("poll-yes-button");
  readonly pollNoBtn = this.page.getByTestId("poll-no-button");
  readonly sortCommentsBtn = this.page.getByTestId("sort-comments");
  readonly changeVoteBtn = this.page.getByTestId("change-vote-button");
  readonly changeVoteYesBtn = this.page.getByTestId(
    "change-poll-vote-yes-button"
  );

  // content
  readonly copyLinkText = this.page.getByTestId("copy-link-text");
  readonly pollVoteCard = this.page.getByTestId("poll-vote-card");
  readonly totalComments = this.page.getByTestId("total-comments");

  // Input
  readonly commentInput = this.page.getByTestId("comment-input");
  readonly replyInput = this.page.getByTestId("reply-input");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId: string) {
    await this.page.goto(
      `${environments.frontendUrl}/budget_discussion/${proposalId}`
    );
  }

  async sortAndValidate(
    order: string,
    validationFn: (date1: string, date2: string) => boolean
  ) {
    const responsePromise = this.page.waitForResponse((response) =>
      response.url().includes(`&sort[createdAt]=${order}`)
    );

    await this.sortCommentsBtn.click();
    const response = await responsePromise;

    const comments: CommentResponse[] = (await response.json()).data;

    // API validation
    for (let i = 0; i < comments.length - 1; i++) {
      const isValid = validationFn(
        comments[i].attributes.updatedAt,
        comments[i + 1].attributes.updatedAt
      );
      expect(isValid).toBe(true);
    }
  }

  async addComment(comment: string) {
    await this.commentInput.fill(comment);
    ``;
    await this.commentBtn.click();
  }

  async replyComment(reply: string) {
    await this.page
      .locator('[data-testid^="comment-"][data-testid$="-content-card"]')
      .first()
      .getByTestId("reply-button")
      .click();
    await this.replyInput.fill(reply);
    await this.replyCommentBtn.click();
  }

  async voteOnPoll(vote: string) {
    await this.page.getByTestId(`poll-${vote.toLowerCase()}-button`).click();
  }

  async changePollVote() {
    await this.changeVoteBtn.click();
    await this.changeVoteYesBtn.click();
  }

  async deleteProposal() {
    await this.page.waitForTimeout(2_000);

    await this.page.getByTestId("menu-button").click();
    await this.page.getByTestId("delete-proposal").click();
    await this.page.getByTestId("delete-proposal-yes-button").click();
  }
}
