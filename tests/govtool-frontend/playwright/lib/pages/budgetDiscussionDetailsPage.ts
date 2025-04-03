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
}
