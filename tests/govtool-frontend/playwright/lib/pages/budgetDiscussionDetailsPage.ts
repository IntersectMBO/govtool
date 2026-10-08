import { Page } from "@playwright/test";
import environments from "lib/constants/environments";

/** One archived 2025 budget proposal: read-only. */
export default class BudgetDiscussionDetailsPage {
  // buttons
  readonly shareBtn = this.page.getByTestId("share-button");
  readonly copyLinkBtn = this.page.getByTestId("copy-link");
  readonly sortCommentsBtn = this.page.getByTestId("sort-comments");
  readonly reviewVersionsBtn = this.page.getByTestId("review-version");
  readonly loadMoreCommentsBtn = this.page.getByTestId("load-more-comments");

  // controls the archive must not show
  readonly commentInput = this.page.getByTestId("comment-input");
  readonly commentBtn = this.page.getByTestId("comment-button");
  readonly replyBtn = this.page.getByTestId("reply-button");
  readonly pollYesBtn = this.page.getByTestId("poll-yes-button");
  readonly pollNoBtn = this.page.getByTestId("poll-no-button");
  readonly menuButton = this.page.getByTestId("menu-button");

  // content
  readonly titleContent = this.page.getByTestId("title-content");
  readonly copyLinkText = this.page.getByTestId("copy-link-text");
  readonly totalComments = this.page.getByTestId("total-comments");
  readonly pollResultCard = this.page.getByTestId("poll-result-card");
  readonly pollTotalVotes = this.page.getByTestId("poll-total-votes");
  readonly pollYesCount = this.page.getByTestId("poll-yes-count");
  readonly pollNoCount = this.page.getByTestId("poll-no-count");
  readonly reviewVersionsDialog = this.page.getByTestId("review-versions");
  readonly commentCards = this.page.locator(
    '[data-testid^="comment-"][data-testid$="-content-card"]'
  );

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(masterId: number | string) {
    await this.page.goto(
      `${environments.frontendUrl}/budget_discussion/${masterId}`
    );
    await this.titleContent.waitFor();
  }
}
