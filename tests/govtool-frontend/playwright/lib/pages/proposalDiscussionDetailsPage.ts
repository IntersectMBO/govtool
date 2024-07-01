import environments from "@constants/environments";
import { Page, expect } from "@playwright/test";
import { CommentResponse } from "@types";

export default class ProposalDiscussionDetailsPage {
  // Buttons
  readonly likeBtn = this.page.getByTestId("like-button");
  readonly dislikeBtn = this.page.getByTestId("dislike-button");
  readonly commentBtn = this.page.getByTestId("comment-button");
  readonly replyCommentBtn = this.page.getByTestId("reply-comment-button");
  readonly addPollBtn = this.page.getByTestId("add-poll-button");
  readonly SubmitBtn = this.page.getByTestId("submit-button");
  readonly menuBtn = this.page.getByTestId("menu-button");
  readonly editProposalBtn = this.page.getByTestId("edit-proposal");
  readonly deleteProposalBtn = this.page.getByTestId("delete-proposal");
  readonly reviewVersionsBtn = this.page.getByTestId("review-versions");
  readonly closePollBtn = this.page.getByTestId("close-poll-button");
  readonly sortBtn = this.page.getByTestId("sort-comments");
  readonly proposeGovernanceAction = this.page.getByTestId("propose-GA-button");
  readonly replyBtn = this.page.getByTestId("reply-button");
  readonly pollYesBtn = this.page.getByTestId("poll-yes-button");
  readonly pollNoBtn = this.page.getByTestId("poll-no-button");
  readonly showReplyBtn = this.page.getByTestId("show-more-reply");
  readonly closePollYesBtn = this.page.getByRole("button", {
    name: "Yes, close Poll",
  }); // BUG missing test id
  readonly changePollYesBtn = this.page.getByTestId(
    "change-poll-vote-yes-button"
  );
  readonly changeVoteBtn = this.page.getByTestId("change-vote-button");
  readonly shareButton = this.page.locator("#share-button"); // BUG missing test id

  // Indicators
  readonly likesCounts = this.page.getByTestId("like-count");
  readonly dislikesCounts = this.page.getByTestId("dislike-count");
  readonly commentsCount = this.page.getByTestId("comments-count");
  readonly pollYesCount = this.page.getByTestId("poll-yes-count");
  readonly pollNoCount = this.page.getByTestId("poll-no-count");

  // Cards
  readonly pollVoteCard = this.page.getByTestId("poll-vote-card");
  readonly pollResultCard = this.page.getByTestId("poll-result-card");

  //inputs
  readonly commentInput = this.page.getByRole("textbox"); // bug comment input test id must be on textarea instead of div

  constructor(private readonly page: Page) {}

  async goto(proposalId: number) {
    await this.page.goto(
      `${environments.frontendUrl}/connected/proposal_pillar/proposal_discussion/${proposalId}`
    );
  }

  async closeUsernamePrompt() {
    await this.page
      .locator("div")
      .filter({ hasText: /^Hey, setup your username$/ })
      .getByRole("button")
      .click();
  }

  async addComment(comment: string) {
    await this.commentInput.fill(comment);
    await this.commentBtn.click();
  }

  async replyComment(reply: string) {
    await this.replyBtn.click();
    await this.page.getByPlaceholder("Add comment").fill(reply);
    await this.replyCommentBtn.click();
  }

  async sortAndValidate(
    order: string,
    validationFn: (date1: string, date2: string) => boolean
  ) {
    const responsePromise = this.page.waitForResponse((response) =>
      response.url().includes(`&sort[createdAt]=${order}`)
    );

    await this.sortBtn.click();
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

  async voteOnPoll(vote: string) {
    await this.page.getByTestId(`poll-${vote.toLowerCase()}-button`).click();
  }

  async deleteProposal() {
    await this.page.waitForTimeout(2_000);

    await this.page.locator("#menu-button").click();
    await this.page.getByRole("menuitem", { name: "Delete Proposal" }).click();

    // confirm deletion
    await this.page
      .getByRole("button", { name: "Yes, delete my proposal" })
      .click();
  }
}
