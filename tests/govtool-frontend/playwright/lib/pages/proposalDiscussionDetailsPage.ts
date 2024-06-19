import environments from "@constants/environments";
import { Page, expect } from "@playwright/test";
import { CommentResponse } from "@types";

export default class ProposalDiscussionDetailsPage {
  // Buttons
  readonly likeBtn = this.page.getByRole("button", {
    name: "proposal likes",
  });
  readonly dislikeBtn = this.page.getByRole("button", {
    name: "proposal dislikes",
  });
  readonly commentBtn = this.page.getByRole("button", {
    name: "Comment",
    exact: true,
  }); // this.page.getByTestId("comment-button");
  readonly addPollBtn = this.page.getByRole("button", { name: "Add Poll" }); // BUG missing test id
  readonly SubmitBtn = this.page.getByTestId("submit-button");
  readonly menuBtn = this.page.getByTestId("menu-button");
  readonly editProposalBtn = this.page.getByTestId("edit-proposal");
  readonly deleteProposalBtn = this.page.getByTestId("delete-proposal");
  readonly reviewVersionsBtn = this.page.getByTestId("review-versions");
  readonly closePollBtn = this.page.getByRole("button", { name: "Close Poll" }); // BUG missing test id
  readonly sortBtn = this.page
    .locator("div")
    .filter({ hasText: /^Comments$/ })
    .getByRole("button"); // this.page.getByTestId("sort-button");
  readonly proposeGovernanceAction = this.page.getByTestId("propose-GA-button");
  readonly replyBtn = this.page.getByTestId("reply-button");
  readonly pollYesBtn = this.page.getByRole("button", { name: "Yes" }); //BUG missing test id
  readonly pollNoBtn = this.page.getByRole("button", { name: "No" }); //BUG missing test id
  readonly showReplyBtn = this.page.getByTestId("show-more-reply");
  readonly closePollYesBtn = this.page.getByRole("button", {
    name: "Yes, close Poll",
  }); // BUG missing test id
  readonly changeVoteBtn = this.page.getByRole("button", {
    name: "Change Vote",
  });

  // Indicators
  readonly likesCounts = this.page.getByTestId("likes-count");
  readonly dislikesCounts = this.page.getByTestId("dislikse-count");
  readonly commentsCount = this.page.getByTestId("comments-count");

  // Cards
  readonly pollVoteCard = this.page.getByTestId("poll-vote-card");
  readonly pollResultCard = this.page.getByTestId("poll-result-card");
  readonly commentCard =
    this.proposeGovernanceAction.getByTestId("comment-card");

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
    await this.page.getByRole("textbox").fill(comment);
    await this.page
      .getByRole("button", { name: "Comment", exact: true })
      .click();
  }

  async replyComment(reply: string) {
    await this.page.getByRole("button", { name: "Reply" }).click();
    await this.page.getByPlaceholder("Add comment").fill(reply);
    await this.page.getByRole("button", { name: "Comment" }).nth(2).click();
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
    await this.page.getByRole("button", { name: `${vote}` }).click();
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
