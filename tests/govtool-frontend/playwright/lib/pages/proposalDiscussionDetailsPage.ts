import environments from "@constants/environments";
import { Page } from "@playwright/test";

export default class ProposalDiscussionDetailsPage {
  private readonly proposalId: number | null;
  // Buttons
  readonly likeBtn = this.page.getByTestId("like-button");
  readonly dislikeBtn = this.page.getByTestId("dislike-button");
  readonly commentBtn = this.page.getByRole("button", {
    name: "Comment",
    exact: true,
  }); // this.page.getByTestId("comment-button");
  readonly addPollBtn = this.page.getByTestId("add-poll");
  readonly SubmitBtn = this.page.getByTestId("submit-button");
  readonly menuBtn = this.page.getByTestId("menu-button");
  readonly editProposalBtn = this.page.getByTestId("edit-proposal");
  readonly deleteProposalBtn = this.page.getByTestId("delete-proposal");
  readonly reviewVersionsBtn = this.page.getByTestId("review-versions");
  readonly closePollBtn = this.page.getByTestId("close-poll");
  readonly sortBtn = this.page
    .locator("div")
    .filter({ hasText: /^Comments$/ })
    .getByRole("button"); // this.page.getByTestId("sort-button");
  readonly proposeGovernanceAction = this.page.getByTestId("propose-GA-button");
  readonly replyBtn = this.page.getByTestId("reply-button");
  readonly pollYesBtn = this.page.getByTestId("poll-yes-button");
  readonly pollNoBtn = this.page.getByTestId("poll-No-button");

  // Inputs
  readonly commentInput = this.page.getByRole("textbox"); //this.page.getByTestId("comment-input");
  readonly replyInput = this.page.getByTestId("reply-input");

  // Indicators
  readonly likesCounts = this.page.getByTestId("likes-count");
  readonly dislikesCounts = this.page.getByTestId("dislikse-count");
  readonly commentsCount = this.page.getByTestId("comments-count");

  readonly pollYesVoteCount = this.page.getByTestId("poll-yes-vote-count");
  readonly pollNoVoteCount = this.page.getByTestId("poll-No-vote-count");

  // Cards
  readonly pollVoteCard = this.page.getByTestId("poll-vote-card");
  readonly pollResultCard = this.page.getByTestId("poll-result-card");
  readonly commentCard =
    this.proposeGovernanceAction.getByTestId("comment-card");

  constructor(
    private readonly page: Page,
    proposalId?: number
  ) {
    this.proposalId = proposalId;
  }

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId?: number) {
    await this.page.goto(
      `${environments.frontendUrl}/proposal_discussion/${proposalId ?? this.proposalId}`
    );
  }
}
