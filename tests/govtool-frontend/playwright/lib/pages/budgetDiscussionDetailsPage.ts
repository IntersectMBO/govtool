import { expect, Page } from "@playwright/test";
import { BudgetProposalProps, CommentResponse } from "@types";
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
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");
  readonly readMoreBtn = this.page.getByTestId("read-more-button");

  // content
  readonly copyLinkText = this.page.getByTestId("copy-link-text");
  readonly pollVoteCard = this.page.getByTestId("poll-vote-card");
  readonly totalComments = this.page.getByTestId("total-comments");
  readonly linkTextContent = this.page.getByTestId("link-0-text-content");
  readonly linkUrlContent = this.page.getByTestId("link-0-url-content");
  readonly budgetDiscussionTypeContent = this.page
    .getByTestId("budget-discussion-type")
    .first();
  readonly publicProposalChampionContent = this.page.getByTestId(
    "public-proposal-champion"
  );
  readonly socialHandlesContent = this.page.getByTestId("social-handles");
  readonly problemStatementContent = this.page.getByTestId("problem-statement");
  readonly proposalBenefitsContent = this.page.getByTestId("problem-benefit");
  readonly productRoadMapContent = this.page.getByTestId("product-roadmap");
  readonly alignProposalComittesContent = this.page.getByTestId(
    "align-proposal-committees"
  );
  readonly evidenceContent = this.page.getByTestId("evidence");
  readonly proposalNameContent = this.page.getByTestId("proposal-name");
  readonly proposalDescriptionContent = this.page.getByTestId(
    "proposal-description"
  );
  readonly proposalKeyDependenciesContent = this.page.getByTestId(
    "proposal-key-dependencies"
  );
  readonly milestonesContent = this.page.getByTestId("proposal-milestone");
  readonly proposalResourcesAndEstimates = this.page.getByTestId(
    "proposal-resources-&-duration-estimates"
  );
  readonly projectExperienceContent =
    this.page.getByTestId("project-experience");
  readonly proposalContractingContent = this.page.getByTestId(
    "proposal-contracting"
  );
  readonly costingAmountContent = this.page.getByTestId("consting-amount"); // BUG typo
  readonly costingConversionRateContent = this.page.getByTestId(
    "costing-conversion-rate"
  );
  readonly constingPreferedCurrencyContent = this.page.getByTestId(
    "costing-preferred-currency"
  );
  readonly costingPreferedCurrencyAmountContent = this.page.getByTestId(
    "costing-prefereed-currency-amount"
  );
  readonly costBreakdownContent = this.page.getByTestId("cost-breakdown");
  readonly includeAsAuditorContent =
    this.page.getByTestId("include-as-auditor");

  // Input
  readonly commentInput = this.page.getByTestId("comment-input");
  readonly replyInput = this.page.getByTestId("reply-input");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId: number) {
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

  async validateProposalDetails(budgetProposal: BudgetProposalProps) {
    await this.readMoreBtn.click();

    // proposal ownership validation
    await expect(this.publicProposalChampionContent).toHaveText(
      budgetProposal.proposalOwnership.publicChampion
    );
    await expect(this.socialHandlesContent).toHaveText(
      budgetProposal.proposalOwnership.contactDetails
    );

    // problem statement and benefits validation
    await expect(this.problemStatementContent).toHaveText(
      budgetProposal.problemStatementAndBenefits.problemStatement
    );
    await expect(this.proposalBenefitsContent).toHaveText(
      budgetProposal.problemStatementAndBenefits.proposalBenefits
    );
    await expect(this.productRoadMapContent).toHaveText(
      budgetProposal.problemStatementAndBenefits.roadmapName
    );
    await expect(this.budgetDiscussionTypeContent).toHaveText(
      budgetProposal.problemStatementAndBenefits.budgetDiscussionType
    );
    await expect(this.alignProposalComittesContent).toHaveText(
      budgetProposal.problemStatementAndBenefits.committeeAlignmentType
    );
    await expect(this.evidenceContent).toHaveText(
      budgetProposal.problemStatementAndBenefits.suplimentaryEndorsement
    );

    // proposal details validation
    await expect(this.proposalNameContent).toHaveText(
      budgetProposal.proposalDetails.proposalName
    );
    await expect(this.proposalDescriptionContent).toHaveText(
      budgetProposal.proposalDetails.proposalDescription
    );
    await expect(this.proposalKeyDependenciesContent).toHaveText(
      budgetProposal.proposalDetails.proposalKeyDependencies
    );
    await expect(this.milestonesContent).toHaveText(
      budgetProposal.proposalDetails.milestones
    );
    await expect(this.proposalResourcesAndEstimates).toHaveText(
      budgetProposal.proposalDetails.teamSizeAndDuration
    );
    await expect(this.projectExperienceContent).toHaveText(
      budgetProposal.proposalDetails.previousExperience
    );
    await expect(this.proposalContractingContent).toHaveText(
      budgetProposal.proposalDetails.contracting
    );

    // costing validation
    await expect(this.costingAmountContent).toHaveText(
      budgetProposal.costing.adaAmount.toString()
    );
    await expect(this.costingConversionRateContent).toHaveText(
      budgetProposal.costing.adaToUsdConversionRate.toString()
    );
    await expect(this.constingPreferedCurrencyContent).toHaveText(
      budgetProposal.costing.preferredCurrency
    );
    await expect(this.costingPreferedCurrencyAmountContent).toHaveText(
      budgetProposal.costing.AmountInPreferredCurrency.toString()
    );
    await expect(this.costBreakdownContent).toHaveText(
      budgetProposal.costing.costBreakdown
    );

    // further information validation
    await expect(this.linkTextContent).toHaveText(
      budgetProposal.furtherInformation[0].prop_link_text
    );

    // administration and auditing validation
    await expect(this.includeAsAuditorContent).toHaveText(
      budgetProposal.administrationAndAuditing.intersectAdministration === true
        ? "Yes"
        : "No"
    );
  }
}
