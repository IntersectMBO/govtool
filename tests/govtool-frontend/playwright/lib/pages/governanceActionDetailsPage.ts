import environments from "@constants/environments";
import { Page } from "@playwright/test";

export default class GovernanceActionDetailsPage {
  readonly voteBtn = this.page.getByTestId("vote-button");
  readonly changeVoteBtn = this.page.getByTestId("change-vote");
  readonly yesVoteRadio = this.page.getByTestId("yes-radio");
  readonly noVoteRadio = this.page.getByTestId("no-radio");
  readonly abstainRadio = this.page.getByTestId("abstain-radio");
  readonly governanceActionType = this.page.getByText(
    "Governance Action Type:"
  );
  readonly submittedDate = this.page.getByTestId("submission-date");
  readonly expiryDate = this.page.getByTestId("expiry-date");
  readonly externalModalBtn = this.page.getByTestId("external-modal-button");
  readonly governanceActionId = this.page.getByText("Governance Action ID:");

  readonly contextBtn = this.page.getByRole("button", {
    name: "Provide context about your",
  }); // BUG testId
  readonly viewOtherDetailsLink = this.page.getByTestId(
    "view-other-details-button"
  );
  readonly continueModalBtn = this.page.getByTestId("continue-modal-button");

  readonly voteSuccessModal = this.page.getByTestId("alert-success");
  readonly externalLinkModal = this.page.getByTestId("external-link-modal");

  readonly contextInput = this.page.getByPlaceholder("Provide context"); // BUG testId
  readonly cancelModalBtn = this.page.getByTestId("cancel-modal-button");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId: string) {
    await this.page.goto(
      `${environments.frontendUrl}/governance_actions/${proposalId}`
    );
  }

  async vote() {
    await this.yesVoteRadio.click();
    await this.voteBtn.click();
  }

  async reVote() {
    await this.noVoteRadio.click();
    await this.changeVoteBtn.click();
  }
}
