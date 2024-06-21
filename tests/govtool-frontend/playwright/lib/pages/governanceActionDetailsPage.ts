import environments from "@constants/environments";
import { downloadMetadata } from "@helpers/metadata";
import { Download, Page } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { withTxConfirmation } from "lib/transaction.decorator";

export default class GovernanceActionDetailsPage {
  readonly voteBtn = this.page.getByTestId("vote-button");
  readonly changeVoteBtn = this.page.getByTestId("change-vote");
  readonly yesVoteRadio = this.page.getByTestId("yes-radio");
  readonly noVoteRadio = this.page.getByTestId("no-radio");
  readonly abstainRadio = this.page.getByTestId("abstain-radio");
  readonly governanceActionType = this.page.getByText(
    "Governance Action Type:"
  );
  readonly showVotesBtn = this.page.getByTestId("show-votes-button");
  readonly submittedDate = this.page.getByTestId("submission-date");
  readonly expiryDate = this.page.getByTestId("expiry-date");
  readonly externalModalBtn = this.page.getByTestId("external-modal-button");
  readonly governanceActionId = this.page.getByText("Governance Action ID:");

  readonly contextBtn = this.page.getByTestId("provide-context-button");
  readonly viewOtherDetailsLink = this.page.getByTestId(
    "view-other-details-button"
  );
  readonly continueModalBtn = this.page.getByTestId("continue-modal-button");
  readonly confirmModalBtn = this.page.getByTestId("confirm-modal-button");

  readonly voteSuccessModal = this.page.getByTestId("alert-success");
  readonly externalLinkModal = this.page.getByTestId("external-link-modal");

  readonly contextInput = this.page.getByTestId("provide-context-input");
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

  @withTxConfirmation
  async vote(context?: string) {
    await this.yesVoteRadio.click();

    if (context) {
      await this.contextBtn.click();
      await this.contextInput.fill(context);
      await this.confirmModalBtn.click();
      await this.page.getByRole("checkbox").click();
      await this.confirmModalBtn.click();

      this.page
        .getByRole("button", { name: "download Vote_Context.jsonld" })
        .click(); // BUG missing test id
      const voteMetadata = await this.downloadVoteMetadata();
      const url = await metadataBucketService.uploadMetadata(
        voteMetadata.name,
        voteMetadata.data
      );

      await this.page.getByPlaceholder("URL").fill(url); // Bug showing data-testid="undefinedinput" on url
      await this.confirmModalBtn.click();
      await this.page.getByTestId("go-to-vote-modal-button").click();
    }

    await this.voteBtn.click();
  }

  async downloadVoteMetadata() {
    const download: Download = await this.page.waitForEvent("download");
    return downloadMetadata(download);
  }

  @withTxConfirmation
  async reVote() {
    await this.noVoteRadio.click();
    await this.changeVoteBtn.click();
  }
}
