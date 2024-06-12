import { Page } from "@playwright/test";
import environments from "lib/constants/environments";
import ProposalDiscussionDetailsPage from "./proposalDiscussionDetailsPage";

export default class ProposalDiscussionPage {
  // Buttons
  readonly filterBtn = this.page.locator("#filters-button"); // this.page.getByTestId("filters-button");
  readonly shareBtn = this.page
    .locator(".MuiCardHeader-action > .MuiButtonBase-root")
    .first(); //this.page.getByTestId("share-button");
  readonly sortBtn = this.page.locator("button:nth-child(2)").first(); //this.page.getByTestId("sort-button");
  readonly searchInput = this.page.getByPlaceholder("Search..."); // this.page.getByTestId("search-input");
  readonly showAllBtn = this.page
    .getByRole("button", { name: "Show all" })
    .first(); //this.page.getByTestId("show-all-button");
  readonly showLessBtn = this.page.getByRole("button", { name: "Show less" });
  readonly infoRadio = this.page.getByLabel("Info");
  readonly treasuryRadio = this.page.getByLabel("Treasury");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/proposal_discussion`);
    await this.page.waitForTimeout(2_000);
  }

  async gotoProposal(proposalId: number) {
    await this.page.goto(
      `${environments.frontendUrl}/proposal_discussion/${proposalId}`
    );
    return new ProposalDiscussionDetailsPage(this.page);
  }

  async viewProposal(
    proposalId: string
  ): Promise<ProposalDiscussionDetailsPage> {
    const proposalTestId = `proposal-${proposalId}-view-detail`;
    await this.page.getByTestId(proposalTestId).click();

    return new ProposalDiscussionDetailsPage(this.page);
  }

  async viewFirstProposal(): Promise<ProposalDiscussionDetailsPage> {
    await this.page
      .locator('[data-testid^="govaction-"][data-testid$="-view-detail"]')
      .first()
      .click();
    return new ProposalDiscussionDetailsPage(this.page);
  }

  async getAllProposals() {
    await this.page.waitForTimeout(2_000);
    return this.page.locator('[data-testid$="-card"]').all(); // BUG
  }

  async getFirstProposal() {
    await this.page.waitForTimeout(2_000);
    return this.page.locator('[data-testid$="-card"]').first();
  }
}
