import environments from "lib/constants/environments";
import { Page, expect } from "@playwright/test";
import { IProposal } from "@types";
import GovernanceActionDetailsPage from "./governanceActionDetailsPage";
import removeAllSpaces from "@helpers/removeAllSpaces";

export default class GovernanceActionsPage {
  readonly filterBtn = this.page.getByTestId("filters-button");
  readonly sortBtn = this.page.getByTestId("sort-button");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/governance_actions`);
  }

  async viewProposal(
    proposal: IProposal,
  ): Promise<GovernanceActionDetailsPage> {
    const proposalId = `govaction-${proposal.txHash}#${proposal.index}-view-detail`;
    await this.page.getByTestId(proposalId).click();

    return new GovernanceActionDetailsPage(this.page);
  }

  async viewVotedProposal(
    proposal: IProposal,
  ): Promise<GovernanceActionDetailsPage> {
    const proposalId = `govaction-${proposal.txHash}#${proposal.index}-change-your-vote`;
    await this.page.getByTestId(proposalId).click();

    return new GovernanceActionDetailsPage(this.page);
  }

  async filterProposalByName(name: string) {
    const sanitizedProposalName = removeAllSpaces(name);

    await this.page.getByTestId(`${sanitizedProposalName}-checkbox`).check();
  }

  async unFilterProposalByName(name: string) {
    const sanitizedProposalName = removeAllSpaces(name);

    await this.page.getByTestId(`${sanitizedProposalName}-checkbox`).uncheck();
  }

  async sortProposalByName(name: string) {
    const sanitizedProposalName = removeAllSpaces(name);

    await this.page.getByTestId(`${sanitizedProposalName}-checkbox`).check();
  }

  async unSortProposalByName(name: string) {
    const sanitizedProposalName = removeAllSpaces(name);

    await this.page.getByTestId(`${sanitizedProposalName}-checkbox`).check();
  }
}
