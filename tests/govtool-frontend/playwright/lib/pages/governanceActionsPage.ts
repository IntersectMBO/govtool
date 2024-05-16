import removeAllSpaces from "@helpers/removeAllSpaces";
import { Locator, Page, expect } from "@playwright/test";
import { FilterOption, IProposal } from "@types";
import environments from "lib/constants/environments";
import GovernanceActionDetailsPage from "./governanceActionDetailsPage";

export default class GovernanceActionsPage {
  readonly filterBtn = this.page.getByTestId("filters-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly votedTab = this.page.getByTestId("voted-tab");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/governance_actions`);
  }

  async viewProposal(
    proposal: IProposal
  ): Promise<GovernanceActionDetailsPage> {
    const proposalId = `govaction-${proposal.txHash}#${proposal.index}-view-detail`;
    await this.page.getByTestId(proposalId).click();

    return new GovernanceActionDetailsPage(this.page);
  }

  async viewFirstProposal(): Promise<GovernanceActionDetailsPage> {
    await this.page
      .locator('[data-testid^="govaction-"][data-testid$="-view-detail"]')
      .first()
      .click();
    return new GovernanceActionDetailsPage(this.page);
  }

  async viewFirstVotedProposal(): Promise<GovernanceActionDetailsPage> {
    await this.page
      .locator('[data-testid^="govaction-"][data-testid$="-change-your-vote"]')
      .first()
      .click();
    return new GovernanceActionDetailsPage(this.page);
  }

  async viewVotedProposal(
    proposal: IProposal
  ): Promise<GovernanceActionDetailsPage> {
    const proposalId = `govaction-${proposal.txHash}#${proposal.index}-change-your-vote`;
    await this.page.getByTestId(proposalId).click();

    return new GovernanceActionDetailsPage(this.page);
  }

  async filterProposalByNames(names: string[]) {
    for (const name of names) {
      const sanitizedProposalName = removeAllSpaces(name);
      await this.page.getByTestId(`${sanitizedProposalName}-checkbox`).click();
    }
  }

  async unFilterProposalByNames(names: string[]) {
    for (const name of names) {
      const sanitizedProposalName = removeAllSpaces(name);
      await this.page.getByTestId(`${sanitizedProposalName}-checkbox`).click();
    }
  }

  async getAllProposals() {
    return this.page.locator('[data-test-id$="-card"]').all();
  }

  async validateFilters(filters: string[]) {
    const proposalCards = await this.getAllProposals();

    for (const proposalCard of proposalCards) {
      const hasFilter = await this._validateFiltersInProposalCard(
        proposalCard,
        filters
      );
      expect(
        hasFilter,
        "A proposal card does not contain any of the filters"
      ).toBe(true);
    }
  }

  async sortProposal(option: string) {
    await this.page.getByTestId(`${option}-radio`).check();
  }

  async validateSort(
    sortOption: string,
    validationFn: (p1: IProposal, p2: IProposal) => boolean,
    filterKeys = Object.keys(FilterOption)
  ) {
    const responses = await Promise.all(
      filterKeys.map((filterKey) =>
        this.page.waitForResponse((response) =>
          response
            .url()
            .includes(`&type[]=${FilterOption[filterKey]}&sort=${sortOption}`)
        )
      )
    );
    const proposalData = await Promise.all(
      responses.map(async (response) => {
        return await response.json();
      })
    );
    expect(proposalData.length, "No proposals to sort").toBeGreaterThan(0);

    // API validation
    proposalData.forEach(async (proposal) => {
      if (proposal.elements.length <= 1) return;

      const proposals = proposal.elements as IProposal[];
      for (let i = 0; i <= proposals.length - 2; i++) {
        const isValid = validationFn(proposals[i], proposals[i + 1]);
        expect(isValid, "API Sorting validation failed").toBe(true);
      }
    });

    // Frontend validation
    const proposalCards = await Promise.all(
      filterKeys.map((key) =>
        this.page.getByTestId(`govaction-${key}-card`).allInnerTexts()
      )
    );

    for (let dIdx = 0; dIdx <= proposalData.length - 1; dIdx++) {
      const proposals = proposalData[dIdx].elements as IProposal[];
      for (let i = 0; i <= proposals.length - 1; i++) {
        expect(
          proposalCards[dIdx][i].includes(proposals[i].txHash),
          "Frontend validation failed"
        ).toBe(true);
      }
    }
  }

  async _validateFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    for (const filter of filters) {
      try {
        await expect(proposalCard.getByText(filter)).toBeVisible();
        return true;
      } catch (e) {}
      return false;
    }
  }
}
