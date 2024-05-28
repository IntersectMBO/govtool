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

  async sortAndValidate(
    sortOption: string,
    validationFn: (p1: IProposal, p2: IProposal) => boolean,
    filterKeys = Object.keys(FilterOption)
  ) {
    const responsesPromise = Promise.all(
      filterKeys.map((filterKey) =>
        this.page.waitForResponse((response) =>
          response
            .url()
            .includes(`&type[]=${FilterOption[filterKey]}&sort=${sortOption}`)
        )
      )
    );

    await this.sortProposal(sortOption);
    const responses = await responsesPromise;

    let proposalData: IProposal[][] = await Promise.all(
      responses.map(async (response) => {
        const { elements } = await response.json();
        return elements.length ? elements : null;
      })
    );
    proposalData = proposalData.filter(Boolean);

    // API validation
    proposalData.forEach(async (proposal) => {
      if (proposal.length <= 1) return;

      const proposals = proposal;
      for (let i = 0; i <= proposals.length - 2; i++) {
        const isValid = validationFn(proposals[i], proposals[i + 1]);
        expect(isValid).toBe(true);
      }
    });

    // Frontend validation
    for (let dIdx = 0; dIdx <= proposalData.length - 1; dIdx++) {
      const proposals = proposalData[dIdx] as IProposal[];
      const slides = await this.page
        .locator(`[data-testid="govaction-${proposals[0].type}-card"]`)
        .all();
      for (let i = 0; i <= slides.length - 1; i++) {
        await expect(slides[i]).toContainText(`${proposals[i].txHash}`);
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
