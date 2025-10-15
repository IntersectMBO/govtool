import removeAllSpaces from "@helpers/removeAllSpaces";
import { Locator, Page, expect } from "@playwright/test";
import { GovernanceActionType, IProposal } from "@types";
import environments from "lib/constants/environments";
import GovernanceActionDetailsPage from "./governanceActionDetailsPage";
import { getEnumKeyByValue } from "@helpers/enum";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";

const MAX_SLIDES_DISPLAY_PER_TYPE = 6;

export default class GovernanceActionsPage {
  readonly filterBtn = this.page.getByTestId("filters-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly votedTab = this.page.getByTestId("voted-tab");
  readonly searchInput = this.page.getByTestId("search-input");

  readonly actionsLoading = this.page.getByRole("progressbar").getByRole("img");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/governance_actions`);
    await this.page.waitForTimeout(2_000); // Waits to ensure the alert-success popup does not interfere
  }

  get currentPage(): Page {
    return this.page;
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

  async viewFirstProposalByGovernanceAction(
    governanceAction: GovernanceActionType
  ): Promise<GovernanceActionDetailsPage> {
     const proposalCard = this.page
          .locator('[data-testid^="govaction-"][data-testid$="-card"]')
          .first();

    const isVisible = await proposalCard.isVisible();

    if (isVisible) {
      await proposalCard
        .locator('[data-testid^="govaction-"][data-testid$="-view-detail"]')
        .first()
        .click();

      return new GovernanceActionDetailsPage(this.page);
    } else {
      console.warn(
        `Governance action details page for "${governanceAction}" was not found.`
      );
      return null;
    }
  }

  async getFirstProposal(
  ) {
    await functionWaitedAssert(
      async () => {
        const proposalCard = this.page
          .locator('[data-testid^="govaction-"][data-testid$="-card"]')
          .first();

        await expect(proposalCard
          .locator('[data-testid^="govaction-"][data-testid$="-view-detail"]')
          .first()).toBeVisible()
      }, { name: "Retrying to get the first proposal" });
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

  async getAllProposals(): Promise<Locator[]> {
    await waitedLoop(async () => {
      return (
        (await this.page.locator('[data-testid$="-card"]').count()) > 0 ||
        (await this.page.getByText("No results for the search.").isVisible())
      );
    });
    return this.page.locator('[data-testid$="-card"]').all();
  }

  async validateFilters(filters: string[]) {
    await functionWaitedAssert(async () => {
      const proposalCards = await this.getAllProposals();

      for (const proposalCard of proposalCards) {
        if (await proposalCard.locator('[data-testid$="-type"]').isVisible()) {
          const hasFilter = await this._validateFiltersInProposalCard(
            proposalCard,
            filters
          );
          expect(
            hasFilter,
            hasFilter == false &&
            `A proposal card does not contain any of the ${filters}`
          ).toBe(true);
        }
      }
    });
  }

  async sortProposal(option: string) {
    await this.page.getByTestId(`${option}-radio`).check();
  }

  async sortAndValidate(
    sortOption: string,
    validationFn: (p1: IProposal, p2: IProposal) => boolean,
    filterKey?: string
  ) {
    const apiUrl = filterKey
      ? `proposal/list?page=0&pageSize=10&type%5B%5D=${GovernanceActionType[filterKey]}&sort=${sortOption}`
      : `proposal/list?page=0&pageSize=10&sort=${sortOption}`;

    const responsesPromise = this.page.waitForResponse((response) =>
      response.url().includes(apiUrl)
    );

    await this.sortProposal(sortOption);
    const response = await responsesPromise;

    let responseJson = await response.json();
    let  proposals: IProposal[] = responseJson.elements;

    // API validation
    for (let i = 0; i <= proposals.length - 2; i++) {
      const isValid = validationFn(proposals[i], proposals[i + 1]);
      expect(isValid).toBe(true);
    }

    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeHidden({ timeout: 20_000 });

    await functionWaitedAssert(
      async () => {
        // Frontend validation
        for (let dIdx = 0; dIdx <= proposals.length - 1; dIdx++) {
          const fullTransactionHash = proposals[dIdx].txHash + "#" + proposals[dIdx].index;
          await expect(this.page.getByTestId(`${fullTransactionHash}-id`)).toBeVisible();
        }
      },
      {
        name: `frontend sort validation of ${sortOption} and filter ${filterKey ? filterKey : "None"}`,
      }
    );
  }

  async _validateFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const govActionTypeTextContent = await proposalCard
      .locator('[data-testid$="-type"]')
      .textContent();
    const govActionType = govActionTypeTextContent.split(":")[1];

    return filters.includes(govActionType);
  }
}
