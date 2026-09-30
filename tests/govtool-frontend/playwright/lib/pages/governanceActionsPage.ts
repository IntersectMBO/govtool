import removeAllSpaces from "@helpers/removeAllSpaces";
import { Locator, Page, expect } from "@playwright/test";
import { GovernanceActionType, IProposal } from "@types";
import environments from "lib/constants/environments";
import GovernanceActionDetailsPage from "./governanceActionDetailsPage";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";

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
    // Details live at /governance_actions/<txHash>#<index> (prefixed with
    // /connected when a wallet is connected); wait for it so callers do not
    // assert against the list page while navigation is still in flight.
    await this.page.waitForURL(/\/governance_actions\/(?!category\/)[^/?#]+/);
    // The router updates the URL before it renders the new route, so also
    // wait for the list cards to go.
    await expect(
      this.page.locator('[data-testid^="govaction-"][data-testid$="-view-detail"]')
    ).toHaveCount(0);
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
    filterKeys: string[] = []
  ) {
    const expectedTypes = filterKeys.map(
      (filterKey) => GovernanceActionType[filterKey]
    );

    // The list is fetched as a single paged request carrying every selected
    // type; axios percent-encodes the brackets, so decode before matching.
    const responsePromise = this.page.waitForResponse((response) => {
      if (!response.url().includes("/proposal/list?")) return false;
      const params = new URL(response.url()).searchParams;
      const types = params.getAll("type[]");
      return (
        params.get("page") === "0" &&
        params.get("sort") === sortOption &&
        types.length === expectedTypes.length &&
        expectedTypes.every((type) => types.includes(type))
      );
    });

    await this.sortProposal(sortOption);
    const response = await responsePromise;
    const { elements: proposals }: { elements: IProposal[] } =
      await response.json();

    // API validation
    for (let i = 0; i <= proposals.length - 2; i++) {
      const isValid = validationFn(proposals[i], proposals[i + 1]);
      expect(
        isValid,
        !isValid &&
          `${sortOption} order broken between ${proposals[i].txHash}#${proposals[i].index} and ${proposals[i + 1].txHash}#${proposals[i + 1].index}`
      ).toBe(true);
    }

    if (expectedTypes.length > 0) {
      for (const proposal of proposals) {
        expect(expectedTypes).toContain(proposal.type);
      }
    }

    await expect(this.actionsLoading).toBeHidden({ timeout: 20_000 });

    await functionWaitedAssert(
      async () => {
        // Frontend validation: the grid renders the response in order
        const cards = this.page.locator(
          '[data-testid^="govaction-"][data-testid$="-card"]'
        );
        expect(await cards.count()).toBeGreaterThanOrEqual(proposals.length);

        for (let i = 0; i <= proposals.length - 1; i++) {
          await expect(cards.nth(i)).toContainText(`${proposals[i].txHash}`);
        }
      },
      {
        name: `frontend sort validation of ${sortOption} and filter ${filterKeys}`,
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
