import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { expect, Locator, Page } from "@playwright/test";
import { BudgetProposalType, ProposedGovAction } from "@types";
import environments from "lib/constants/environments";

export default class BudgetDiscussionPage {
  // Buttons
  readonly drawerBtn = this.page.getByTestId("open-drawer-button");
  readonly proposalBudgetDiscussionBtn = this.page.getByTestId(
    "propose-a-budget-discussion-button"
  );
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");
  readonly filterBtn = this.page.getByTestId("filter-button");
  readonly sortBtn = this.page.getByTestId("sort-button");

  // input
  readonly searchInput = this.page.getByTestId("search-input");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/budget_discussion`);
    // wait for the proposal cards to load
    await this.page.waitForTimeout(2_000);
  }

  async getAllProposals() {
    const proposalCardSelector =
      '[data-testid^="budget-discussion-"][data-testid$="-card"]';

    await waitedLoop(async () => {
      const count = await this.page.locator(proposalCardSelector).count();
      return count > 0;
    });
    const proposalCards = await this.page.locator(proposalCardSelector).all();

    expect(true, "No budget proposals found.").toBeTruthy();

    return proposalCards;
  }

  async clickRadioButtonsByNames(names: string[]) {
    for (const name of names) {
      const budgetProposalValue = Object.values(BudgetProposalType).includes(
        name as BudgetProposalType
      );
      if (budgetProposalValue) {
        await this.page.getByLabel(name).click();
      }
    }
  }

  async filterProposalByNames(names: string[]) {
    await this.clickRadioButtonsByNames(names);
  }

  async unFilterProposalByNames(names: string[]) {
    await this.clickRadioButtonsByNames(names);
  }

  async applyAndValidateFilters(
    filters: string[],
    validateFunction: (proposalCard: any, filters: string[]) => Promise<boolean>
  ) {
    await this.page.waitForTimeout(4_000); // wait for the proposals to load
    // single filter
    for (const filter of filters) {
      await this.filterProposalByNames([filter]);
      await this.validateFilters([filter], validateFunction);
      await this.unFilterProposalByNames([filter]);
    }

    // multiple filter
    const multipleFilters = [...filters];
    while (multipleFilters.length > 1) {
      await this.filterProposalByNames(multipleFilters);
      await this.validateFilters(multipleFilters, validateFunction);
      await this.unFilterProposalByNames(multipleFilters);
      multipleFilters.pop();
    }
  }

  async validateFilters(
    filters: string[],
    validateFunction: (proposalCard: any, filters: string[]) => Promise<boolean>
  ) {
    await functionWaitedAssert(async () => {
      const proposalCards = await this.getAllProposals();

      for (const proposalCard of proposalCards) {
        if (await proposalCard.isVisible()) {
          const type = await proposalCard
            .getByTestId("budget-proposal-type")
            .textContent();
          const hasFilter = await validateFunction(proposalCard, filters);

          expect(
            hasFilter,
            !hasFilter &&
              `A budget proposal type ${type} does not contain on ${filters}`
          ).toBe(true);
        }
      }
    });
  }

  async _validateTypeFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const govActionType = await proposalCard
      .getByTestId("budget-proposal-type")
      .textContent();

    return filters.includes(govActionType);
  }

  async sortAndValidate(
    option: "asc" | "desc",
    validationFn: (p1: ProposedGovAction, p2: ProposedGovAction) => boolean
  ) {
    const responsePromise = this.page.waitForResponse((response) =>
      response
        .url()
        .includes(`&sort[createdAt]=${option}&populate[0]=bd_costing`)
    );

    await this.sortBtn.click();
    const response = await responsePromise;

    let proposals: ProposedGovAction[] = (await response.json()).data;

    // API validation
    for (let i = 0; i <= proposals.length - 2; i++) {
      console.log(
        proposals[i].attributes.createdAt,
        proposals[i + 1].attributes.createdAt
      );
      const isValid = validationFn(proposals[i], proposals[i + 1]);
      expect(isValid).toBe(true);
    }
  }
}
