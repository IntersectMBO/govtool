import environments from "@constants/environments";
import { toCamelCase } from "@helpers/string";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { expect, Locator, Page } from "@playwright/test";
import { outcomeProposal } from "@types";

export default class OutComesPage {
  // Buttons
  readonly filterBtn = this.page.getByRole("button", { name: "Filter" }); // BUG missing test id
  readonly sortBtn = this.page.getByRole("button", { name: "Sort:" }); // BUG missing test id
  readonly viewDetailsBtn = this.page.getByRole("button", {
    name: "View Details",
  }); // BUG missing test id
  readonly clearBtn = this.page.getByText("clear"); // BUG missing test id
  //inputs
  readonly searchInput = this.page.getByTestId("search-input");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/outcomes`);
  }

  async getAllOutcomes(): Promise<Locator[]> {
    await waitedLoop(async () => {
      return (
        (await this.page.locator('[data-testid$="-card"]').count()) > 0 ||
        this.page.getByText("No results for the search.")
      );
    });
    return this.page.locator('[data-testid$="-card"]').all(); // BUG missing testid
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

  async filterProposalByNames(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click();
    }
  }

  async unFilterProposalByNames(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click();
    }
  }

  async validateFilters(
    filters: string[],
    validateFunction: (proposalCard: any, filters: string[]) => Promise<boolean>
  ) {
    let errorMessage = "";
    await functionWaitedAssert(
      async () => {
        const proposalCards = await this.getAllOutcomes();

        for (const proposalCard of proposalCards) {
          const type = await proposalCard
            .getByTestId("governance-action-type")
            .textContent();
          const hasFilter = await validateFunction(proposalCard, filters);
          errorMessage = `A governance action type ${type} does not contain on ${filters}`;
          expect(hasFilter).toBe(true);
        }
      },
      { message: errorMessage }
    );
  }

  getSortType(sortOption: string) {
    let sortType = sortOption;
    if (sortOption === "Highest amount of yes votes") {
      sortType = "Highest yes votes";
    }
    return toCamelCase(sortType);
  }

  async sortAndValidate(
    sortOption: string,
    validationFn: (p1: outcomeProposal, p2: outcomeProposal) => boolean,
    filterKey?: string
  ) {
    const sortType = this.getSortType(sortOption);
    const responsePromise = this.page.waitForResponse((response) =>
      response
        .url()
        .includes(
          filterKey
            ? `&filters[]=${filterKey}&sort=${sortType}`
            : `&sort=${sortType}`
        )
    );

    await this.page.getByLabel(sortOption).click();

    const response = await responsePromise;
    const data = await response.json();
    let outcomeProposalList: outcomeProposal[] = data.length != 0 ? data : null;

    // API validation
    if (outcomeProposalList.length <= 1) return;

    for (let i = 0; i <= outcomeProposalList.length - 2; i++) {
      const isValid = validationFn(
        outcomeProposalList[i],
        outcomeProposalList[i + 1]
      );

      console.log(isValid);

      expect(isValid).toBe(true);
    }

    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeHidden({ timeout: 20_000 });

    // TODO Frontend validation
    const outcomeCards = await this.getAllOutcomes();
    for (const outcomeCard of outcomeCards) {
      const proposalTitle = await outcomeCard
        .getByTestId("governance-action-card-title")
        .textContent();
      expect(proposalTitle).toContain(
        outcomeProposalList[outcomeCards.indexOf(outcomeCard)].title
      );
    }
  }

  async _validateFiltersInOutcomeCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const govActionType = await proposalCard
      .getByTestId("governance-action-type")
      .textContent();

    return filters.includes(govActionType);
  }

  async _validateStatusFiltersInOutcomeCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    let govActionType = await proposalCard
      .locator('[data-testid$="-status"]')
      .textContent();
    return filters.includes(govActionType);
  }
}
