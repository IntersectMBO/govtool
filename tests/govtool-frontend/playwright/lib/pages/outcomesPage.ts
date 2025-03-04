import environments from "@constants/environments";
import { outcomeStatusType } from "@constants/index";
import { toCamelCase } from "@helpers/string";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { expect, Locator, Page } from "@playwright/test";
import { outcomeProposal, outcomeType } from "@types";
import OutcomeDetailsPage from "./outcomeDetailsPage";

export default class OutComesPage {
  // Buttons
  readonly filterBtn = this.page.getByTestId("filters-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly showMoreBtn = this.page.getByTestId("show-more-button");

  //inputs
  readonly searchInput = this.page.getByTestId("search-input");

  constructor(private readonly page: Page) {}

  async goto(filter?: string): Promise<void> {
    await this.page.goto(
      filter
        ? `${environments.frontendUrl}/outcomes?sort=newestFirst&type=${filter}`
        : `${environments.frontendUrl}/outcomes?sort=newestFirst`
    );
  }

  async getAllListedCIP105GovernanceIds(): Promise<string[]> {
    const dRepCards = await this.getAllOutcomes();
    const dRepIds = [];

    for (const dRep of dRepCards) {
      const dRepIdTextContent = await dRep
        .locator('[data-testid$="-CIP-105-id"]')
        .textContent();
      dRepIds.push(dRepIdTextContent.replace(/^.*ID/, ""));
    }

    return dRepIds;
  }

  async viewFirstOutcomes(): Promise<OutcomeDetailsPage> {
    await this.page.locator('[data-testid$="-view-details"]').first().click();
    return new OutcomeDetailsPage(this.page);
  }

  async getAllOutcomes(): Promise<Locator[]> {
    await waitedLoop(async () => {
      return (
        (await this.page.locator('[data-testid$="-outcome-card"]').count()) >
          0 ||
        (await this.page.getByText("No governance actions found").isVisible())
      );
    });
    return await this.page.locator('[data-testid$="-outcome-card"]').all();
  }

  async clickCheckboxByNames(names: string[]) {
    const formattedNames = names.map((name) =>
      name === "Info Action" ? "Info" : name
    );
    for (const name of formattedNames) {
      const testId = name.toLowerCase().replace(/ /g, "-");
      await this.page.getByTestId(`${testId}-checkbox`).click();
    }
  }

  async filterProposalByNames(names: string[]) {
    await this.clickCheckboxByNames(names);
  }

  async unFilterProposalByNames(names: string[]) {
    await this.clickCheckboxByNames(names);
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
    await functionWaitedAssert(
      async () => {
        const proposalCards = await this.getAllOutcomes();
        for (const proposalCard of proposalCards) {
          const type = await proposalCard
            .locator('[data-testid$="-type"]')
            .textContent();
          const outcomeType = type.replace(/^.*Type/, "");
          const hasFilter = await validateFunction(proposalCard, filters);
          if (!hasFilter) {
            const errorMessage = `A outcomne type ${outcomeType} does not contain on ${filters}`;
            throw errorMessage;
          }
          expect(hasFilter).toBe(true);
        }
      },
      {
        name: "validateFilters",
      }
    );
  }

  getSortType(sortOption: string) {
    let sortType = sortOption;
    if (sortOption === "Highest amount of yes votes") {
      sortType = "Highest yes votes";
    }
    return toCamelCase(sortType);
  }

  getSortTestId(sortOption: string) {
    const sortType = this.getSortType(sortOption);
    return sortType.toLowerCase().replace(/[\s.]/g, "") + "-radio";
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
            ? `&filters=${filterKey}&sort=${sortType}`
            : `&sort=${sortType}`
        )
    );

    await this.page.getByTestId(this.getSortTestId(sortOption)).click();

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
      expect(isValid).toBe(true);
    }

    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeHidden({ timeout: 20_000 });

    await functionWaitedAssert(
      async () => {
        const outcomeCards = await this.getAllOutcomes();
        for (const [index, outcomeCard] of outcomeCards.entries()) {
          const outcomeProposalFromAPI = outcomeProposalList[index];
          const proposalTypeFromUI = await outcomeCard
            .locator('[data-testid$="-type"]')
            .textContent();
          const proposalTypeFromApi = outcomeType[outcomeProposalFromAPI.type];

          const cip105IdFromUI = await outcomeCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent();
          const cip105IdFromApi = `${outcomeProposalFromAPI.tx_hash}#${outcomeProposalFromAPI.index}`;

          expect(proposalTypeFromUI.replace(/^.*Type/, "")).toContain(
            proposalTypeFromApi
          );

          expect(cip105IdFromUI.replace(/^.*ID/, "")).toContain(
            cip105IdFromApi
          );
        }
      },
      {
        name: `frontend sort validation of ${sortOption} and filter ${filterKey}`,
      }
    );
  }

  async _validateFiltersInOutcomeCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const type = await proposalCard
      .locator('[data-testid$="-type"]')
      .textContent();
    const outcomeType = type.replace(/^.*Type/, "");
    return filters.includes(outcomeType);
  }

  async _validateStatusFiltersInOutcomeCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const status = await proposalCard
      .locator('[data-testid$="-status"]')
      .textContent();
    const outcomeStatus = outcomeStatusType.filter((statusType) => {
      return status.includes(statusType);
    });
    return outcomeStatus.some((status) => filters.includes(status));
  }
}
