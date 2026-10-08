import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { expect, Locator, Page } from "@playwright/test";
import {
  BudgetArchiveList,
  BudgetDiscussionEnum,
  BudgetProposalFilterTypes,
} from "@types";
import environments from "lib/constants/environments";
import BudgetDiscussionDetailsPage from "./budgetDiscussionDetailsPage";

export const BUDGET_ARCHIVE_PATH = "/budget-proposals-2025";

// Requests the archive must never make: the forum's budget discussion API.
export const BUDGET_DISCUSSION_API =
  /\/api\/(bds|bd-|bd\/versions)|\/api\/comments\?.*bd_proposal_id/;

const PROPOSAL_CARD_SELECTOR =
  '[data-testid^="budget-discussion-"][data-testid$="-card"]';

/** The read-only 2025 budget proposals archive list. */
export default class BudgetDiscussionPage {
  readonly filterBtn = this.page.getByTestId("filter-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly searchInput = this.page.getByTestId("search-input");
  readonly archiveBanner = this.page.getByTestId(
    "budget-proposals-archive-banner"
  );
  readonly backToListBtn = this.page.getByTestId(
    "back-to-budget-proposals-button"
  );

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/budget_discussion`);
    await this.page.locator(PROPOSAL_CARD_SELECTOR).first().waitFor();
  }

  /** The archive's list file, as the page reads it. */
  async fetchArchiveList(): Promise<BudgetArchiveList> {
    const response = await this.page.request.get(
      `${environments.frontendUrl}${BUDGET_ARCHIVE_PATH}/list.json`
    );
    expect(response.ok()).toBe(true);
    return response.json();
  }

  async viewFirstProposal(): Promise<BudgetDiscussionDetailsPage> {
    await this.page
      .locator(
        '[data-testid^="budget-discussion-"][data-testid$="-view-details"]'
      )
      .first()
      .click();
    return new BudgetDiscussionDetailsPage(this.page);
  }

  async getAllProposals() {
    await waitedLoop(async () => {
      const count = await this.page.locator(PROPOSAL_CARD_SELECTOR).count();
      return count > 0;
    });
    return this.page.locator(PROPOSAL_CARD_SELECTOR).all();
  }

  async showAll(category: BudgetDiscussionEnum) {
    const slug =
      category === BudgetDiscussionEnum.NoCategory
        ? "no-category"
        : category.toLowerCase().replace(/ /g, "-");
    await this.page.getByTestId(`${slug}-show-all-button`).click();
  }

  async clickCategoryCheckboxes(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click();
    }
  }

  async applyAndValidateFilters(
    filters: string[],
    validateFunction: (
      proposalCard: Locator,
      filters: string[]
    ) => Promise<boolean>
  ) {
    // single filter
    for (const filter of filters) {
      await this.clickCategoryCheckboxes([filter]);
      await this.validateFilters([filter], validateFunction);
      await this.clickCategoryCheckboxes([filter]);
    }

    // multiple filters
    const multipleFilters = [...filters];
    while (multipleFilters.length > 1) {
      await this.clickCategoryCheckboxes(multipleFilters);
      await this.validateFilters(multipleFilters, validateFunction);
      await this.clickCategoryCheckboxes(multipleFilters);
      multipleFilters.pop();
    }
  }

  async validateFilters(
    filters: string[],
    validateFunction: (
      proposalCard: Locator,
      filters: string[]
    ) => Promise<boolean>
  ) {
    await functionWaitedAssert(async () => {
      const proposalCards = await this.getAllProposals();

      for (const proposalCard of proposalCards) {
        const type = await proposalCard
          .getByTestId("budget-discussion-type")
          .textContent();
        const hasFilter = await validateFunction(proposalCard, filters);

        expect(
          hasFilter,
          !hasFilter &&
            `A budget proposal type ${type} does not contain on ${filters}`
        ).toBe(true);
      }
    });
  }

  async _validateTypeFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const type = await proposalCard
      .getByTestId("budget-discussion-type")
      .textContent();

    if (type === "None of these") {
      return filters.includes(BudgetDiscussionEnum.NoCategory);
    }
    return filters.includes(type);
  }

  async sortBy(type: BudgetProposalFilterTypes) {
    await this.sortBtn.click();
    await this.page.getByTestId(`${type}-sort-option`).click();
    await expect(this.sortBtn).toHaveText(`Sort: ${type}`);
  }

  /**
   * Sorts, then checks that every adjacent pair of cards on the page (one
   * category shown in full) is in order by the value `read` takes from a card.
   */
  async sortAndValidate<T>(
    type: BudgetProposalFilterTypes,
    read: (proposalCard: Locator) => Promise<T>,
    inOrder: (a: T, b: T) => boolean
  ) {
    await this.sortBy(type);

    await functionWaitedAssert(async () => {
      const values = await Promise.all(
        (await this.getAllProposals()).map(read)
      );
      for (let i = 0; i < values.length - 1; i++) {
        expect(
          inOrder(values[i], values[i + 1]),
          `Sorting ${type}: ${values[i]} before ${values[i + 1]}`
        ).toBe(true);
      }
    });
  }
}
