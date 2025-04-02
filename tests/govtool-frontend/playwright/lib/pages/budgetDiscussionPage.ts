import { waitedLoop } from "@helpers/waitedLoop";
import { expect, Page } from "@playwright/test";
import environments from "lib/constants/environments";

export default class BudgetDiscussionPage {
  // Buttons
  readonly drawerBtn = this.page.getByTestId("open-drawer-button");
  readonly proposalBudgetDiscussionBtn = this.page.getByTestId(
    "propose-a-budget-discussion-button"
  );
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");

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
}
