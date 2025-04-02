import { Page } from "@playwright/test";
import environments from "lib/constants/environments";

export default class BudgetDiscussionPage {
  // Buttons
  readonly drawerBtn = this.page.getByTestId("open-drawer-button");
  readonly proposalBudgetDiscussionBtn = this.page.getByTestId(
    "propose-a-budget-discussion-button"
  );
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/budget_discussion`);
    // wait for the proposal cards to load
    await this.page.waitForTimeout(2_000);
  }
}
