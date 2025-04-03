import { Page } from "@playwright/test";
import environments from "lib/constants/environments";

export default class BudgetDiscussionDetailsPage {
  // Buttons
  readonly shareBtn = this.page.getByTestId("share-button");
  readonly copyLinkBtn = this.page.getByTestId("copy-link");

  // content
  readonly copyLinkText = this.page.getByTestId("copy-link-text");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId: string) {
    await this.page.goto(
      `${environments.frontendUrl}/budget_discussion/${proposalId}`
    );
  }
}
