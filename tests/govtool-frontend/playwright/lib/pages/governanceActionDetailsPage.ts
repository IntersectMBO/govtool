import { Page } from "@playwright/test";

export default class GovernanceActionDetailsPage {
  readonly voteBtn = this.page.getByTestId("vote-button");

  constructor(private readonly page: Page) {}
}
