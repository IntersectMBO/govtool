import { Page } from "@playwright/test";

export default class DRepDetailsPage {
  readonly copyIdBtn = this.page.getByTestId("copy-drep-id-button");
  readonly delegateBtn = this.page.getByTestId("delegate-button");
  readonly shareBtn = this.page.getByTestId("share-button");

  constructor(private readonly page: Page) {}

  async goto(dRepId: string) {
    await this.page.goto(`/connected/drep_directory/${dRepId}`);
  }

  async shareLink() {
    await this.shareBtn.click();
    await this.page.getByTestId("copy-link-from-share-button").click();
  }
}
