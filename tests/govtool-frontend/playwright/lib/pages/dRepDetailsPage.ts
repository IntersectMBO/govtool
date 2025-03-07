import { expect, Page } from "@playwright/test";

export default class DRepDetailsPage {
  readonly copyIdBtn = this.page.getByTestId("copy-drep-id-button");
  readonly delegateBtn = this.page.getByTestId("delegate-button");
  readonly shareBtn = this.page.getByTestId("share-button");

  constructor(private readonly page: Page) {}

  async goto(dRepId: string) {
    await this.page.goto(`/connected/drep_directory/${dRepId}`);
  }

  async shareLink() {
    const isShareButtonVisible = await this.page
      .waitForSelector(`[data-testid="share-button"]`, { timeout: 60_000 })
      .then(() => true)
      .catch(() => false);

    if (isShareButtonVisible) {
      await this.shareBtn.click();
    } else {
      expect(false, "Share button not found").toBeTruthy();
    }

    await this.page
      .getByTestId("copy-link-from-share-button")
      .click()
      .catch(() => {
        expect(false, "Copy link button not found").toBeTruthy();
      });
  }
}
