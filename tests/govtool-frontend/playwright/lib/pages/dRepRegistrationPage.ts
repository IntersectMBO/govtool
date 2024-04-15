import environments from "lib/constants/environments";
import { Page } from "@playwright/test";

export default class DRepRegistrationPage {
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly urlInput = this.page.getByTestId("url-input");
  readonly hashInput = this.page.getByTestId("hash-input");
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly confirmBtn = this.page.getByTestId("confirm-button");

  readonly urlInputError = this.page.getByTestId("invalid-url-format-error");
  readonly hashInputError = this.page.getByTestId(
    "hash-must-be-exactly-64-characters-long-error",
  );

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/register`);
  }

  async register(url?: string, hash?: string) {
    if (url != null && hash != null) {
      await this.urlInput.fill(url);
      await this.hashInput.fill(hash);
      await this.confirmBtn.click();
    } else {
      await this.skipBtn.click();
    }

    await this.registerBtn.click();
  }
}
