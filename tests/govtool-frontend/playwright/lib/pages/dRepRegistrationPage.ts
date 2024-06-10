import environments from "@constants/environments";
import { Page } from "@playwright/test";
import DRepForm from "../forms/dRepForm";

export default class DRepRegistrationPage extends DRepForm {
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly confirmBtn = this.page.getByTestId("confirm-modal-button");
  readonly registrationSuccessModal = this.page.getByTestId(
    "governance-action-submitted-modal"
  );

  constructor(private readonly page: Page) {
    super(page);
  }

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/register_drep`);
    await this.continueBtn.click(); // BUG: testId -> continue-register-button
  }
}
