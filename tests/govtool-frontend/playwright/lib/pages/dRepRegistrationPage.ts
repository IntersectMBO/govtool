import { Page } from "@playwright/test";
import { IDRepInfo } from "@types";
import environments from "lib/constants/environments";

export default class DRepRegistrationPage {
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly confirmBtn = this.page.getByTestId("confirm-modal-button");
  readonly registrationSuccessModal = this.page.getByTestId(
    "governance-action-submitted-modal"
  );
  readonly continueBtn = this.page.getByTestId("retire-button"); // BUG testId -> continue-button
  readonly addLinkBtn = this.page.getByRole("button", { name: "+ Add link" }); // BUG: testId -> add-link-button

  // input fields
  readonly nameInput = this.page.getByPlaceholder("ex. JohnDRep"); // BUG testId
  readonly emailInput = this.page.getByPlaceholder("john.smith@email.com"); // BUG testId
  readonly bioInput = this.page.getByPlaceholder("Enter your Bio"); // BUG testId
  readonly linkInput = this.page.getByPlaceholder("https://website.com/"); // BUG: testId

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/register_drep`);
    await this.continueBtn.click(); // BUG: testId -> continue-register-button
  }

  async register(dRepInfo: IDRepInfo) {
    await this.nameInput.fill(dRepInfo.name);

    if (dRepInfo.email != null) {
      await this.emailInput.fill(dRepInfo.email);
    }
    if (dRepInfo.bio != null) {
      await this.bioInput.fill(dRepInfo.bio);
    }
    if (dRepInfo.extraContentLinks != null) {
      for (let i = 0; i < dRepInfo.extraContentLinks.length; i++) {
        await this.linkInput.nth(i).fill(dRepInfo.extraContentLinks[i]);
      }
    }

    await this.continueBtn.click(); // BUG: testId -> submit-button
    await this.page.getByRole("checkbox").click();
    await this.continueBtn.click(); // BUG: testId -> submit-button

    await this.page
      .getByPlaceholder("URL")
      .fill(`${environments.metadataBucketUrl}/Test_dRep`);
    await this.continueBtn.click();
  }
}
