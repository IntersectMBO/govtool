import { downloadMetadata } from "@helpers/metadata";
import { Download, Page, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { IDRepInfo } from "@types";
import environments from "lib/constants/environments";
import { withTxConfirmation } from "lib/transaction.decorator";

const formErrors = {
  dRepName: [
    "max-80-characters-error",
    "this-field-is-required-error",
    "nickname-can-not-contain-whitespaces-error",
  ],
  email: "invalid-email-address-error",
  link: "invalid-url-error",
};

export default class DRepRegistrationPage {
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly confirmBtn = this.page.getByTestId("confirm-modal-button");
  readonly registrationSuccessModal = this.page.getByTestId(
    "governance-action-submitted-modal"
  );
  readonly continueBtn = this.page.getByTestId("continue-button");
  readonly addLinkBtn = this.page.getByTestId("add-link-button");
  readonly metadataDownloadBtn = this.page.getByTestId(
    "metadata-download-button"
  );

  // input fields
  readonly nameInput = this.page.getByTestId("name-input");
  readonly emailInput = this.page.getByTestId("email-input");
  readonly bioInput = this.page.getByTestId("bio-input");
  readonly linkInput = this.page.getByTestId("link-1-input");
  readonly metadataUrlInput = this.page.getByTestId("metadata-url-input");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/register_drep`);
    await this.continueBtn.click(); // BUG: testId -> continue-register-button
  }

  @withTxConfirmation
  async register(dRepInfo: IDRepInfo) {
    await this.registerWithoutTxConfirmation(dRepInfo);
  }

  async registerWithoutTxConfirmation(dRepInfo: IDRepInfo) {
    await this.nameInput.fill(dRepInfo.name);

    if (dRepInfo.email != null) {
      await this.emailInput.fill(dRepInfo.email);
    }
    if (dRepInfo.bio != null) {
      await this.bioInput.fill(dRepInfo.bio);
    }
    if (dRepInfo.extraContentLinks != null) {
      for (let i = 0; i < dRepInfo.extraContentLinks.length; i++) {
        if (i > 0) {
          await this.addLinkBtn.click();
        }
        await this.linkInput.nth(i).fill(dRepInfo.extraContentLinks[i]);
      }
    }
    await this.continueBtn.click();
    await this.page.getByRole("checkbox").click();
    await this.continueBtn.click();

    this.metadataDownloadBtn.click();
    const dRepMetadata = await this.downloadVoteMetadata();
    const url = await metadataBucketService.uploadMetadata(
      dRepMetadata.name,
      dRepMetadata.data
    );

    await this.metadataUrlInput.fill(url);
    await this.page.getByTestId("register-button").click();
  }

  async downloadVoteMetadata() {
    const download: Download = await this.page.waitForEvent("download");
    return downloadMetadata(download);
  }

  async validateForm(name: string, email: string, bio: string, link: string) {
    await this.nameInput.fill(name);
    await this.emailInput.fill(email);
    await this.bioInput.fill(bio);
    await this.linkInput.fill(link);

    for (const err of formErrors.dRepName) {
      await expect(this.page.getByTestId(err)).toBeHidden();
    }

    await expect(this.page.getByTestId(formErrors.email)).toBeHidden();

    expect(await this.bioInput.textContent()).toEqual(bio);

    await expect(this.page.getByTestId(formErrors.link)).toBeHidden();

    await expect(this.continueBtn).toBeEnabled();
  }

  async inValidateForm(name: string, email: string, bio: string, link: string) {
    await this.nameInput.fill(name);
    await this.emailInput.fill(email);
    await this.bioInput.fill(bio);
    await this.linkInput.fill(link);

    function convertTestIdToText(testId: string) {
      let text = testId.replace("-error", "");
      text = text.replace(/-/g, " ");
      return text[0].toUpperCase() + text.substring(1);
    }

    const regexPattern = new RegExp(
      formErrors.dRepName.map(convertTestIdToText).join("|")
    );

    const nameErrors = await this.page
      .locator('[data-testid$="-error"]')
      .filter({
        hasText: regexPattern,
      })
      .all();

    expect(nameErrors.length).toEqual(1);

    await expect(this.page.getByTestId(formErrors.email)).toBeVisible();

    expect(await this.bioInput.textContent()).not.toEqual(bio);

    await expect(this.page.getByTestId(formErrors.link)).toBeVisible();

    await expect(this.continueBtn).toBeDisabled();
  }
}
