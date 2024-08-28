import { downloadMetadata } from "@helpers/metadata";
import { Download, Page, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { IDRepInfo } from "@types";
import { withTxConfirmation } from "lib/transaction.decorator";

const formErrors = {
  dRepName: [
    "max-80-characters-error",
    "this-field-is-required-error",
    "nickname-can-not-contain-whitespaces-error",
  ],
  email: "invalid-email-address-error",
  link: "invalid-url-error",
  paymentAddress: "invalid-payment-address-error",
};

export default class DRepForm {
  readonly continueBtn = this.form.getByTestId("continue-button");
  readonly addLinkBtn = this.form.getByTestId("add-link-button");
  readonly registerBtn = this.form.getByTestId("register-button");
  readonly submitBtn = this.form.getByTestId("submit-button");
  readonly metadataDownloadBtn = this.form.getByTestId(
    "metadata-download-button"
  );

  // input fields
  readonly nameInput = this.form.getByTestId("name-input");
  readonly emailInput = this.form.getByTestId("email-input");
  readonly bioInput = this.form.getByTestId("bio-input");
  readonly linkInput = this.form.getByTestId("link-1-input");
  readonly metadataUrlInput = this.form.getByTestId("metadata-url-input");
  readonly objectivesInput = this.form.getByTestId("objectives-input");
  readonly motivationsInput = this.form.getByTestId("motivations-input");
  readonly qualificationsInput = this.form.getByTestId("qualifications-input");
  readonly paymentAddressInput = this.form.getByTestId("payment-address-input");

  constructor(private readonly form: Page) {}

  @withTxConfirmation
  async register(dRepInfo: IDRepInfo) {
    await this.registerWithoutTxConfirmation(dRepInfo);
  }

  async registerWithoutTxConfirmation(dRepInfo: IDRepInfo) {
    await this.nameInput.fill(dRepInfo.name);

    if (dRepInfo.objectives != null) {
      await this.objectivesInput.fill(dRepInfo.objectives);
    }
    if (dRepInfo.motivations != null) {
      await this.motivationsInput.fill(dRepInfo.motivations);
    }
    if (dRepInfo.qualifications != null) {
      await this.qualificationsInput.fill(dRepInfo.qualifications);
    }
    if (dRepInfo.paymentAddress != null) {
      await this.paymentAddressInput.fill(dRepInfo.paymentAddress);
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
    await this.form.getByRole("checkbox").click();
    await this.registerBtn.click();

    this.metadataDownloadBtn.click();
    const dRepMetadata = await this.downloadVoteMetadata();
    const url = await metadataBucketService.uploadMetadata(
      dRepMetadata.name,
      dRepMetadata.data
    );

    await this.metadataUrlInput.fill(url);
    await this.submitBtn.click();
  }

  async downloadVoteMetadata() {
    const download: Download = await this.form.waitForEvent("download");
    return downloadMetadata(download);
  }

  async validateForm(dRepInfo: IDRepInfo) {
    await this.nameInput.fill(dRepInfo.name);
    await this.objectivesInput.fill(dRepInfo.objectives);
    await this.motivationsInput.fill(dRepInfo.motivations);
    await this.qualificationsInput.fill(dRepInfo.qualifications);
    await this.paymentAddressInput.fill(dRepInfo.paymentAddress);
    await this.linkInput.fill(dRepInfo.extraContentLinks[0]);

    for (const err of formErrors.dRepName) {
      await expect(this.form.getByTestId(err)).toBeHidden();
    }

    expect(await this.objectivesInput.textContent()).toEqual(
      dRepInfo.objectives
    );

    expect(await this.motivationsInput.textContent()).toEqual(
      dRepInfo.motivations
    );
    expect(await this.qualificationsInput.textContent()).toEqual(
      dRepInfo.qualifications
    );

    await expect(this.form.getByTestId(formErrors.link)).toBeHidden();
    await expect(this.form.getByTestId(formErrors.paymentAddress)).toBeHidden();

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

    const nameErrors = await this.form
      .locator('[data-testid$="-error"]')
      .filter({
        hasText: regexPattern,
      })
      .all();

    expect(nameErrors.length).toEqual(1);

    await expect(this.form.getByTestId(formErrors.email)).toBeVisible();

    expect(await this.bioInput.textContent()).not.toEqual(bio);

    await expect(this.form.getByTestId(formErrors.link)).toBeVisible();

    await expect(this.continueBtn).toBeDisabled();
  }
}
