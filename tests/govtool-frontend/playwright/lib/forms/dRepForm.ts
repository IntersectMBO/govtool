import { downloadMetadata } from "@helpers/metadata";
import { Download, Page, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { IDRepInfo } from "@types";
import { withTxConfirmation } from "lib/transaction.decorator";

const formErrors = {
  dRepName: [
    "max-80-characters-error",
    "this-field-is-required-error",
    "no-spaces-allowed-error",
  ],
  linkDescription: "max-80-characters-error",
  email: "invalid-email-address-error",
  links: {
    url: "link-reference-description-1-error",
    description: "link-reference-description-1-error",
  },
  identity: {
    url: "identity-reference-url-1-error",
    description: "identity-reference-description-1-error",
  },
  paymentAddress: "invalid-payment-address-error",
};

export default class DRepForm {
  readonly continueBtn = this.form.getByTestId("continue-button");
  readonly addIdentityReferenceBtn = this.form.getByTestId(
    "add-identity-reference-button"
  );
  readonly addLinkReferenceBtn = this.form.getByTestId(
    "add-link-reference-button"
  );
  readonly registerBtn = this.form.getByTestId("register-button");
  readonly submitBtn = this.form.getByTestId("submit-button");
  readonly metadataDownloadBtn = this.form.getByTestId(
    "metadata-download-button"
  );

  // input fields
  readonly nameInput = this.form.getByTestId("name-input");
  readonly emailInput = this.form.getByTestId("email-input");
  readonly bioInput = this.form.getByTestId("bio-input");
  readonly linkRefrenceFirstUrlInput = this.form.getByTestId(
    "link-reference-url-1-input"
  );
  readonly linkRefrenceFirstDescriptionInput = this.form.getByTestId(
    "link-reference-description-1-input"
  );
  readonly identityReferenceFirstDescriptionInput = this.form.getByTestId(
    "identity-reference-description-1-input"
  );
  readonly identityReferenceFirstUrlInput = this.form.getByTestId(
    "identity-reference-url-1-input"
  );
  readonly metadataUrlInput = this.form.getByTestId("metadata-url-input");
  readonly objectivesInput = this.form.getByTestId("objectives-input");
  readonly motivationsInput = this.form.getByTestId("motivations-input");
  readonly qualificationsInput = this.form.getByTestId("qualifications-input");
  readonly paymentAddressInput = this.form.getByTestId("payment-address-input");
  readonly doNotListCheckBox = this.form.getByRole("checkbox");

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

    if (dRepInfo.linksReferenceLinks != null) {
      for (let i = 0; i < dRepInfo.linksReferenceLinks.length; i++) {
        if (i > 0) {
          await this.addLinkReferenceBtn.click();
        }
        await this.form
          .getByTestId(`link-reference-url-${i + 1}-input`)
          .fill(dRepInfo.linksReferenceLinks[i].url);
        await this.form
          .getByTestId(`link-reference-description-${i + 1}-input`)
          .fill(dRepInfo.linksReferenceLinks[i].description);
      }
    }

    if (dRepInfo.identityReferenceLinks != null) {
      for (let i = 0; i < dRepInfo.identityReferenceLinks.length; i++) {
        if (i > 0) {
          await this.addIdentityReferenceBtn.click();
        }
        await this.form
          .getByTestId(`identity-reference-url-${i + 1}-input`)
          .fill(dRepInfo.identityReferenceLinks[i].url);
        await this.form
          .getByTestId(`identity-reference-description-${i + 1}-input`)
          .fill(dRepInfo.identityReferenceLinks[i].description);
      }
    }
    if (dRepInfo.donNotList) {
      await this.doNotListCheckBox.click();
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

  async fillupForm(dRepInfo: IDRepInfo) {
    await this.nameInput.fill(dRepInfo.name);
    await this.objectivesInput.fill(dRepInfo.objectives);
    await this.motivationsInput.fill(dRepInfo.motivations);
    await this.qualificationsInput.fill(dRepInfo.qualifications);
    await this.paymentAddressInput.fill(dRepInfo.paymentAddress);
    await this.linkRefrenceFirstUrlInput.fill(
      dRepInfo.linksReferenceLinks[0].url
    );
    await this.linkRefrenceFirstDescriptionInput.fill(
      dRepInfo.linksReferenceLinks[0].description
    );
    await this.identityReferenceFirstUrlInput.fill(
      dRepInfo.identityReferenceLinks[0].url
    );
    await this.identityReferenceFirstDescriptionInput.fill(
      dRepInfo.identityReferenceLinks[0].description
    );
  }

  async validateForm(dRepInfo: IDRepInfo) {
    await this.fillupForm(dRepInfo);

    for (const err of formErrors.dRepName) {
      await expect(this.form.getByTestId(err)).toBeHidden();
    }
    const objectivesInputText = await this.objectivesInput.textContent();
    const motivationsInputText = await this.motivationsInput.textContent();
    const qualificationsInputText =
      await this.qualificationsInput.textContent();
    const isReferenceLinkErrorVisible = await this.form
      .getByTestId(formErrors.links.url)
      .isVisible();
    const isIdentityLinkErrorVisible = await this.form
      .getByTestId(formErrors.identity.url)
      .isVisible();
    const isPaymentAddressErrorVisible = await this.form
      .getByTestId(formErrors.paymentAddress)
      .isVisible();

    expect(await this.objectivesInput.textContent({ timeout: 60_000 }), {
      message:
        objectivesInputText !== dRepInfo.objectives &&
        `${dRepInfo.objectives} is not equal to ${await this.objectivesInput.textContent()}`,
    }).toEqual(dRepInfo.objectives);

    expect(await this.motivationsInput.textContent(), {
      message:
        motivationsInputText !== dRepInfo.motivations &&
        `${dRepInfo.motivations} is not equal to ${await this.motivationsInput.textContent()}`,
    }).toEqual(dRepInfo.motivations);
    expect(await this.qualificationsInput.textContent(), {
      message:
        qualificationsInputText !== dRepInfo.qualifications &&
        `${dRepInfo.qualifications} is not equal to ${await this.qualificationsInput.textContent()}`,
    }).toEqual(dRepInfo.qualifications);

    await expect(this.form.getByTestId(formErrors.links.url), {
      message:
        isReferenceLinkErrorVisible &&
        `${dRepInfo.linksReferenceLinks[0].url} is an invalid url`,
    }).toBeHidden();
    await expect(this.form.getByTestId(formErrors.identity.url), {
      message:
        isIdentityLinkErrorVisible &&
        `${dRepInfo.identityReferenceLinks[0].url} is an invalid url`,
    }).toBeHidden();
    await expect(this.form.getByTestId(formErrors.paymentAddress), {
      message:
        isPaymentAddressErrorVisible &&
        `${dRepInfo.paymentAddress} is an invalid paymentAddress`,
    }).toBeHidden({ timeout: 60_000 });
    await expect(this.continueBtn).toBeEnabled();
  }

  async inValidateForm(dRepInfo: IDRepInfo) {
    await this.fillupForm(dRepInfo);

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

    expect(nameErrors.length).toBeGreaterThanOrEqual(1);

    const objectivesInputText = await this.objectivesInput.textContent();
    const motivationsInputText = await this.motivationsInput.textContent();
    const qualificationsInputText =
      await this.qualificationsInput.textContent();
    const isReferenceLinkErrorVisible = await this.form
      .getByTestId(formErrors.links.url)
      .isVisible();
    const isReferenceLinkDescriptionErrorVisible = await this.form
      .getByTestId(formErrors.links.description)
      .isVisible();
    const isIdentityLinkErrorVisible = await this.form
      .getByTestId(formErrors.identity.url)
      .isVisible();
    const isIdentityLinkDescriptionErrorVisible = await this.form
      .getByTestId(formErrors.identity.description)
      .isVisible();
    const isPaymentAddressErrorVisible = await this.form
      .getByTestId(formErrors.paymentAddress)
      .isVisible();

    await expect(this.form.getByTestId(formErrors.paymentAddress), {
      message:
        !isPaymentAddressErrorVisible &&
        `${dRepInfo.paymentAddress} is a valid paymentAddress`,
    }).toBeVisible({ timeout: 60_000 });

    expect(await this.objectivesInput.textContent(), {
      message:
        objectivesInputText === dRepInfo.objectives &&
        `${dRepInfo.objectives} is equal to ${await this.objectivesInput.textContent()}`,
    }).not.toEqual(dRepInfo.objectives);
    expect(await this.motivationsInput.textContent(), {
      message:
        motivationsInputText === dRepInfo.motivations &&
        `${dRepInfo.motivations} is equal to ${await this.motivationsInput.textContent()}`,
    }).not.toEqual(dRepInfo.qualifications);
    expect(await this.qualificationsInput.textContent(), {
      message:
        qualificationsInputText === dRepInfo.qualifications &&
        `${dRepInfo.qualifications} is equal to ${await this.qualificationsInput.textContent()}`,
    }).not.toEqual(dRepInfo.qualifications);

    await expect(this.form.getByTestId(formErrors.links.url), {
      message:
        !isReferenceLinkErrorVisible &&
        `${dRepInfo.linksReferenceLinks[0].url} is a valid url`,
    }).toBeVisible();
    await expect(this.form.getByTestId(formErrors.links.description), {
      message:
        !isReferenceLinkDescriptionErrorVisible &&
        `${dRepInfo.linksReferenceLinks[0].description} is a valid description`,
    }).toBeVisible();
    await expect(this.form.getByTestId(formErrors.identity.url), {
      message:
        !isIdentityLinkErrorVisible &&
        `${dRepInfo.identityReferenceLinks[0].url} is a valid url`,
    }).toBeVisible();
    await expect(this.form.getByTestId(formErrors.identity.description), {
      message:
        !isIdentityLinkDescriptionErrorVisible &&
        `${dRepInfo.identityReferenceLinks[0].description} is a valid description`,
    }).toBeVisible();

    await expect(this.continueBtn).toBeDisabled();
  }
}
