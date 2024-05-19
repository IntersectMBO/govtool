import { downloadMetadata } from "@helpers/metadata";
import { Download, Page } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { IDRepInfo } from "@types";
import environments from "lib/constants/environments";
import { withTxConfirmation } from "lib/transaction.decorator";

export default class DRepRegistrationPage {
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly confirmBtn = this.page.getByTestId("confirm-modal-button");
  readonly registrationSuccessModal = this.page.getByTestId(
    "governance-action-submitted-modal"
  );
  readonly continueBtn = this.page.getByTestId("continue-button");
  readonly addLinkBtn = this.page.getByTestId("add-link-button");

  // input fields
  readonly nameInput = this.page.getByTestId("name-input");
  readonly emailInput = this.page.locator('[data-testid="email-input"] input'); // BUG incorrect cannot interact with text input
  readonly bioInput = this.page.getByTestId("bio-input");
  readonly linkInput = this.page.locator('[data-testid="link-input"] input'); // BUG incorrect cannot interact with text input

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/register_drep`);
    await this.continueBtn.click(); // BUG: testId -> continue-register-button
  }

  @withTxConfirmation
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
        if (i > 0) {
          await this.addLinkBtn.click();
        }
        await this.linkInput.nth(i).fill(dRepInfo.extraContentLinks[i]);
      }
    }
    await this.continueBtn.click();
    await this.page.getByRole("checkbox").click();
    await this.continueBtn.click();

    this.page.getByRole("button", { name: `${dRepInfo.name}.jsonld` }).click();
    const dRepMetadata = await this.downloadVoteMetadata();
    const url = await metadataBucketService.uploadMetadata(
      dRepMetadata.name,
      dRepMetadata.data
    );

    await this.page.getByPlaceholder("URL").fill(url);
    await this.page.getByTestId("register-button").click();
  }

  async downloadVoteMetadata() {
    const download: Download = await this.page.waitForEvent("download");
    return downloadMetadata(download);
  }
}
