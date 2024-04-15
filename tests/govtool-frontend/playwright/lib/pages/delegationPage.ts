import environments from "lib/constants/environments";
import { Page } from "@playwright/test";

export default class DelegationPage {
  readonly delegateBtn = this.page.getByTestId("delegate-button");
  readonly otherOptionsBtn = this.page.getByText("Other options");
  readonly nextStepBtn = this.page.getByTestId("next-step-button");
  readonly dRepInput = this.page.getByRole("textbox");

  readonly delegateToDRepCard = this.page.getByTestId("delegate-to-drep-card");
  readonly delegateToMyselfCard = this.page.getByTestId(
    "delegate-to-myself-card",
  );
  readonly signalNoConfidenceCard = this.page.getByTestId(
    "signal-no-confidence-card",
  );
  readonly abstainDelegationCard = this.page.getByTestId("vote-abstain-card");

  readonly delegationErrorModal = this.page.getByTestId(
    "delegation-transaction-error-modal",
  );

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/delegate`);
  }

  async delegateToDrep(dRepId: string) {
    await this.delegateToDRepCard.click();
    await this.nextStepBtn.click();

    await this.dRepInput.fill(dRepId);
    await this.delegateBtn.click();
  }

  async resetDRepForm() {
    if (await this.delegationErrorModal.isVisible()) {
      await this.page.getByTestId("confirm-modal-button").click();
    }
    await this.dRepInput.clear();
  }
}
