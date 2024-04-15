import { CIP30Instance, Cip95Instance } from "@mock/cardano-test-wallet/types";
import { Page, expect } from "@playwright/test";

export default class LoginPage {
  readonly connectWalletBtn = this.page.getByTestId("connect-wallet-button");
  readonly demosWalletBtn = this.page.getByTestId("demos-wallet-button").nth(1);
  readonly acceptSanchoNetInfoBtn = this.page
    .getByTestId("confirm-modal-button")
    .nth(0);
  readonly disconnectWalletBtn = this.page.getByTestId("disconnect-button");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto("/");
  }

  async login() {
    await this.goto();

    await this.connectWalletBtn.click();
    await this.demosWalletBtn.click({ force: true });
    await this.acceptSanchoNetInfoBtn.click({ force: true });

    const { stakeKeys, rewardAddresses } = await this.page.evaluate(
      async () => {
        // @ts-ignore
        const wallet: CIP30Instance | Cip95Instance =
          await window.cardano.demos.enable();

        let stakeKeys = [];
        let rewardAddresses = [];
        if ("cip95" in wallet) {
          stakeKeys = await wallet.cip95.getRegisteredPubStakeKeys();
          rewardAddresses = await wallet.getRewardAddresses();
        }

        return { stakeKeys, rewardAddresses };
      },
    );

    // Handle multiple stake keys
    if (stakeKeys.length > 1) {
      await this.page
        .getByTestId(`${rewardAddresses[0]}-radio`)
        .getByText("Voting power:")
        .click({ force: true });
      await this.page.getByTestId("select-button").click();
    }
  }

  async logout() {
    await this.disconnectWalletBtn.click();
  }

  async isLoggedIn() {
    await expect(this.disconnectWalletBtn).toBeVisible();
  }
}
