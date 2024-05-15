import {
  CIP30Instance,
  Cip95Instance,
} from "@cardanoapi/cardano-test-wallet/types";
import { isMobile, openDrawer, openDrawerLoggedIn } from "@helpers/mobile";
import { Page, expect } from "@playwright/test";

export default class LoginPage {
  readonly connectWalletBtn = this.page.getByTestId("connect-wallet-button");
  readonly demosWalletBtn = this.page.getByTestId("demos-wallet-button");
  readonly acceptSanchoNetInfoBtn = this.page
    .getByTestId("confirm-modal-button")
    .nth(0);
  readonly disconnectWalletBtn = this.page.getByTestId("disconnect-button");
  readonly dRepIdDisplay = this.page.getByTestId("dRep-id-display");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto("/");
  }

  async login() {
    await this.goto();

    if (isMobile(this.page)) {
      await openDrawer(this.page);
      await this.page
        .getByRole("button", { name: "Connect your wallet" }) // BUG testId should be same as connect-wallet-button
        .click();
    } else {
      await this.connectWalletBtn.click();
    }
    await this.demosWalletBtn.click({ force: true });
    await this.acceptSanchoNetInfoBtn.click({ force: true });

    const { stakeKeys, rewardAddresses } = await this.page.evaluate(
      async () => {
        const walletInstance: CIP30Instance | Cip95Instance =
          await window["cardano"]["demos"].enable();

        let stakeKeys = [];
        let rewardAddresses = [];
        if ("cip95" in walletInstance) {
          stakeKeys = await walletInstance.cip95.getRegisteredPubStakeKeys();
          rewardAddresses = await walletInstance.getRewardAddresses();
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
    if (isMobile(this.page)) {
      await openDrawerLoggedIn(this.page);
    }
    await this.disconnectWalletBtn.click();
  }

  async isLoggedIn() {
    if (isMobile(this.page)) {
      await openDrawerLoggedIn(this.page);
    }
    await expect(this.disconnectWalletBtn).toBeVisible();
  }
}
