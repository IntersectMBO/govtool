import { isMobile, openDrawer } from "@helpers/mobile";
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

    await this.connectWalletBtn.click();
    await this.demosWalletBtn.click({ force: true });

    // One stake key: the connection modal opens (dismissed by its
    // confirm-modal-button). Several: the stake key page opens instead.
    await Promise.race([
      this.acceptSanchoNetInfoBtn.waitFor({ state: "visible" }).catch(() => {}),
      this.page.waitForURL(/stake_keys/).catch(() => {}),
    ]);
    if (await this.acceptSanchoNetInfoBtn.isVisible()) {
      await this.acceptSanchoNetInfoBtn.click({ force: true });
    }

    const { stakeKeys, rewardAddresses } = await this.page.evaluate(
      async () => {
        const walletInstance = await (window as any)["cardano"][
          "demos"
        ].enable();

        let stakeKeys: string[] = [];
        let rewardAddresses: string[] = [];
        if ("cip95" in walletInstance) {
          stakeKeys = await walletInstance.cip95.getRegisteredPubStakeKeys();
          rewardAddresses = await walletInstance.getRewardAddresses();
        }

        return { stakeKeys, rewardAddresses };
      }
    );

    // Handle multiple stake keys
    if (stakeKeys.length > 1) {
      await this.page
        .getByTestId(`${rewardAddresses[0]}-radio`)
        .locator("div")
        .first()
        .click({ force: true });
      await this.page.getByTestId("select-button").click();
      // Choosing a key opens the connection modal over the dashboard.
      await this.page.getByTestId("confirm-modal-button").click({ force: true });
    }
  }

  // Disconnect lives in the wallet menu on desktop and in the drawer on
  // phones; below 480px the bar shows no wallet pill at all.
  async openWalletMenu() {
    const walletMenu = this.page.getByTestId("wallet-menu-button");
    if (isMobile(this.page) || !(await walletMenu.isVisible())) {
      await openDrawer(this.page);
      return;
    }
    await walletMenu.click();
  }

  async logout() {
    await this.openWalletMenu();
    await this.disconnectWalletBtn.click();
    // Disconnecting asks for confirmation first.
    await this.page.getByTestId("confirm-disconnect-button").click();
  }

  async isLoggedIn() {
    await this.openWalletMenu();
    await expect(this.disconnectWalletBtn).toBeVisible();
    if (!isMobile(this.page) && (await this.page.getByTestId("wallet-menu-button").isVisible())) {
      await this.page.keyboard.press("Escape");
    }
  }
}
