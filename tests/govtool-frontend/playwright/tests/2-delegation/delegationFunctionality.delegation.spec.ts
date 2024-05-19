import environments from "@constants/environments";
import { adaHolder01Wallet } from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import extractDRepFromWallet from "@helpers/shellyWallet";
import {
  registerStakeForWallet,
  transferAdaForWallet,
  waitForTxConfirmation,
} from "@helpers/transaction";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect } from "@playwright/test";

test.describe("Delegate to others", () => {
  test.describe.configure({ mode: "serial" });

  test.use({
    storageState: ".auth/adaHolder01.json",
    wallet: adaHolder01Wallet,
  });

  test("2A. Should show delegated DRep Id (on Dashboard, and DRep Directory) after delegation", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepId = "drep1qzw234c0ly8csamxf8hrhfahvzwpllh2ckuzzvl38d22wwxxquu";

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRepId);

    // Verify dRepId in dRep directory
    await expect(
      page.getByTestId(`${dRepId}-delegate-button')`)
    ).not.toBeVisible();
    await expect(page.getByText(dRepId)).toHaveCount(1);

    // Verify dRepId in dashboard
    await page.goto("/dashboard");
    await expect(page.getByText(dRepId)).toBeVisible();
  });

  test("2F. Should change delegated dRep", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepId = "drep1qzw234c0ly8csamxf8hrhfahvzwpllh2ckuzzvl38d22wwxxquu";

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();
    await dRepDirectoryPage.delegateToDRep(dRepId);
    await expect(page.getByTestId(`${dRepId}-copy-id-button`)).toHaveText(
      dRepId
    ); // verify delegation
  });
});

test.describe("Delegate to myself", () => {
  test("2E. Should register as Sole voter", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await ShelleyWallet.generate();
    const dRepId = extractDRepFromWallet(wallet);

    await transferAdaForWallet(wallet, 600);
    await registerStakeForWallet(wallet);

    const dRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableStakeSigning: true,
    });

    await dRepPage.goto("/");
    await dRepPage.getByTestId("register-as-sole-voter-button").click();
    await dRepPage.getByTestId("continue-button").click();
    await expect(
      dRepPage.getByTestId("registration-transaction-submitted-modal")
    ).toBeVisible();
    await dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    // Checks in dashboard
    await expect(page.getByText(dRepId)).toHaveText(dRepId);

    // Checks in dRep directory
    await expect(dRepPage.getByText("You are a Direct Voter")).toBeVisible();
    await dRepPage.getByTestId("drep-directory-link").click();
    await expect(dRepPage.getByText("Direct Voter")).toBeVisible();
    await expect(dRepPage.getByTestId(`${dRepId}-copy-id-button`)).toHaveText(
      dRepId
    );
  });
});
