import {
  adaHolder01AuthFile,
  adaHolder02AuthFile,
  adaHolder03AuthFile,
  adaHolder04AuthFile,
  adaHolder05AuthFile,
  adaHolder06AuthFile,
} from "@constants/auth";
import environments from "@constants/environments";
import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  adaHolder05Wallet,
  adaHolder06Wallet,
  dRep01Wallet,
  dRep02Wallet,
} from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import {
  correctDelegatedVoteAdaFormat,
  correctDRepDirectoryFormat,
} from "@helpers/adaFormat";
import { setAllureEpic } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { StaticWallet } from "@types";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfMainnet();
});

test.describe("Delegate to others", () => {
  test.use({
    storageState: adaHolder01AuthFile,
    wallet: adaHolder01Wallet,
  });

  test.describe.configure({ mode: "serial" });

  test("2A. Should show delegated DRep Id (on Dashboard, and DRep Directory) after delegation", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepId = dRep01Wallet.dRepId;

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRepId);

    // Verify dRepId in dRep directory
    await expect(
      page.getByTestId(`${dRepId}-delegate-button`)
    ).not.toBeVisible();

    await expect(page.getByTestId(`${dRepId}-delegated-card`)).toBeVisible();
    await expect(
      page
        .getByTestId(`${dRepId}-delegated-card`)
        .getByTestId(`${dRepId}-copy-id-button`)
    ).toHaveCount(1, {
      timeout: 60_000,
    });

    // Verify dRepId in dashboard
    await page.goto("/dashboard");
    await expect(page.getByText(dRepId)).toBeVisible();
  });

  test("2H. Should prompt to change delegation after delegation", async ({
    page,
  }) => {
    await page.goto("/");

    await expect(
      page.getByTestId("delegate-to-another-drep-button")
    ).toBeVisible();
  });
});

test.describe("Change delegation", () => {
  test.use({
    storageState: adaHolder02AuthFile,
    wallet: adaHolder02Wallet,
  });

  test("2F. Should change delegated DRep", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepIdFirst = dRep01Wallet.dRepId;
    const dRepIdSecond = dRep02Wallet.dRepId;

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();
    await dRepDirectoryPage.delegateToDRep(dRepIdFirst);

    // verify delegation
    await expect(page.getByTestId(`${dRepIdFirst}-delegated-card`)).toBeVisible(
      { timeout: 60_000 }
    );

    await expect(
      page
        .getByTestId(`${dRepIdFirst}-delegated-card`)
        .getByTestId(`${dRepIdFirst}-copy-id-button`)
    ).toHaveText(`(CIP-105) ${dRepIdFirst}`, { timeout: 60_000 });

    // verify delegation
    await dRepDirectoryPage.delegateToDRep(dRepIdSecond);
    await expect(
      page.getByTestId(`${dRepIdSecond}-delegated-card`)
    ).toBeVisible();
    await expect(
      page
        .getByTestId(`${dRepIdSecond}-delegated-card`)
        .getByTestId(`${dRepIdSecond}-copy-id-button`)
    ).toHaveText(`(CIP-105) ${dRepIdSecond}`, { timeout: 60_000 });
  });
});

test.describe("Register DRep state", () => {
  let dRepPage: Page;
  let wallet: StaticWallet;

  test.beforeEach(async ({ page, browser }) => {
    wallet = await walletManager.popWallet("registerDRep");
    await walletManager.removeCopyWallet(wallet, "registerDRepCopy");

    const dRepAuth = await createTempDRepAuth(page, wallet);
    dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableStakeSigning: true,
      enableDRepSigning: true,
    });

    await dRepPage.goto("/");
    await dRepPage.waitForTimeout(2_000); // Waits to ensure the wallet-connection modal not interfere with interactions
  });

  test("2E. Should register as Direct voter", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await dRepPage.getByTestId("register-as-sole-voter-button").click();
    await dRepPage.getByTestId("continue-button").click();
    await expect(
      dRepPage.getByTestId("registration-transaction-submitted-modal")
    ).toBeVisible({ timeout: 60_000 });
    await dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    // Checks in dashboard
    await expect(dRepPage.getByText("You are a Direct Voter")).toBeVisible({
      timeout: 60_000,
    });
    await expect(
      dRepPage.getByTestId("register-as-sole-voter-button")
    ).not.toBeVisible();
    await expect(
      dRepPage.getByTestId("retire-as-sole-voter-button")
    ).toBeVisible();
  });

  test("2S. Should retire as a Direct Voter on delegating to another DRep", async ({}, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    await dRepPage.getByTestId("register-as-sole-voter-button").click();
    await dRepPage.getByTestId("continue-button").click();
    await expect(
      dRepPage.getByTestId("registration-transaction-submitted-modal")
    ).toBeVisible({ timeout: 60_000 });
    await dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);
    await expect(dRepPage.getByText("You are a Direct Voter")).toBeVisible({
      timeout: 60_000,
    });

    const dRepDirectoryPage = new DRepDirectoryPage(dRepPage);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRep01Wallet.dRepId);
    await dRepPage.goto("/dashboard");

    await expect(
      dRepPage.getByText("You Have Retired as a Direct")
    ).toBeVisible({ timeout: 60_000 });
  });
});

test("2G. Should delegate to myself", async ({ page, browser }, testInfo) => {
  test.setTimeout(testInfo.timeout + environments.txTimeOut);

  const wallet = await walletManager.popWallet("registeredDRep");
  const dRepId = wallet.dRepId;

  const dRepAuth = await createTempDRepAuth(page, wallet);
  const dRepPage = await createNewPageWithWallet(browser, {
    storageState: dRepAuth,
    wallet,
    enableStakeSigning: true,
  });

  const dRepDirectoryPage = new DRepDirectoryPage(dRepPage);
  await dRepDirectoryPage.goto();

  await dRepDirectoryPage.delegateToDRep(dRepId);

  await expect(
    dRepDirectoryPage.currentPage.getByTestId(`${dRepId}-delegate-button`)
  ).not.toBeVisible({ timeout: 60_000 });
  await expect(
    dRepDirectoryPage.currentPage.getByTestId(`${dRepId}-copy-id-button`)
  ).toHaveCount(1, {
    timeout: 60_000,
  });
});

test.describe("Multiple delegations", () => {
  test.use({
    storageState: adaHolder05AuthFile,
    wallet: adaHolder05Wallet,
  });

  test("2R. Should display a modal indicating waiting for previous transaction when delegating if the previous transaction is not completed", async ({
    page,
  }) => {
    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.searchInput.fill(dRep01Wallet.dRepId);

    await page.getByTestId(`${dRep01Wallet.dRepId}-delegate-button`).click();
    await expect(page.getByTestId("alert-warning")).toHaveText(/in progress/i, {
      timeout: 60_000,
    });

    await dRepDirectoryPage.searchInput.fill(dRep02Wallet.dRepId);
    await page.getByTestId(`${dRep02Wallet.dRepId}-delegate-button`).click();

    await expect(page.getByTestId("transaction-inprogress-modal")).toBeVisible({
      timeout: 60_000,
    });
  });
});

test.describe("Abstain delegation", () => {
  test.use({
    storageState: adaHolder03AuthFile,
    wallet: adaHolder03Wallet,
  });

  test("2U. Should show delegated voting power to Abstain", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
    await page.getByTestId("abstain-from-every-vote-delegate-button").click();
    await waitForTxConfirmation(page);

    const balance = await kuberService.getBalance(adaHolder03Wallet.address);

    await expect(
      page.getByText(
        `You have delegated ₳${correctDRepDirectoryFormat(balance)}`
      )
    ).toBeVisible({
      timeout: 60_000,
    });
  });
});

test.describe("No confidence delegation", () => {
  test.use({
    storageState: adaHolder04AuthFile,
    wallet: adaHolder04Wallet,
  });

  test("2V. Should show delegated voting power to No confidence", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
    await page
      .getByTestId("signal-no-confidence-on-every-vote-delegate-button")
      .click();
    await waitForTxConfirmation(page);

    const balance = await kuberService.getBalance(adaHolder04Wallet.address);
    await expect(
      page.getByText(
        `You have delegated ₳${correctDRepDirectoryFormat(balance)}`
      )
    ).toBeVisible({
      timeout: 60_000,
    });
  });
});

test.describe("Delegated ADA visibility", () => {
  test.use({
    storageState: adaHolder06AuthFile,
    wallet: adaHolder06Wallet,
  });

  test("2W. Should show my delegated ADA to the DRep", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRep01Wallet.dRepId);

    const adaHolderVotingPower = await kuberService.getBalance(
      adaHolder06Wallet.address
    );
    await expect(
      page.getByText(
        `You have delegated ₳ ${correctDRepDirectoryFormat(adaHolderVotingPower)}`
      )
    ).toBeVisible({ timeout: 60_000 });

    await page.goto("/");
    await expect(
      page.getByText(
        `Your Voting Power of ₳${correctDelegatedVoteAdaFormat(adaHolderVotingPower)} is Delegated to`
      )
    ).toBeVisible({ timeout: 60_000 });
  });
});
