import environments from "@constants/environments";
import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  dRep01Wallet,
  dRep02Wallet,
} from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import extractDRepFromWallet from "@helpers/shellyWallet";
import {
  registerDRepForWallet,
  registerStakeForWallet,
  transferAdaForWallet,
  waitForTxConfirmation,
} from "@helpers/transaction";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
});

test.describe("Delegate to others", () => {
  test.use({
    storageState: ".auth/adaHolder01.json",
    wallet: adaHolder01Wallet,
  });

  test("2A. Should show delegated DRep Id (on Dashboard, and DRep Directory) after delegation", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepId = dRep01Wallet.dRepId;

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRepId);

    // Verify dRepId in dRep directory
    await expect(
      page.getByTestId(`${dRepId}-delegate-button')`)
    ).not.toBeVisible();
    await expect(page.getByText(dRepId)).toHaveCount(1, { timeout: 10_000 });

    // Verify dRepId in dashboard
    await page.goto("/dashboard");
    await expect(page.getByText(dRepId)).toBeVisible();
  });
});

test.describe("Change delegation", () => {
  test.use({
    storageState: ".auth/adaHolder02.json",
    wallet: adaHolder02Wallet,
  });

  test("2F. Should change delegated dRep", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepIdFirst = dRep01Wallet.dRepId;
    const dRepIdSecond = dRep02Wallet.dRepId;

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();
    await dRepDirectoryPage.delegateToDRep(dRepIdFirst);
    await expect(page.getByTestId(`${dRepIdFirst}-copy-id-button`)).toHaveText(
      dRepIdFirst
    ); // verify delegation

    await dRepDirectoryPage.delegateToDRep(dRepIdSecond);
    await expect(page.getByTestId(`${dRepIdSecond}-copy-id-button`)).toHaveText(
      dRepIdSecond
    ); // verify delegation
  });
});

test.describe("Delegate to myself", () => {
  let dRepPage: Page;
  let wallet: ShelleyWallet;

  test.beforeEach(async ({ page, browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    wallet = await ShelleyWallet.generate();

    await transferAdaForWallet(wallet, 600);
    await registerStakeForWallet(wallet);

    const dRepAuth = await createTempDRepAuth(page, wallet);
    dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableStakeSigning: true,
    });
  });

  test("2E. Should register as Sole voter", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepId = extractDRepFromWallet(wallet);

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

  test("2S. Should retire as a Direct Voter on delegating to another DRep", async () => {
    await dRepPage.goto("/");
    await dRepPage.getByTestId("register-as-sole-voter-button").click();
    await dRepPage.getByTestId("continue-button").click();
    await expect(
      dRepPage.getByTestId("registration-transaction-submitted-modal")
    ).toBeVisible();
    await dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);
    await expect(dRepPage.getByText("You are a Direct Voter")).toBeVisible();

    const dRepDirectoryPage = new DRepDirectoryPage(dRepPage);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRep01Wallet.dRepId);
    await dRepPage.goto("/dashboard");

    await expect(
      dRepPage.getByText("You Have Retired as a Direct")
    ).toBeVisible();
  });
});

test.describe("Multiple delegations", () => {
  test.use({
    storageState: ".auth/adaHolder02.json",
    wallet: adaHolder02Wallet,
  });

  test("2R. Should display a modal indicating waiting for previous transaction when delegating if the previous transaction is not completed", async ({
    page,
  }) => {
    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.searchInput.fill(dRep01Wallet.dRepId);
    const delegateBtn = page.getByTestId(
      `${dRep01Wallet.dRepId}-delegate-button`
    );
    await expect(delegateBtn).toBeVisible();
    await page.getByTestId(`${dRep01Wallet.dRepId}-delegate-button`).click();

    await page.waitForTimeout(2_000);
    await dRepDirectoryPage.searchInput.fill(dRep02Wallet.dRepId);
    await page.getByTestId(`${dRep02Wallet.dRepId}-delegate-button`).click();

    await expect(
      page.getByTestId("transaction-inprogress-modal")
    ).toBeVisible();
  });
});

test.describe("Abstain delegation", () => {
  test.describe.configure({ mode: "serial" });

  let dRepWallet: ShelleyWallet;

  test.beforeAll(async () => {
    dRepWallet = await ShelleyWallet.generate();
  });

  test("2U_1. Register DRep and Delegate", async ({ browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    await registerDRepForWallet(dRepWallet);

    const adaHolderPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/adaHolder03.json",
      wallet: ShelleyWallet.fromJson(adaHolder03Wallet),
      enableStakeSigning: true,
    });
    const dRepDirectoryPage = new DRepDirectoryPage(adaHolderPage);
    await dRepDirectoryPage.goto();

    const dRepId = extractDRepFromWallet(dRepWallet);
    await dRepDirectoryPage.delegateToDRep(dRepId);
    console.debug(`Delegated to ${dRepId}`);
  });

  test("2U_2. Should delegate my own voting power to Abstain as registered DRep with delegated voting power", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    await transferAdaForWallet(dRepWallet);
    await registerStakeForWallet(dRepWallet);

    const dRepId = extractDRepFromWallet(dRepWallet);
    console.debug(`Using ${dRepId}`);
    const tempDRepAuth = await createTempDRepAuth(page, dRepWallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet: dRepWallet,
      enableStakeSigning: true,
    });

    const dRepDirectoryPage = new DRepDirectoryPage(dRepPage);
    await dRepDirectoryPage.goto();
    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
    await dRepPage
      .getByTestId("abstain-from-every-vote-delegate-button")
      .click();
    await waitForTxConfirmation(dRepPage);

    const balance = await kuberService.getBalance(
      dRepWallet.addressBech32(environments.networkId)
    );
    await expect(
      dRepPage.getByText(`You have delegated ₳${balance}`)
    ).toBeVisible();
  });
});

test.describe("No confidence delegation", () => {
  test.describe.configure({ mode: "serial" });

  let dRepWallet: ShelleyWallet;

  test.beforeAll(async () => {
    dRepWallet = await ShelleyWallet.generate();
  });

  test("2V_1. Register DRep and Delegate", async ({ browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    await registerDRepForWallet(dRepWallet);

    const adaHolderPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/adaHolder04.json",
      wallet: ShelleyWallet.fromJson(adaHolder04Wallet),
      enableStakeSigning: true,
    });
    const dRepDirectoryPage = new DRepDirectoryPage(adaHolderPage);
    await dRepDirectoryPage.goto();

    const dRepId = extractDRepFromWallet(dRepWallet);
    await dRepDirectoryPage.delegateToDRep(dRepId);
    console.debug(`Delegated to ${dRepId}`);
  });

  test("2V_2. Should delegate my own voting power to Abstain as registered DRep with delegated voting power", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    await transferAdaForWallet(dRepWallet);
    await registerStakeForWallet(dRepWallet);

    const dRepId = extractDRepFromWallet(dRepWallet);
    console.debug(`Using ${dRepId}`);
    const tempDRepAuth = await createTempDRepAuth(page, dRepWallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: tempDRepAuth,
      wallet: dRepWallet,
      enableStakeSigning: true,
    });

    const dRepDirectoryPage = new DRepDirectoryPage(dRepPage);
    await dRepDirectoryPage.goto();
    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
    await dRepPage
      .getByTestId("signal-no-confidence-on-every-vote-delegate-button")
      .click();
    await waitForTxConfirmation(dRepPage);

    const balance = await kuberService.getBalance(
      dRepWallet.addressBech32(environments.networkId)
    );
    await expect(
      dRepPage.getByText(`You have delegated ₳${balance}`)
    ).toBeVisible();
  });
});
