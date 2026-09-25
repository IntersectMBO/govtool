import environments from "@constants/environments";
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
import { sharedDRep } from "lib/wallet/sharedDReps";
import {
  adaBalance,
  ensureFunded,
  testWallet,
  TestWallet,
} from "lib/wallet/testWallets";
import { registeredDRepWallet } from "lib/wallet/transactions";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfMainnet();
});

async function dRepIdOf(name: "dRep01" | "dRep02") {
  return (await sharedDRep(name)).wallet.dRepId;
}

test.describe("Delegate to others", () => {
  test.use({ walletName: "adaHolder01" });

  test.describe.configure({ mode: "serial" });

  test("2A. Should show delegated DRep Id (on Dashboard, and DRep Directory) after delegation", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepId = await dRepIdOf("dRep01");

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(dRepId);

    // Verify dRepId in dRep directory
    await expect(page.getByTestId(`${dRepId}-delegate-button`)).not.toBeVisible(
      { timeout: 60_000 }
    );

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
  test.use({ walletName: "adaHolder02" });

  test("2F. Should change delegated DRep", async ({ page }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const dRepIdFirst = await dRepIdOf("dRep01");
    const dRepIdSecond = await dRepIdOf("dRep02");

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
    ).toBeVisible({ timeout: 60_000 });
    await expect(
      page
        .getByTestId(`${dRepIdSecond}-delegated-card`)
        .getByTestId(`${dRepIdSecond}-copy-id-button`)
    ).toHaveText(`(CIP-105) ${dRepIdSecond}`, { timeout: 60_000 });
  });
});

test.describe("Register DRep state", () => {
  let dRepPage: Page;
  let wallet: TestWallet;

  test.beforeEach(async ({ browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    wallet = await testWallet(`${testInfo.title.split(".")[0]}:directVoter`);
    await ensureFunded(wallet, 600);

    dRepPage = await createNewPageWithWallet(browser, { wallet });

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

    await dRepDirectoryPage.delegateToDRep(await dRepIdOf("dRep01"));
    await dRepPage.goto("/dashboard");

    await expect(
      dRepPage.getByText("You Have Retired as a Direct")
    ).toBeVisible({ timeout: 60_000 });
  });
});

test("2G. Should delegate to myself", async ({ browser }, testInfo) => {
  test.setTimeout(testInfo.timeout + 3 * environments.txTimeOut);

  const wallet = await registeredDRepWallet("2G:dRep");
  const dRepId = wallet.dRepId;

  const dRepPage = await createNewPageWithWallet(browser, { wallet });

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
  test.use({ walletName: "adaHolder05" });

  test("2R. Should display a modal indicating waiting for previous transaction when delegating if the previous transaction is not completed", async ({
    page,
  }) => {
    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    const dRepIdFirst = await dRepIdOf("dRep01");
    const dRepIdSecond = await dRepIdOf("dRep02");

    await dRepDirectoryPage.searchInput.fill(dRepIdFirst);

    await page.getByTestId(`${dRepIdFirst}-delegate-button`).click();
    await expect(page.getByTestId("alert-warning")).toHaveText(/in progress/i, {
      timeout: 60_000,
    });

    await dRepDirectoryPage.searchInput.fill(dRepIdSecond);
    await page.getByTestId(`${dRepIdSecond}-delegate-button`).click();

    await expect(page.getByTestId("transaction-inprogress-modal")).toBeVisible({
      timeout: 60_000,
    });
  });
});

test.describe("No confidence delegation", () => {
  test.use({ walletName: "adaHolder04" });

  test("2V. Should show delegated voting power to No confidence", async ({
    page,
    wallet,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
    await page
      .getByTestId("signal-no-confidence-on-every-vote-delegate-button")
      .click();
    await waitForTxConfirmation(page);

    const balance = await adaBalance(wallet!);
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
    walletName: "adaHolder06",
    // A second registered stake key, so the app asks which one to use.
    pageWallet: async ({}, use) => {
      const other = await testWallet("adaHolder05");
      await use({
        extraRegisteredPubStakeKeys: [other.stake.public],
        extraRewardAddresses: [other.rewardAddress],
      });
    },
  });

  test("2W. Should show my delegated ADA to the DRep", async ({
    page,
    wallet,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.delegateToDRep(await dRepIdOf("dRep01"));

    const adaHolderVotingPower = await adaBalance(wallet!);
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
