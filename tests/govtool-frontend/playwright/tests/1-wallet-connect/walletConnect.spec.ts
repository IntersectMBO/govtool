import environments from "@constants/environments";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import LoginPage from "@pages/loginPage";
import { expect } from "@playwright/test";
import { connectTestWallet } from "lib/wallet/pageWallet";
import { randomWallet, testWallet } from "lib/wallet/testWallets";

test.beforeEach(async () => {
  await setAllureEpic("1. Wallet connect");
});

test("1A. Should connect wallet and choose stake-key to use", async ({
  page,
}) => {
  const other = await testWallet("1A:extraStake");

  await connectTestWallet(page, await randomWallet(), {
    autoConnect: false,
    extraRegisteredPubStakeKeys: [other.stake.public],
    extraRewardAddresses: [other.rewardAddress],
  });

  const loginPage = new LoginPage(page);
  await loginPage.login();
});

test("1C. Should disconnect Wallet When connected", async ({ page }) => {
  await connectTestWallet(page, await randomWallet(), { autoConnect: false });

  const loginPage = new LoginPage(page);
  await loginPage.login();

  await loginPage.logout();
});

test("1D. Should reject wallet connection if on different network", async ({
  page,
}) => {
  const wrongNetworkId = environments.networkId == 0 ? 1 : 0;
  await connectTestWallet(page, await randomWallet(wrongNetworkId), {
    autoConnect: false,
  });

  await page.goto("/");

  await page.getByTestId("connect-wallet-button").click();
  await page.getByTestId("demos-wallet-button").click();

  await expect(page.getByTestId("wallet-connection-error-modal")).toHaveText(
    /You are trying to connect/
  );
});

test("1E. Should hide incompatible wallets when connecting", async ({
  page,
}) => {
  await connectTestWallet(page, await randomWallet(), {
    autoConnect: false,
    supportedExtensions: [],
  });

  await page.goto("/");
  await page.getByTestId("connect-wallet-button").click();

  await expect(page.getByTestId("demos-wallet-button")).not.toBeVisible();
});
