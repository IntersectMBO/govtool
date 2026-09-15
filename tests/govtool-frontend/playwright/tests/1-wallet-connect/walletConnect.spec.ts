import environments from "@constants/environments";
import createWallet from "@fixtures/createWallet";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import convertBufferToHex from "@helpers/convertBufferToHex";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import LoginPage from "@pages/loginPage";
import { expect } from "@playwright/test";

test.beforeEach(async () => {
  await setAllureEpic("1. Wallet connect");
});

test("1A. Should connect wallet and choose stake-key to use", async ({
  page,
}) => {
  const shellyWallet = await ShelleyWallet.generate();
  const extraPubStakeKey = convertBufferToHex(shellyWallet.stakeKey.public);
  const extraRewardAddress = convertBufferToHex(
    shellyWallet.rewardAddressRawBytes(environments.networkId)
  );

  await createWallet(page, {
    extraRegisteredPubStakeKeys: [extraPubStakeKey],
    extraRewardAddresses: [extraRewardAddress],
    networkId: environments.networkId,
  });

  const loginPage = new LoginPage(page);
  await loginPage.login();
});

test("1C. Should disconnect Wallet When connected", async ({ page }) => {
  await createWallet(page, {
    networkId: environments.networkId,
  });

  const loginPage = new LoginPage(page);
  await loginPage.login();

  await loginPage.logout();
});

test("1D. Should reject wallet connection if on different network", async ({
  page,
}) => {
  const wrongNetworkId = environments.networkId == 0 ? 1 : 0;
  await createWallet(page, {
    networkId: wrongNetworkId,
  });

  await page.goto("/");

  await page.getByTestId("connect-wallet-button").click();
  await page.getByTestId("demos-wallet-button").click();

  await expect(page.getByTestId("wallet-connection-error-modal")).toHaveText(
    /You are trying to connect/
  );
});

test("1E. Should hide incompatible wallets when connecting", async ({
  browser,
}) => {
  const wallet = (await ShelleyWallet.generate()).json();
  const newPage = await createNewPageWithWallet(browser, {
    wallet,
    supportedExtensions: [],
  });

  await newPage.goto("/");
  await newPage.getByTestId("connect-wallet-button").click();

  await expect(newPage.getByTestId("demos-wallet-button")).not.toBeVisible();
});
