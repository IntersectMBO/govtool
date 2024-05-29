// Saves storage state to a file in the .auth directory

import { importWallet } from "@fixtures/importWallet";
import { ShelleyWallet } from "@helpers/crypto";
import LoginPage from "@pages/loginPage";
import { Page } from "@playwright/test";
import { StaticWallet } from "@types";

const tempDRepAuth = ".auth/tempDRepAuth.json";
const tempUserAuth = ".auth/tempUserAuth.json";
const tempAdaHolderAuth = ".auth/tempAdaHolderAuth.json";

export async function createTempDRepAuth(page: Page, wallet: StaticWallet) {
  await importWallet(page, wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await page.context().storageState({ path: tempDRepAuth });
  return tempDRepAuth;
}

export async function createTempAdaHolderAuth(
  page: Page,
  wallet: ShelleyWallet
) {
  await importWallet(page, wallet.json());

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await page.context().storageState({ path: tempAdaHolderAuth });
  return tempAdaHolderAuth;
}

export async function createTempUserAuth(page: Page, wallet: ShelleyWallet) {
  await importWallet(page, wallet.json());

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await page.context().storageState({ path: tempUserAuth });
  return tempUserAuth;
}
