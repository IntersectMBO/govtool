// Saves storage state to a file in the .auth directory

import {
  adaHolder01Wallet,
  dRep01Wallet,
  user01Wallet,
} from "@constants/staticWallets";
import { importWallet } from "@fixtures/importWallet";
import { test as setup } from "@fixtures/walletExtension";
import LoginPage from "@pages/loginPage";

const dRep01AuthFile = ".auth/dRep01.json";
const adaHolder01AuthFile = ".auth/adaHolder01.json";
const user01AuthFile = ".auth/user01.json";

setup("Create DRep 01 auth", async ({ page, context }) => {
  await importWallet(page, dRep01Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: dRep01AuthFile });
});

setup("Create User 01 auth", async ({ page, context }) => {
  await importWallet(page, user01Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: user01AuthFile });
});

setup("Create AdaHolder 01 auth", async ({ page, context }) => {
  await importWallet(page, adaHolder01Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: adaHolder01AuthFile });
});
