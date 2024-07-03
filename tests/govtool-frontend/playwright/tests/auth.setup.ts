// Saves storage state to a file in the .auth directory

import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  adaHolder05Wallet,
  adaHolder06Wallet,
  dRep01Wallet,
  dRep02Wallet,
  proposal01Wallet,
  user01Wallet,
} from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { importWallet } from "@fixtures/importWallet";
import { test as setup } from "@fixtures/walletExtension";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import LoginPage from "@pages/loginPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";

const dRep01AuthFile = ".auth/dRep01.json";
const dRep02AuthFile = ".auth/dRep02.json";

const adaHolder01AuthFile = ".auth/adaHolder01.json";
const adaHolder02AuthFile = ".auth/adaHolder02.json";
const adaHolder03AuthFile = ".auth/adaHolder03.json";
const adaHolder04AuthFile = ".auth/adaHolder04.json";
const adaHolder05AuthFile = ".auth/adaHolder05.json";
const adaHolder06AuthFile = ".auth/adaHolder06.json";

const user01AuthFile = ".auth/user01.json";

const proposal01AuthFile = ".auth/proposal01.json";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
});

setup("Create DRep 01 auth", async ({ page, context }) => {
  await importWallet(page, dRep01Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: dRep01AuthFile });
});

setup("Create DRep 02 auth", async ({ page, context }) => {
  await importWallet(page, dRep02Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: dRep02AuthFile });
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

setup("Create AdaHolder 02 auth", async ({ page, context }) => {
  await importWallet(page, adaHolder02Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: adaHolder02AuthFile });
});

setup("Create AdaHolder 03 auth", async ({ page, context }) => {
  await importWallet(page, adaHolder03Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: adaHolder03AuthFile });
});

setup("Create AdaHolder 04 auth", async ({ page, context }) => {
  await importWallet(page, adaHolder04Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: adaHolder04AuthFile });
});

setup("Create AdaHolder 05 auth", async ({ page, context }) => {
  await importWallet(page, adaHolder05Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: adaHolder05AuthFile });
});

setup("Create AdaHolder 06 auth", async ({ page, context }) => {
  await importWallet(page, adaHolder06Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: adaHolder06AuthFile });
});

setup("Create Proposal 01 auth", async ({ page, context }) => {
  await importWallet(page, proposal01Wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();
  await proposalDiscussionPage.verifyIdentityBtn.click();

  await proposalDiscussionPage.setUsername(
      faker.internet.userName().toLowerCase()
  );

  await context.storageState({ path: proposal01AuthFile });
});
