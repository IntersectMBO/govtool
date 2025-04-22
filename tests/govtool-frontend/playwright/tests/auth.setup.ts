// Saves storage state to a file in the .auth directory

import {
  adaHolder01AuthFile,
  adaHolder02AuthFile,
  adaHolder03AuthFile,
  adaHolder04AuthFile,
  adaHolder05AuthFile,
  adaHolder06AuthFile,
  user01AuthFile,
} from "@constants/auth";
import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  adaHolder05Wallet,
  adaHolder06Wallet,
  user01Wallet,
} from "@constants/staticWallets";
import { test as setup } from "@fixtures/walletExtension";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { createAuth, createAuthWithMultipleStake } from "@helpers/auth";
import { skipIfNotHardFork } from "@helpers/cardano";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
  await skipIfNotHardFork();
});

setup("Create User 01 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: user01Wallet,
    auth: user01AuthFile,
  });
});

setup("Create AdaHolder 01 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: adaHolder01Wallet,
    auth: adaHolder01AuthFile,
  });
});

setup("Create AdaHolder 02 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: adaHolder02Wallet,
    auth: adaHolder02AuthFile,
  });
});

setup("Create AdaHolder 03 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: adaHolder03Wallet,
    auth: adaHolder03AuthFile,
  });
});

setup("Create AdaHolder 04 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: adaHolder04Wallet,
    auth: adaHolder04AuthFile,
  });
});

setup("Create AdaHolder 05 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: adaHolder05Wallet,
    auth: adaHolder05AuthFile,
  });
});

setup("Create AdaHolder 06 auth", async ({ page, context }) => {
  await createAuthWithMultipleStake({
    page,
    context,
    wallet: adaHolder06Wallet,
    auth: adaHolder06AuthFile,
  });
});
