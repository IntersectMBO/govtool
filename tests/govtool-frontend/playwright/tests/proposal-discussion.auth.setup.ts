import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { test as setup } from "@fixtures/walletExtension";
import { createAuthWithUserName } from "@helpers/auth";
import {
  proposal01Wallet,
  proposal02Wallet,
  proposal03Wallet,
  proposal04Wallet,
  proposal05Wallet,
  proposal06Wallet,
  proposal07Wallet,
  proposal08Wallet,
  proposal09Wallet,
} from "@constants/staticWallets";
import {
  proposal01AuthFile,
  proposal02AuthFile,
  proposal03AuthFile,
  proposal04AuthFile,
  proposal05AuthFile,
  proposal06AuthFile,
  proposal07AuthFile,
  proposal08AuthFile,
  proposal09AuthFile,
} from "@constants/auth";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
  await skipIfNotHardFork();
});

setup("Create Proposal 01 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal01Wallet,
    auth: proposal01AuthFile,
  });
});

setup("Create Proposal 02 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal02Wallet,
    auth: proposal02AuthFile,
  });
});

setup("Create Proposal 03 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal03Wallet,
    auth: proposal03AuthFile,
  });
});

setup("Create Proposal 04 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal04Wallet,
    auth: proposal04AuthFile,
  });
});

setup("Create Proposal 05 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal05Wallet,
    auth: proposal05AuthFile,
  });
});

setup("Create Proposal 06 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal06Wallet,
    auth: proposal06AuthFile,
  });
});

setup("Create Proposal 07 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal07Wallet,
    auth: proposal07AuthFile,
  });
});

setup("Create Proposal 08 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal08Wallet,
    auth: proposal08AuthFile,
  });
});

setup("Create Proposal 09 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: proposal09Wallet,
    auth: proposal09AuthFile,
  });
});
