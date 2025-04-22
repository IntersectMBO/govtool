import { dRep01AuthFile, dRep02AuthFile } from "@constants/auth";
import { dRep01Wallet, dRep02Wallet } from "@constants/staticWallets";
import { test as setup } from "@fixtures/walletExtension";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { createAuth } from "@helpers/auth";
import { skipIfNotHardFork } from "@helpers/cardano";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
  await skipIfNotHardFork();
});

setup("Create DRep 01 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: dRep01Wallet,
    auth: dRep01AuthFile,
  });
});

setup("Create DRep 02 auth", async ({ page, context }) => {
  await createAuth({
    page,
    context,
    wallet: dRep02Wallet,
    auth: dRep02AuthFile,
  });
});
