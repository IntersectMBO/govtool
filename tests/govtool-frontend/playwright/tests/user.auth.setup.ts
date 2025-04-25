import { user01AuthFile } from "@constants/auth";
import { user01Wallet } from "@constants/staticWallets";
import { test as setup } from "@fixtures/walletExtension";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { createAuth } from "@helpers/auth";
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
