import {
  adaHolder01AuthFile,
  adaHolder02AuthFile,
  adaHolder03AuthFile,
  adaHolder04AuthFile,
  adaHolder05AuthFile,
  adaHolder06AuthFile,
} from "@constants/auth";
import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  adaHolder05Wallet,
  adaHolder06Wallet,
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

const authConfigs = [
  {
    name: "AdaHolder 01",
    wallet: adaHolder01Wallet,
    auth: adaHolder01AuthFile,
  },
  {
    name: "AdaHolder 02",
    wallet: adaHolder02Wallet,
    auth: adaHolder02AuthFile,
  },
  {
    name: "AdaHolder 03",
    wallet: adaHolder03Wallet,
    auth: adaHolder03AuthFile,
  },
  {
    name: "AdaHolder 04",
    wallet: adaHolder04Wallet,
    auth: adaHolder04AuthFile,
  },
  {
    name: "AdaHolder 05",
    wallet: adaHolder05Wallet,
    auth: adaHolder05AuthFile,
  },
];

for (const config of authConfigs) {
  setup(`Create ${config.name} auth`, async ({ page, context }) => {
    await createAuth({
      page,
      context,
      wallet: config.wallet,
      auth: config.auth,
    });
  });
}

setup("Create AdaHolder 06 auth", async ({ page, context }) => {
  await createAuthWithMultipleStake({
    page,
    context,
    wallet: adaHolder06Wallet,
    auth: adaHolder06AuthFile,
  });
});
