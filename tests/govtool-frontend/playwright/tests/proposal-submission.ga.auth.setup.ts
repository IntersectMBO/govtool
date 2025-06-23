import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { test as setup } from "@fixtures/walletExtension";
import { createAuthWithUserName } from "@helpers/auth";
import walletManager from "lib/walletManager";
import { proposalSubmissionAuthFile } from "@constants/auth";
import { skipIfMainnet } from "@helpers/cardano";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
  await skipIfMainnet();
});

setup(
  `Create auth for proposal submission balance dependent test`,
  async ({ page, context }) => {
    const proposalSubmissionWallet =
      await walletManager.getFirstWalletByPurpose("proposalSubmissionCopy");
    await createAuthWithUserName({
      page,
      context,
      wallet: proposalSubmissionWallet,
      auth: proposalSubmissionAuthFile,
    });
  }
);
