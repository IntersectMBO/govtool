import { setAllureEpic, setAllureStory } from "@helpers/allure";
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
});

const proposalSetups = [
  { name: "Proposal 01", wallet: proposal01Wallet, auth: proposal01AuthFile },
  { name: "Proposal 02", wallet: proposal02Wallet, auth: proposal02AuthFile },
  { name: "Proposal 03", wallet: proposal03Wallet, auth: proposal03AuthFile },
  { name: "Proposal 04", wallet: proposal04Wallet, auth: proposal04AuthFile },
  { name: "Proposal 05", wallet: proposal05Wallet, auth: proposal05AuthFile },
  { name: "Proposal 06", wallet: proposal06Wallet, auth: proposal06AuthFile },
  { name: "Proposal 07", wallet: proposal07Wallet, auth: proposal07AuthFile },
  { name: "Proposal 08", wallet: proposal08Wallet, auth: proposal08AuthFile },
  { name: "Proposal 09", wallet: proposal09Wallet, auth: proposal09AuthFile },
];

for (const { name, wallet, auth } of proposalSetups) {
  setup(`Create ${name} auth`, async ({ page, context }) => {
    await createAuthWithUserName({
      page,
      context,
      wallet,
      auth,
    });
  });
}
