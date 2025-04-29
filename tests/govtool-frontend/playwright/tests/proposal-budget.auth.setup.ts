import {
  budgetProposal01AuthFile,
  budgetProposal02AuthFile,
  budgetProposal03AuthFile,
  budgetProposal04AuthFile,
  dRep03AuthFile,
} from "@constants/auth";
import {
  budgetProposal01Wallet,
  budgetProposal02Wallet,
  budgetProposal03Wallet,
  budgetProposal04Wallet,
  dRep03Wallet,
} from "@constants/staticWallets";
import { test as setup } from "@fixtures/walletExtension";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { createAuthWithUserName } from "@helpers/auth";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
});

const walletAuthPairs = [
  {
    wallet: budgetProposal01Wallet,
    auth: budgetProposal01AuthFile,
    name: "Budget Proposal 01",
  },
  {
    wallet: budgetProposal02Wallet,
    auth: budgetProposal02AuthFile,
    name: "Budget Proposal 02",
  },
  {
    wallet: budgetProposal03Wallet,
    auth: budgetProposal03AuthFile,
    name: "Budget Proposal 03",
  },
  {
    wallet: budgetProposal04Wallet,
    auth: budgetProposal04AuthFile,
    name: "Budget Proposal 04",
  },
];

walletAuthPairs.forEach(({ wallet, auth, name }) => {
  setup(`Create ${name} auth`, async ({ page, context }) => {
    await createAuthWithUserName({
      page,
      context,
      wallet,
      auth,
    });
  });
});

setup("Create DRep 03 auth with username", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: dRep03Wallet,
    auth: dRep03AuthFile,
  });
});
