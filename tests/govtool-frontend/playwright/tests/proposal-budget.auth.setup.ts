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
import { skipIfNotHardFork } from "@helpers/cardano";

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Authentication");
  await skipIfNotHardFork();
});

setup("Create Budget Proposal 01 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: budgetProposal01Wallet,
    auth: budgetProposal01AuthFile,
  });
});

setup("Create Budget Proposal 02 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: budgetProposal02Wallet,
    auth: budgetProposal02AuthFile,
  });
});

setup("Create Budget Proposal 03 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: budgetProposal03Wallet,
    auth: budgetProposal03AuthFile,
  });
});

setup("Create Budget Proposal 04 auth", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: budgetProposal04Wallet,
    auth: budgetProposal04AuthFile,
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
