// Saves storage state to a file in the .auth directory

import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  adaHolder05Wallet,
  adaHolder06Wallet,
  budgetProposal01Wallet,
  budgetProposal02Wallet,
  budgetProposal03Wallet,
  budgetProposal04Wallet,
  dRep01Wallet,
  dRep02Wallet,
  dRep03Wallet,
  proposal01Wallet,
  proposal02Wallet,
  proposal03Wallet,
  proposal04Wallet,
  proposal05Wallet,
  proposal06Wallet,
  proposal07Wallet,
  proposal08Wallet,
  proposal09Wallet,
  user01Wallet,
} from "@constants/staticWallets";
import { test as setup } from "@fixtures/walletExtension";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import {
  createAuth,
  createAuthWithMultipleStake,
  createAuthWithUserName,
} from "@helpers/auth";
import { skipIfNotHardFork } from "@helpers/cardano";

const dRep01AuthFile = ".auth/dRep01.json";
const dRep02AuthFile = ".auth/dRep02.json";
const dRep03AuthFile = ".auth/dRep03.json";

const adaHolder01AuthFile = ".auth/adaHolder01.json";
const adaHolder02AuthFile = ".auth/adaHolder02.json";
const adaHolder03AuthFile = ".auth/adaHolder03.json";
const adaHolder04AuthFile = ".auth/adaHolder04.json";
const adaHolder05AuthFile = ".auth/adaHolder05.json";
const adaHolder06AuthFile = ".auth/adaHolder06.json";

const user01AuthFile = ".auth/user01.json";

const proposal01AuthFile = ".auth/proposal01.json";
const proposal02AuthFile = ".auth/proposal02.json";
const proposal03AuthFile = ".auth/proposal03.json";
const proposal04AuthFile = ".auth/proposal04.json";
const proposal05AuthFile = ".auth/proposal05.json";
const proposal06AuthFile = ".auth/proposal06.json";
const proposal07AuthFile = ".auth/proposal07.json";
const proposal08AuthFile = ".auth/proposal08.json";
const proposal09AuthFile = ".auth/proposal09.json";

const budgetProposal01AuthFile = ".auth/budgetProposal01.json";
const budgetProposal02AuthFile = ".auth/budgetProposal02.json";
const budgetProposal03AuthFile = ".auth/budgetProposal03.json";
const budgetProposal04AuthFile = ".auth/budgetProposal04.json";

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

setup("Create DRep 03 auth with username", async ({ page, context }) => {
  await createAuthWithUserName({
    page,
    context,
    wallet: dRep03Wallet,
    auth: dRep03AuthFile,
  });
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
