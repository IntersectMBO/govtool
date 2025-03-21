import { importWallet } from "@fixtures/importWallet";
import { valid as mockValid } from "@mock/index";
import LoginPage from "@pages/loginPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { BrowserContext, Page } from "@playwright/test";
import { ProposalType, StaticWallet } from "@types";
import { ShelleyWallet } from "./crypto";
import convertBufferToHex from "./convertBufferToHex";
import { updateWalletConfig } from "@fixtures/createWallet";
import {
  adaHolder05Wallet,
  proposal05Wallet,
  proposal07Wallet,
  proposal08Wallet,
  proposal09Wallet,
} from "@constants/staticWallets";

interface CreateUserProps {
  page: Page;
  context: BrowserContext;
  wallet: StaticWallet;
  auth: string;
}

export async function createAuth({
  page,
  context,
  wallet,
  auth,
}: CreateUserProps) {
  await importWallet(page, wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: auth });
}

export async function createAuthWithUserName({
  page,
  context,
  wallet,
  auth,
}: CreateUserProps) {
  await importWallet(page, wallet);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();
  await proposalDiscussionPage.verifyIdentityBtn.click({ timeout: 15_000 });

  await proposalDiscussionPage.setUsername(mockValid.username());

  await context.storageState({ path: auth });
}

export async function createAuthWithMultipleStake({
  page,
  context,
  auth,
  wallet,
}) {
  const extraPubStakeKey = convertBufferToHex(
    ShelleyWallet.fromJson(adaHolder05Wallet).stakeKey.public
  );
  const extraRewardAddress = convertBufferToHex(
    ShelleyWallet.fromJson(adaHolder05Wallet).rewardAddressRawBytes(0)
  );

  await updateWalletConfig(
    page,
    {
      extraRegisteredPubStakeKeys: [extraPubStakeKey],
      extraRewardAddresses: [extraRewardAddress],
    },
    wallet
  );

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await loginPage.isLoggedIn();

  await context.storageState({ path: auth });
}

export const getDraftProposalWalletAndState = (proposalType: string) => {
  switch (proposalType) {
    case ProposalType.info:
      return {
        storageState: ".auth/proposal05.json",
        wallet: proposal05Wallet,
      };
    case ProposalType.treasury:
      return {
        storageState: ".auth/proposal07.json",
        wallet: proposal07Wallet,
      };
    case ProposalType.updatesToTheConstitution:
      return {
        storageState: ".auth/proposal08.json",
        wallet: proposal08Wallet,
      };
    case ProposalType.motionOfNoConfedence:
      return {
        storageState: ".auth/proposal09.json",
        wallet: proposal09Wallet,
      };
  }
};
