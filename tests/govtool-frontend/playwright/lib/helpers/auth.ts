import { importWallet } from "@fixtures/importWallet";
import { valid as mockValid } from "@mock/index";
import LoginPage from "@pages/loginPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { BrowserContext, Page } from "@playwright/test";
import { StaticWallet } from "@types";

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
  await proposalDiscussionPage.verifyIdentityBtn.click();

  await proposalDiscussionPage.setUsername(mockValid.username());

  await context.storageState({ path: auth });
}
