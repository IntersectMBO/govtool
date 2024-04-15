// Saves storage state to a file in the .auth directory

import { Page, expect } from "@playwright/test";
import loadDemosExtension from "@fixtures/loadExtension";
import createWallet from "@fixtures/createWallet";
import LoginPage from "@pages/loginPage";

const tempDRepAuthFile = ".auth/tempDRepAuth.json";
const tempUserAuth = ".auth/tempUserAuth.json";

export async function createTempDRepAuth(page: Page) {
  // const shellyWallet = await ShelleyWallet.generate();

  // const res = await kuberService.transferADA(
  //   faucetWallet.address,
  //   [shellyWallet.addressBech32(environments.networkId)],
  //   faucetWallet.payment.private,
  //   10
  // );
  // await pollTransaction(res.txId);

  // await injectWallet(page, mkCardanoWalletExtension());

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await expect(page.getByTestId("disconnect-button")).toBeVisible();

  await page.context().storageState({ path: tempDRepAuthFile });
  return tempDRepAuthFile;
}

export async function createTempUserAuth(page: Page) {
  await loadDemosExtension(page);
  await createWallet(page);

  const loginPage = new LoginPage(page);
  await loginPage.login();
  await expect(page.getByTestId("disconnect-button")).toBeVisible();

  await page.context().storageState({ path: tempUserAuth });
  return tempUserAuth;
}
