import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import DelegationPage from "@pages/dRepDirectoryPage";
import { setAllureEpic } from "@helpers/allure";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});
// Skipped: No dRepId to validate
test("6B. Provides error for invalid format", async ({ page }) => {
  test.skip();
  // invalid dRep delegation
  const delegationPage = new DelegationPage(page);
  await delegationPage.goto();
  await delegationPage.delegateToDRep("Random values");
  await expect(delegationPage.delegationErrorModal).toBeVisible();

  // invalid dRep registration
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  // await dRepRegistrationPage.urlInput.fill("abc");
  // await expect(dRepRegistrationPage.urlInputError).toBeVisible();

  // await dRepRegistrationPage.hashInput.fill("abc");
  // await expect(dRepRegistrationPage.hashInputError).toBeVisible();
});

test("6D: Proper label and recognition of the testnet network", async ({
  page,
}) => {
  await page.goto("/");

  await expect(page.getByText("testnet")).toBeVisible();
});
