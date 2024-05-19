import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import DelegationPage from "@pages/dRepDirectoryPage";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

// Skipped: No dRepId to validate
test.skip("6B. Provides error for invalid format", async ({ page }) => {
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
