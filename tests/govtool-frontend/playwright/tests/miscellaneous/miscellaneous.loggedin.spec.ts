import { test } from "@fixtures/walletExtension";
import { user01Wallet } from "@constants/staticWallets";
import { expect } from "@playwright/test";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import DelegationPage from "@pages/delegationPage";

test.describe("Logged in specs", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

  test("6B. Provides error for invalid format @fast @smoke", async ({
    page,
  }) => {
    // invalid dRep delegation
    const delegationPage = new DelegationPage(page);
    await delegationPage.goto();
    await delegationPage.delegateToDrep("Random values");
    await expect(delegationPage.delegationErrorModal).toBeVisible();

    // invalid dRep registration
    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    await dRepRegistrationPage.urlInput.fill("abc");
    await expect(dRepRegistrationPage.urlInputError).toBeVisible();

    await dRepRegistrationPage.hashInput.fill("abc");
    await expect(dRepRegistrationPage.hashInputError).toBeVisible();
  });

  test("6D: Proper label and recognition of the testnet network @fast @smoke", async ({
    page,
  }) => {
    await page.goto("/");

    await expect(page.getByText("testnet")).toBeVisible();
  });
});
