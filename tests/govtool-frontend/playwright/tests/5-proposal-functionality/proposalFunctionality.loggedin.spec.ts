import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test("5J. Should hide retirement option for non-registered DRep", async ({
  page,
}) => {
  await page.goto("/");
  await expect(page.getByTestId("retire-button")).not.toBeVisible();
});
