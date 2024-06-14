import environments from "@constants/environments";
import { dRep01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { expect } from "@playwright/test";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test.use({
  storageState: ".auth/dRep01.json",
  wallet: dRep01Wallet,
});

test("6H. Should restrict dRep registration for dRep", async ({ page }) => {
  await page.goto(`${environments.frontendUrl}/register_drep`);

  await page.waitForTimeout(2_000);

  await expect(page.getByTestId("name-input")).not.toBeVisible();
});
