import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("5. Proposal functionality");
  await skipIfNotHardFork();
});

test("5J. Should hide retirement option for non-registered DRep", async ({
  page,
}) => {
  await page.goto("/");
  await expect(page.getByTestId("retire-button")).not.toBeVisible();
});
