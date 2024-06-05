import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile } from "@helpers/mobile";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
});

test("2B. Should access DRep Directory page", async ({ page }) => {
  await page.goto("/");

  await page.getByTestId("view-drep-directory-button").click();
  if (isMobile(page)) {
    await expect(page.getByText("DRep Directory")).toBeVisible();
  } else {
    await expect(
      page.getByRole("navigation").getByText("DRep Directory")
    ).toBeVisible();
  }
});
