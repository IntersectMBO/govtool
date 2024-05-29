import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile } from "@helpers/mobile";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
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

test("2D. Should show delegation options in connected state", async ({
  page,
}) => {
  const dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();

  // Verifying automatic delegation options
  await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
  await expect(dRepDirectoryPage.abstainDelegationCard).toBeVisible();
  await expect(dRepDirectoryPage.signalNoConfidenceCard).toBeVisible();

  expect(await dRepDirectoryPage.delegateBtns.count()).toBeGreaterThanOrEqual(
    2
  );
});
