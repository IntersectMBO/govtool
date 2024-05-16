import { dRep01Wallet, user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import convertBufferToHex from "@helpers/convertBufferToHex";
import { ShelleyWallet } from "@helpers/crypto";
import extractDRepFromWallet from "@helpers/shellyWallet";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test("2B. Should access delegation to dRep page", async ({ page }) => {
  await page.goto("/");

  await page.getByTestId("view-drep-directory-button").click();
  await expect(
    page.getByRole("navigation").getByText("DRep Directory")
  ).toBeVisible();
});

test("2I. Should check validity of DRep Id", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await expect(dRepDirectory.getDRepCard(dRep01Wallet.dRepId)).toHaveText(
    dRep01Wallet.dRepId
  );

  const wallet = await ShelleyWallet.generate();
  const invalidDRepId = extractDRepFromWallet(wallet);

  await dRepDirectory.searchInput.fill(invalidDRepId);
  await expect(dRepDirectory.getDRepCard(invalidDRepId)).not.toBeVisible();
});

test("2D. Verify Delegation Behavior in Connected State", async ({ page }) => {
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
