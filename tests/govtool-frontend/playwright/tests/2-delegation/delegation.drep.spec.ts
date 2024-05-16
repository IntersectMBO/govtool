import { dRep01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect } from "@playwright/test";

test("2L. Should copy DRepId", async ({ page }) => {
  const dRepDirectory = new DRepDirectoryPage(page);
  await dRepDirectory.goto();

  await dRepDirectory.searchInput.fill(dRep01Wallet.dRepId);
  await page.getByTestId(`${dRep01Wallet.dRepId}-copy-id-button`).click();
  await expect(page.getByText("Copied to clipboard")).toBeVisible();
});
