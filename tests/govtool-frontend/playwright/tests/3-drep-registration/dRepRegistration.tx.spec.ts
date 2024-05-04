import { dRep01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { expect } from "@playwright/test";
import * as crypto from "crypto";

test.describe("Logged in", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  // Skipped: No option to update metadata
  test.skip("3H. Should be able to update metadata @slow", async ({ page }) => {
    page.getByTestId("change-metadata-button").click();
    page.getByTestId("url-input").fill("https://google.com");
    page.getByTestId("hash-input").fill(crypto.randomBytes(32).toString("hex"));
    await expect(page.getByTestId("confirm-modal-button")).toBeVisible();
  });
});
