import { test } from "@fixtures/walletExtension";
import { user01Wallet } from "@constants/staticWallets";
import { expect } from "@playwright/test";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import * as crypto from "crypto";

test.describe("Logged in specs", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

  test("3B. Should access DRep registration page @fast @smoke", async ({
    page,
  }) => {
    await page.goto("/");

    await page.getByTestId("register-button").click();
    await expect(page.getByText(/Add information/i)).toBeVisible();
  });

  test("3D.Verify DRep registration functionality with Wallet Connected State State @fast", async ({
    page,
  }) => {
    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    await expect(dRepRegistrationPage.urlInput).toBeVisible();
    await expect(dRepRegistrationPage.hashInput).toBeVisible();
    await expect(dRepRegistrationPage.skipBtn).toBeVisible();
  });

  test("3E. Should reject invalid data and accept valid data @smoke @fast", async ({
    page,
  }) => {
    // Invalid data;
    const invalidUrls = [
      `https://ipfs.io/ipfs/${crypto.randomBytes(32).toString("hex")}`,
      "abcd",
      " ",
    ];
    const invalidHashes = [
      `ipfs://${crypto.randomBytes(32).toString("hex")}`,
      "abcd",
    ];

    // Valid data;
    const validUrl = "ipfs://" + crypto.randomBytes(28).toString("hex");
    const validHash = crypto.randomBytes(32).toString("hex");

    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    // Invalidity test
    invalidUrls.forEach(async (invalidUrl) => {
      await dRepRegistrationPage.urlInput.fill(invalidUrl);
      await expect(dRepRegistrationPage.urlInputError).toBeVisible();
      await dRepRegistrationPage.urlInput.clear();
    });

    invalidHashes.forEach(async (invalidHash) => {
      await dRepRegistrationPage.hashInput.fill(invalidHash);
      await expect(dRepRegistrationPage.hashInputError).toBeVisible();
      await dRepRegistrationPage.hashInput.clear();
    });

    // Validity test
    await dRepRegistrationPage.urlInput.fill(validUrl);
    await expect(dRepRegistrationPage.urlInputError).not.toBeVisible();

    await dRepRegistrationPage.hashInput.fill(validHash);
    await expect(dRepRegistrationPage.hashInputError).not.toBeVisible();
  });

  test("3F. Should create proper DRep registration request, when registered with data @slow @regression", async ({
    page,
  }) => {
    const urlToIntercept = "**/utxo?**";

    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    await dRepRegistrationPage.register(
      "https://google.com",
      crypto.randomBytes(32).toString("hex"),
    );

    const response = await page.waitForResponse(urlToIntercept);
    expect(response.body.length).toEqual(0);
  });
});
