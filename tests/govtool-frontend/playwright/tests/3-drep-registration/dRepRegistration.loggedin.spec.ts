import { user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";

test.use({
  storageState: ".auth/user01.json",
  wallet: user01Wallet,
});

test("3B. Should access DRep registration page", async ({ page }) => {
  await page.goto("/");

  await page.getByTestId("register-button").click();
  await expect(page.getByText("Become a DRep")).toBeVisible();
});

test("3D. Verify DRep registration form", async ({ page }) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  await expect(dRepRegistrationPage.nameInput).toBeVisible();
  await expect(dRepRegistrationPage.emailInput).toBeVisible();
  await expect(dRepRegistrationPage.bioInput).toBeVisible();
  await expect(dRepRegistrationPage.linkInput).toBeVisible();
  await expect(dRepRegistrationPage.addLinkBtn).toBeVisible();
  await expect(dRepRegistrationPage.continueBtn).toBeVisible();
});

test("3E. Should reject invalid data in DRep form", async ({
  page,
}) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  // Invalidity test
  faker.helpers
    .multiple(() => faker.internet.displayName(), { count: 100 })
    .forEach(async (dRepName) => {
      await dRepRegistrationPage.nameInput.fill(dRepName);
      await dRepRegistrationPage.nameInput.clear();
    });

  // Validity test
});

test("3F. Should create proper DRep registration request, when registered with data", async ({
  page,
}) => {
  const urlToIntercept = "**/utxo?**";

  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  await dRepRegistrationPage.register({ name: "Test_dRep" });

  const response = await page.waitForResponse(urlToIntercept);
  expect(response.body.length).toEqual(0);
});
