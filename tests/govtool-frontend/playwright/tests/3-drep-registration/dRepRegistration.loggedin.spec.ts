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

test("3E. Should accept valid data in DRep form", async ({ page }) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  for (let i = 0; i < 100; i++) {
    await dRepRegistrationPage.validateForm(
      faker.internet.displayName(),
      faker.internet.email(),
      faker.lorem.paragraph(),
      faker.internet.url()
    );
  }

  for (let i = 0; i < 6; i++) {
    await expect(dRepRegistrationPage.addLinkBtn).toBeVisible();
    await dRepRegistrationPage.addLinkBtn.click();
  }

  await expect(dRepRegistrationPage.addLinkBtn).toBeHidden();
});

test("3L. Should reject invalid data in DRep form", async ({ page }) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  function generateInvalidEmail() {
    const choice = faker.number.int({ min: 1, max: 3 });

    if (choice === 1) {
      return faker.lorem.word() + faker.number + "@invalid.com";
    } else if (choice == 2) {
      return faker.lorem.word() + "@";
    }
    return faker.lorem.word() + "@gmail_com";
  }
  function generateInvalidUrl() {
    const choice = faker.number.int({ min: 1, max: 3 });

    if (choice === 1) {
      return faker.internet.url().replace("https://", "http://");
    } else if (choice === 2) {
      return faker.lorem.word() + ".invalid";
    }
    return faker.lorem.word() + ".@com";
  }
  function generateInvalidName() {
    const choice = faker.number.int({ min: 1, max: 3 });
    if (choice === 1) {
      // space invalid
      return faker.lorem.word() + " " + faker.lorem.word();
    } else if (choice === 2) {
      // maximum 80 words invalid
      return faker.lorem.paragraphs().replace(/\s+/g, "");
    }
    // empty invalid
    return " ";
  }

  for (let i = 0; i < 100; i++) {
    await dRepRegistrationPage.inValidateForm(
      generateInvalidName(),
      generateInvalidEmail(),
      faker.lorem.paragraph(40),
      generateInvalidUrl()
    );
  }
});

test("3F. Should create proper DRep registration request, when registered with data", async ({
  page,
}) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  await dRepRegistrationPage.register({ name: "Test" }).catch((err) => {
    // Fails because real tx is not submitted
  });

  await expect(
    page.getByTestId("registration-transaction-error-modal")
  ).toBeVisible();
});
