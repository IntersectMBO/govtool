import { user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { invalid as mockInvalid } from "@mock/index";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";

test.use({
  storageState: ".auth/user01.json",
  wallet: user01Wallet,
});

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
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

test.describe("Validation of dRep Registration Form", () => {
  test("3E_1 Should accept valid data in DRep form", async ({ page }) => {
    test.slow();

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

  test("3E_2. Should reject invalid data in DRep form", async ({ page }) => {
    test.slow();

    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    for (let i = 0; i < 100; i++) {
      await dRepRegistrationPage.inValidateForm(
        mockInvalid.name(),
        mockInvalid.email(),
        faker.lorem.paragraph(40),
        mockInvalid.url()
      );
    }
  });

  test("3L_1. Should accept valid dRep metadata anchor", async ({ page }) => {
    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    const dRepName = "Test_DRep";
    await dRepRegistrationPage.nameInput.fill(dRepName);

    await dRepRegistrationPage.continueBtn.click();
    await page.getByRole("checkbox").click();
    await dRepRegistrationPage.continueBtn.click();

    for (let i = 0; i < 100; i++) {
      await dRepRegistrationPage.metadataUrlInput.fill(faker.internet.url());
      await expect(page.getByTestId("invalid-url-error")).toBeHidden();
    }
  });

  test("3L_2. Should reject invalid dRep metadata anchor", async ({ page }) => {
    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    const dRepName = "Test_DRep";
    await dRepRegistrationPage.nameInput.fill(dRepName);

    await dRepRegistrationPage.continueBtn.click();
    await page.getByRole("checkbox").click();
    await dRepRegistrationPage.continueBtn.click();

    for (let i = 0; i < 100; i++) {
      await dRepRegistrationPage.metadataUrlInput.fill(mockInvalid.url());
      await expect(page.getByTestId("invalid-url-error")).toBeVisible();
    }

    const sentenceWithoutSpace = faker.lorem
      .sentence(128)
      .replace(/[\s.]/g, "");
    const metadataAnchorGreaterThan128Bytes =
      faker.internet.url({ appendSlash: true }) + sentenceWithoutSpace;

    await dRepRegistrationPage.metadataUrlInput.fill(
      metadataAnchorGreaterThan128Bytes
    );

    await expect(page.getByTestId("invalid-url-error")).toBeVisible();
  });
});

test("3F. Should create proper DRep registration request, when registered with data", async ({
  page,
}) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  await dRepRegistrationPage.registerWithoutTxConfirmation({ name: "Test" });
  await expect(
    page.getByTestId("registration-transaction-error-modal")
  ).toBeVisible();
});
