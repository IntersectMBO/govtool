import { user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { invalid as mockInvalid, valid as mockValid } from "@mock/index";
import DRepRegistrationPage from "@pages/dRepRegistrationPage";
import { expect } from "@playwright/test";
import environments from "@constants/environments";
import { user01AuthFile } from "@constants/auth";
import EditDRepPage from "@pages/editDRepPage";

test.use({
  storageState: user01AuthFile,
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
  await expect(dRepRegistrationPage.objectivesInput).toBeVisible();
  await expect(dRepRegistrationPage.motivationsInput).toBeVisible();
  await expect(dRepRegistrationPage.qualificationsInput).toBeVisible();
  await expect(dRepRegistrationPage.paymentAddressInput).toBeVisible();
  await expect(dRepRegistrationPage.addIdentityReferenceBtn).toBeVisible();
  await expect(dRepRegistrationPage.addLinkReferenceBtn).toBeVisible();
  await expect(
    dRepRegistrationPage.linkRefrenceFirstDescriptionInput
  ).toBeVisible();
  await expect(dRepRegistrationPage.linkRefrenceFirstUrlInput).toBeVisible();
  await expect(
    dRepRegistrationPage.identityReferenceFirstDescriptionInput
  ).toBeVisible();
  await expect(
    dRepRegistrationPage.identityReferenceFirstUrlInput
  ).toBeVisible();
  await expect(dRepRegistrationPage.doNotListCheckBox).toBeVisible();

  await expect(dRepRegistrationPage.continueBtn).toBeVisible();
});

test.describe("Validation of dRep Registration Form", () => {
  test("3E_1. Should accept valid data in DRep form", async ({ page }) => {
    test.slow();

    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    await expect(page.getByTestId("alert-success")).not.toBeVisible();

    for (let i = 0; i < 100; i++) {
      await dRepRegistrationPage.validateForm({
        name: mockValid.name(),
        objectives: faker.lorem.paragraph(2),
        motivations: faker.lorem.paragraph(2),
        qualifications: faker.lorem.paragraph(2),
        paymentAddress: (await ShelleyWallet.generate()).addressBech32(
          environments.networkId
        ),
        linksReferenceLinks: [
          {
            url: faker.internet.url(),
            description: faker.internet.displayName(),
          },
        ],
        identityReferenceLinks: [
          {
            url: faker.internet.url(),
            description: faker.internet.displayName(),
          },
        ],
      });
    }

    for (let i = 0; i < 6; i++) {
      await expect(dRepRegistrationPage.addLinkReferenceBtn).toBeVisible();
      await dRepRegistrationPage.addLinkReferenceBtn.click();
    }

    for (let i = 0; i < 6; i++) {
      await expect(dRepRegistrationPage.addIdentityReferenceBtn).toBeVisible();
      await dRepRegistrationPage.addIdentityReferenceBtn.click();
    }

    await expect(dRepRegistrationPage.addLinkReferenceBtn).toBeHidden();
    await expect(dRepRegistrationPage.addIdentityReferenceBtn).toBeHidden();
  });

  test("3E_2. Should reject invalid data in DRep form", async ({ page }) => {
    test.slow();

    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();
    await expect(page.getByTestId("alert-success")).not.toBeVisible();

    for (let i = 0; i < 100; i++) {
      await dRepRegistrationPage.inValidateForm({
        name: mockInvalid.name(),
        objectives: faker.lorem.paragraph(40),
        motivations: faker.lorem.paragraph(40),
        qualifications: faker.lorem.paragraph(40),
        paymentAddress: faker.string.alphanumeric(45),
        linksReferenceLinks: [
          {
            url: mockInvalid.url(),
            description: faker.lorem.paragraph(20),
          },
        ],
        identityReferenceLinks: [
          {
            url: mockInvalid.url(),
            description: faker.lorem.paragraph(20),
          },
        ],
      });
    }
  });

  test("3L_1. Should accept valid metadata anchor on create dRep", async ({
    page,
  }) => {
    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    const dRepName = "Test_DRep";
    await dRepRegistrationPage.nameInput.fill(dRepName);

    await dRepRegistrationPage.continueBtn.click();
    await page.getByRole("checkbox").click();
    await dRepRegistrationPage.registerBtn.click();

    for (let i = 0; i < 100; i++) {
      await dRepRegistrationPage.metadataUrlInput.fill(mockValid.url());
      await expect(page.getByTestId("invalid-url-error")).toBeHidden();
    }
  });

  test("3L_2. Should reject invalid dRep metadata anchor on create dRep", async ({
    page,
  }) => {
    const dRepRegistrationPage = new DRepRegistrationPage(page);
    await dRepRegistrationPage.goto();

    const dRepName = "Test_DRep";
    await dRepRegistrationPage.nameInput.fill(dRepName);

    await dRepRegistrationPage.continueBtn.click();
    await page.getByRole("checkbox").click();
    await dRepRegistrationPage.registerBtn.click();

    for (let i = 0; i < 100; i++) {
      const invalidUrl = mockInvalid.url(false);

      await dRepRegistrationPage.metadataUrlInput.fill(invalidUrl);
      if (invalidUrl.length <= 128) {
        await expect(page.getByTestId("invalid-url-error")).toBeVisible();
      } else {
        await expect(
          page.getByTestId("url-must-be-less-than-128-bytes-error")
        ).toBeVisible();
      }
    }

    const sentenceWithoutSpace = faker.lorem
      .sentence(128)
      .replace(/[\s.]/g, "");
    const metadataAnchorGreaterThan128Bytes =
      faker.internet.url({ appendSlash: true }) + sentenceWithoutSpace;

    await dRepRegistrationPage.metadataUrlInput.fill(
      metadataAnchorGreaterThan128Bytes
    );

    await expect(
      page.getByTestId("url-must-be-less-than-128-bytes-error")
    ).toBeVisible();
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
  ).toBeVisible({ timeout: 60_000 });
});

test("3O. Should reject invalid dRep registration metadata", async ({
  page,
}) => {
  const dRepRegistrationPage = new DRepRegistrationPage(page);
  await dRepRegistrationPage.goto();

  const dRepName = "Test_DRep";
  await dRepRegistrationPage.nameInput.fill(dRepName);

  await dRepRegistrationPage.continueBtn.click();
  await page.getByRole("checkbox").click();
  await dRepRegistrationPage.registerBtn.click();

  const invalidMetadataAnchor = "https://www.google.com";
  await dRepRegistrationPage.metadataUrlInput.fill(invalidMetadataAnchor);
  await dRepRegistrationPage.submitBtn.click();

  await expect(dRepRegistrationPage.metadataErrorModal).toHaveText(
    /your external data does not/i
  );
});

test("3R. Should restrict edit dRep for non dRep", async ({ page }) => {
  const editDrepPage = new EditDRepPage(page);
  await editDrepPage.goto();

  await page.waitForTimeout(2_000);
  await expect(editDrepPage.nameInput).not.toBeVisible();
});
