import { dRep02Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { invalid as mockInvalid, valid as mockValid } from "@mock/index";
import { skipIfMainnet, skipIfNotHardFork } from "@helpers/cardano";
import EditDRepPage from "@pages/editDRepPage";
import { expect } from "@playwright/test";
import environments from "@constants/environments";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
  await skipIfNotHardFork();
  await skipIfMainnet();
});

test.use({ wallet: dRep02Wallet, storageState: ".auth/dRep02.json" });

test.describe("Validation of edit dRep Form", () => {
  test("3M_1. Should accept valid data in edit dRep form", async ({ page }) => {
    test.slow();

    const editDRepPage = new EditDRepPage(page);
    await editDRepPage.goto();

    // wait until wallet alert close
    await page.waitForTimeout(5_000);

    for (let i = 0; i < 100; i++) {
      await editDRepPage.validateForm({
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
      await expect(editDRepPage.addLinkReferenceBtn).toBeVisible();
      await editDRepPage.addLinkReferenceBtn.click();
    }

    for (let i = 0; i < 6; i++) {
      await expect(editDRepPage.addIdentityReferenceBtn).toBeVisible();
      await editDRepPage.addIdentityReferenceBtn.click();
    }

    await expect(editDRepPage.addLinkReferenceBtn).toBeHidden();
    await expect(editDRepPage.addIdentityReferenceBtn).toBeHidden();
  });

  test("3M_2. Should reject invalid data in edit dRep form", async ({
    page,
  }) => {
    test.slow();

    const editDRepPage = new EditDRepPage(page);
    await editDRepPage.goto();

    await expect(editDRepPage.nameInput).toBeVisible({ timeout: 20_000 }); // assert to wait for the page to load

    for (let i = 0; i < 100; i++) {
      await editDRepPage.inValidateForm({
        name: mockInvalid.name(),
        objectives: faker.lorem.paragraph(40),
        motivations: faker.lorem.paragraph(40),
        qualifications: faker.lorem.paragraph(40),
        paymentAddress: faker.string.alphanumeric(45),
        linksReferenceLinks: [
          {
            url: mockInvalid.url(),
            description: faker.lorem.paragraph(40),
          },
        ],
        identityReferenceLinks: [
          {
            url: mockInvalid.url(),
            description: faker.lorem.paragraph(40),
          },
        ],
      });
    }
  });

  test("3N_1. Should accept valid metadata anchor on edit dRep", async ({
    page,
  }) => {
    const editDRepPage = new EditDRepPage(page);
    await editDRepPage.goto();

    const dRepName = "Test_DRep";
    await editDRepPage.nameInput.fill(dRepName);

    await editDRepPage.continueBtn.click();
    await page.getByRole("checkbox").click();
    await editDRepPage.registerBtn.click();

    for (let i = 0; i < 100; i++) {
      await editDRepPage.metadataUrlInput.fill(mockValid.url());
      await expect(page.getByTestId("invalid-url-error")).toBeHidden();
    }
  });

  test("3N_2. Should reject invalid dRep metadata anchor on edit dRep", async ({
    page,
  }) => {
    const editDRepPage = new EditDRepPage(page);
    await editDRepPage.goto();

    const dRepName = "Test_DRep";
    await editDRepPage.nameInput.fill(dRepName);

    await editDRepPage.continueBtn.click();
    await page.getByRole("checkbox").click();
    await editDRepPage.registerBtn.click();

    for (let i = 0; i < 100; i++) {
      const invalidUrl = mockInvalid.url(false);
      await editDRepPage.metadataUrlInput.fill(invalidUrl);
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

    await editDRepPage.metadataUrlInput.fill(metadataAnchorGreaterThan128Bytes);

    await expect(
      page.getByTestId("url-must-be-less-than-128-bytes-error")
    ).toBeVisible();
  });
});

test("3P. Should reject invalid edit dRep metadata", async ({ page }) => {
  const editDRepPage = new EditDRepPage(page);
  await editDRepPage.goto();

  const dRepName = "Test_DRep";
  await editDRepPage.nameInput.fill(dRepName);

  await editDRepPage.continueBtn.click();
  await page.getByRole("checkbox").click();
  await editDRepPage.registerBtn.click();

  const invalidMetadataAnchor = "https://www.google.com";
  await editDRepPage.metadataUrlInput.fill(invalidMetadataAnchor);
  await editDRepPage.submitBtn.click();

  await expect(page.getByTestId("modal")).toHaveText(
    /your external data does not/i
  );
});
