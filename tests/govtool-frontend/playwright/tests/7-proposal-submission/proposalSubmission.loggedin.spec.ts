import { proposal01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { invalid } from "@mock/index";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { ProposalCreateRequest, ProposalType } from "@types";
import { bech32 } from "bech32";

test.use({ storageState: ".auth/proposal01.json", wallet: proposal01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
});

test.describe("Accept valid data", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7E_${index + 1}. Should accept valid data in ${type.toLowerCase()} proposal form`, async ({
      page,
    }) => {
      test.slow(); // Brute-force testing with 100 random data

      const proposalSubmissionPage = new ProposalSubmissionPage(page);

      await proposalSubmissionPage.goto();

      await proposalSubmissionPage.continueBtn.click();
      await proposalSubmissionPage.governanceActionType.click();
      await page.getByRole("option", { name: type }).click();
      await proposalSubmissionPage.addLinkBtn.click();

      for (let i = 0; i < 100; i++) {
        const rewardAddressBech32 = (
          await ShelleyWallet.generate()
        ).rewardAddressBech32(0);
        const formFields: ProposalCreateRequest =
          proposalSubmissionPage.generateValidProposalFormFields(
            type,
            false,
            rewardAddressBech32
          );
        await proposalSubmissionPage.validateForm(formFields);
      }

      for (let i = 0; i < 6; i++) {
        await expect(proposalSubmissionPage.addLinkBtn).toBeVisible();
        await proposalSubmissionPage.addLinkBtn.click();
      }

      await expect(proposalSubmissionPage.addLinkBtn).toBeHidden();
    });
  });
});

test.describe("Reject invalid  data", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7F_${index + 1}. Should reject invalid data in ${type.toLowerCase()} Proposal form`, async ({
      page,
    }) => {
      test.slow(); // Brute-force testing with 100 random data

      const proposalSubmissionPage = new ProposalSubmissionPage(page);
      await proposalSubmissionPage.goto();

      await proposalSubmissionPage.continueBtn.click();
      await proposalSubmissionPage.governanceActionType.click();
      await page.getByRole("option", { name: type }).click();
      await proposalSubmissionPage.addLinkBtn.click();

      const formFields: ProposalCreateRequest =
        proposalSubmissionPage.generateInValidProposalFormFields(type);

      for (let i = 0; i < 100; i++) {
        await proposalSubmissionPage.inValidateForm(formFields);
      }
    });
  });
});

test.describe("Proposal submission check", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7G_${index + 1}. Should open wallet connection popup, when registered with proper ${type.toLowerCase()} data`, async ({
      page,
      wallet,
    }) => {
      const proposalSubmissionPage = new ProposalSubmissionPage(page);

      await proposalSubmissionPage.goto();

      await page.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      const walletAddressBech32 =
        ShelleyWallet.fromJson(wallet).rewardAddressBech32(0);

      const proposal: ProposalCreateRequest =
        proposalSubmissionPage.generateValidProposalFormFields(
          type,
          false,
          walletAddressBech32
        );
      await proposalSubmissionPage.register({ ...proposal });
      await expect(
        proposalSubmissionPage.registrationErrorModal.getByText(
          "UTxO Balance Insufficient"
        )
      ).toBeVisible();
    });
  });
});

test.describe("Review fillup form", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7I_${index + 1}. Should valid review submission in ${type.toLowerCase()} Proposal form`, async ({
      page,
    }) => {
      const proposalSubmissionPage = new ProposalSubmissionPage(page);

      await proposalSubmissionPage.goto();

      await page.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      const randomBytes = new Uint8Array(10);
      const bech32Address = bech32.encode("addr_test", randomBytes);

      const formFields: ProposalCreateRequest =
        proposalSubmissionPage.generateValidProposalFormFields(
          type,
          false,
          bech32Address
        );
      await proposalSubmissionPage.validateForm(formFields);
      proposalSubmissionPage.continueBtn.click();

      await expect(page.getByText(formFields.prop_name)).toBeVisible();
      await expect(page.getByText(formFields.prop_abstract)).toBeVisible();
      await expect(page.getByText(formFields.prop_motivation)).toBeVisible();
      await expect(page.getByText(formFields.prop_rationale)).toBeVisible();
      await expect(
        page.getByText(formFields.proposal_links[0].prop_link)
      ).toBeVisible();
      await expect(
        page.getByText(formFields.proposal_links[0].prop_link_text)
      ).toBeVisible();

      if (type === ProposalType.treasury) {
        await expect(
          page.getByText(formFields.prop_receiving_address)
        ).toBeVisible();
        await expect(page.getByText(formFields.prop_amount)).toBeVisible();
      }
    });
  });
});

test("7L. Should reject invalid proposal metadata", async ({ page }) => {
  const invalidMetadataAnchor = "https://www.google.com";

  const proposalSubmissionPage = new ProposalSubmissionPage(page);
  await proposalSubmissionPage.goto();
  ``;
  await page.getByTestId(`${ProposalType.info}-radio`).click();
  await proposalSubmissionPage.continueBtn.click();

  const proposal: ProposalCreateRequest =
    proposalSubmissionPage.generateValidProposalFormFields(ProposalType.info);

  await proposalSubmissionPage.fillupForm(proposal);
  await proposalSubmissionPage.continueBtn.click();
  await proposalSubmissionPage.continueBtn.click();
  await page.getByRole("checkbox").click();
  await proposalSubmissionPage.continueBtn.click();

  await proposalSubmissionPage.metadataUrlInput.fill(invalidMetadataAnchor);
  await proposalSubmissionPage.continueBtn.click();

  await expect(proposalSubmissionPage.registrationErrorModal).not.toHaveText(
    /utxo balance insufficient/i
  );
});

test.describe("Edit proposal form", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7J_${index + 1}. Should edit review submission in ${type.toLowerCase()} Proposal form`, async ({
      page,
    }) => {
      const proposalSubmissionPage = new ProposalSubmissionPage(page);

      await proposalSubmissionPage.goto();

      await page.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      const randomBytes = new Uint8Array(10);
      const bech32Address = bech32.encode("addr_test", randomBytes);

      const formFields: ProposalCreateRequest =
        proposalSubmissionPage.generateValidProposalFormFields(
          type,
          false,
          bech32Address
        );
      await proposalSubmissionPage.validateForm(formFields);
      proposalSubmissionPage.continueBtn.click();

      await proposalSubmissionPage.editSubmissionButton.click();

      const newTitle = faker.person.firstName();

      await proposalSubmissionPage.titleInput.fill(newTitle);

      await proposalSubmissionPage.continueBtn.click();

      await expect(page.getByText(newTitle)).toBeVisible();
      await expect(page.getByText(formFields.prop_abstract)).toBeVisible();
      await expect(page.getByText(formFields.prop_motivation)).toBeVisible();
      await expect(page.getByText(formFields.prop_rationale)).toBeVisible();
      await expect(
        page.getByText(formFields.proposal_links[0].prop_link)
      ).toBeVisible();
      await expect(
        page.getByText(formFields.proposal_links[0].prop_link_text)
      ).toBeVisible();

      if (type === ProposalType.treasury) {
        await expect(
          page.getByText(formFields.prop_receiving_address)
        ).toBeVisible();
        await expect(page.getByText(formFields.prop_amount)).toBeVisible();
      }
    });
  });
});

test("7K_1. Should accept valid metadata anchor on proposal submission", async ({
  page,
}) => {
  const invalidMetadataAnchor = "https://www.google.com";

  const proposalSubmissionPage = new ProposalSubmissionPage(page);
  await proposalSubmissionPage.goto();
  ``;
  await page.getByTestId(`${ProposalType.info}-radio`).click();
  await proposalSubmissionPage.continueBtn.click();

  const proposal: ProposalCreateRequest =
    proposalSubmissionPage.generateValidProposalFormFields(ProposalType.info);

  await proposalSubmissionPage.fillupForm(proposal);
  await proposalSubmissionPage.continueBtn.click();
  await proposalSubmissionPage.continueBtn.click();
  await page.getByRole("checkbox").click();
  await proposalSubmissionPage.continueBtn.click();

  for (let i = 0; i < 100; i++) {
    await proposalSubmissionPage.metadataUrlInput.fill(faker.internet.url());
    await expect(page.getByTestId("invalid-url-error")).toBeHidden();
  }
});

test("7K_2. Should reject invalid metadata anchor on proposal submission", async ({
  page,
}) => {
  const invalidMetadataAnchor = "https://www.google.com";

  const proposalSubmissionPage = new ProposalSubmissionPage(page);
  await proposalSubmissionPage.goto();
  ``;
  await page.getByTestId(`${ProposalType.info}-radio`).click();
  await proposalSubmissionPage.continueBtn.click();

  const proposal: ProposalCreateRequest =
    proposalSubmissionPage.generateValidProposalFormFields(ProposalType.info);

  await proposalSubmissionPage.fillupForm(proposal);
  await proposalSubmissionPage.continueBtn.click();
  await proposalSubmissionPage.continueBtn.click();
  await page.getByRole("checkbox").click();
  await proposalSubmissionPage.continueBtn.click();

  for (let i = 0; i < 100; i++) {
    await proposalSubmissionPage.metadataUrlInput.fill(invalid.url());
    await expect(page.getByTestId("invalid-url-error")).toBeVisible();
  }

  const sentenceWithoutSpace = faker.lorem.sentence(128).replace(/[\s.]/g, "");
  const metadataAnchorGreaterThan128Bytes =
    faker.internet.url({ appendSlash: true }) + sentenceWithoutSpace;

  await proposalSubmissionPage.metadataUrlInput.fill(
    metadataAnchorGreaterThan128Bytes
  );

  await expect(page.getByTestId("invalid-url-error")).toBeVisible();
});
