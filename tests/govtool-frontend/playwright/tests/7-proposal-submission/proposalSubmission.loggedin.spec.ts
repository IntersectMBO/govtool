import {
  proposal01Wallet,
  proposal03Wallet,
  proposal04Wallet,
  proposal05Wallet,
  proposal06Wallet,
  proposal07Wallet,
} from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import {
  skipIfTreasuryAndBootstrapping,
  skipIfNotHardFork,
} from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { invalid, valid as mockValid } from "@mock/index";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { ProposalCreateRequest, ProposalType } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
  await skipIfNotHardFork();
});

test.describe("Proposal created logged state", () => {
  test.use({ storageState: ".auth/proposal01.json", wallet: proposal01Wallet });
  test("7B. Should access proposal creation page", async ({ page }) => {
    await page.goto("/");
    await page.getByTestId("propose-governance-actions-button").click();

    await expect(page.getByText(/proposals/i)).toHaveCount(2);
  });

  test.describe("Accept valid data", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7E_${index + 1}. Should accept valid data in ${type.toLowerCase()} proposal form`, async ({
        page,
      }) => {
        await skipIfTreasuryAndBootstrapping(type);

        test.slow(); // Brute-force testing with 50 random data

        const proposalSubmissionPage = new ProposalSubmissionPage(page);

        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByTestId(`${type.toLocaleLowerCase()}-button`).click();
        await proposalSubmissionPage.addLinkBtn.click();

        for (let i = 0; i < 50; i++) {
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
        await skipIfTreasuryAndBootstrapping(type);

        test.slow(); // Brute-force testing with 50 random data

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByTestId(`${type.toLocaleLowerCase()}-button`).click();
        await proposalSubmissionPage.addLinkBtn.click();

        for (let i = 0; i < 50; i++) {
          const formFields: ProposalCreateRequest =
            proposalSubmissionPage.generateInValidProposalFormFields(type);
          await proposalSubmissionPage.inValidateForm(formFields);
        }
      });
    });
  });

  test.describe("Create a proposal with proper data", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7G_${index + 1}. Should create a proposal with proper ${type.toLowerCase()} data`, async ({
        page,
        wallet,
      }) => {
        await skipIfTreasuryAndBootstrapping(type);

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.addLinkBtn.click();

        const walletAddressBech32 =
          ShelleyWallet.fromJson(wallet).rewardAddressBech32(0);
        const proposal: ProposalCreateRequest =
          proposalSubmissionPage.generateValidProposalFormFields(
            type,
            false,
            walletAddressBech32
          );

        await proposalSubmissionPage.fillupForm(proposal);
        await proposalSubmissionPage.continueBtn.click();
        await proposalSubmissionPage.submitBtn.click();

        await expect(page.getByTestId("submit-as-GA-button")).toBeVisible();
        await expect(page.getByTestId("title-content")).toHaveText(
          proposal.prop_name
        );
        await expect(
          page.getByTestId("governance-action-type-content")
        ).toHaveText(type);
        await expect(page.getByTestId("abstract-content")).toHaveText(
          proposal.prop_abstract
        );
        await expect(page.getByTestId("motivation-content")).toHaveText(
          proposal.prop_motivation
        );
        await expect(page.getByTestId("rationale-content")).toHaveText(
          proposal.prop_rationale
        );
        await expect(page.getByTestId("link-0-text-content")).toHaveText(
          proposal.proposal_links[0].prop_link_text
        );
      });
    });
  });

  test.describe("Review fillup form", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7I_${index + 1}. Should valid review submission in ${type.toLowerCase()} Proposal form`, async ({
        page,
      }) => {
        await skipIfTreasuryAndBootstrapping(type);

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.addLinkBtn.click();

        const walletAddressBech32 =
          ShelleyWallet.fromJson(proposal01Wallet).rewardAddressBech32(0);
        const proposal: ProposalCreateRequest =
          proposalSubmissionPage.generateValidProposalFormFields(
            type,
            false,
            walletAddressBech32
          );

        await proposalSubmissionPage.fillupForm(proposal);
        await proposalSubmissionPage.continueBtn.click();

        await expect(
          page.getByTestId("governance-action-type-content")
        ).toHaveText(type);
        await expect(page.getByTestId("title-content")).toHaveText(
          proposal.prop_name
        );
        await expect(page.getByTestId("abstract-content")).toHaveText(
          proposal.prop_abstract
        );
        await expect(page.getByTestId("motivation-content")).toHaveText(
          proposal.prop_motivation
        );
        await expect(page.getByTestId("rationale-content")).toHaveText(
          proposal.prop_rationale
        );
        await expect(page.getByTestId("link-0-text-content")).toHaveText(
          proposal.proposal_links[0].prop_link_text
        );
        if (type === ProposalType.treasury) {
          await expect(
            page.getByTestId("receiving-address-content")
          ).toHaveText(proposal.prop_receiving_address);
          await expect(page.getByTestId("amount-content")).toHaveText(
            proposal.prop_amount
          );
        }
      });
    });
  });

  test.describe("Verify Proposal form", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7D_${index + 1}. Verify ${type.toLocaleLowerCase()} proposal form`, async ({
        page,
      }) => {
        await skipIfTreasuryAndBootstrapping(type);

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByRole("option", { name: type }).click();

        await expect(proposalSubmissionPage.titleInput).toBeVisible();
        await expect(proposalSubmissionPage.abstractInput).toBeVisible();
        await expect(proposalSubmissionPage.motivationInput).toBeVisible();
        await expect(proposalSubmissionPage.rationaleInput).toBeVisible();
        await expect(proposalSubmissionPage.addLinkBtn).toBeVisible();
        if (type === ProposalType.treasury) {
          await expect(
            proposalSubmissionPage.receivingAddressInput
          ).toBeVisible();

          await expect(proposalSubmissionPage.amountInput).toBeVisible();
        }
      });
    });
  });

  test.describe("proposed as a governance action", () => {
    let proposalSubmissionPage: ProposalSubmissionPage;
    test.beforeEach(async ({ page, proposalId }) => {
      const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
        page
      );
      await proposalDiscussionDetailsPage.goto(proposalId);

      await proposalDiscussionDetailsPage.verifyIdentityBtn.click();
      await proposalDiscussionDetailsPage.submitAsGABtn.click();

      proposalSubmissionPage = new ProposalSubmissionPage(page);
      await page.getByTestId("agree-checkbox").click();
      await proposalSubmissionPage.continueBtn.click();
    });

    test.describe("Metadata anchor validation", () => {
      test("7J_1. Should accept valid metadata anchor on proposal submission", async ({
        page,
      }) => {
        test.slow(); // Brute-force testing with 100 random data
        for (let i = 0; i < 50; i++) {
          await proposalSubmissionPage.metadataUrlInput.fill(mockValid.url());
          await expect(page.getByTestId("url-input-error-text")).toBeHidden();
        }
      });

      test("7J_2. Should reject invalid metadata anchor on proposal submission", async ({
        page,
      }) => {
        test.slow(); // Brute-force testing with 100 random data
        for (let i = 0; i < 50; i++) {
          await proposalSubmissionPage.metadataUrlInput.fill(
            invalid.url(false)
          );
          await expect(page.getByTestId("url-input-error-text")).toBeVisible();
        }

        const sentenceWithoutSpace = faker.lorem
          .sentence(128)
          .replace(/[\s.]/g, "");
        const metadataAnchorGreaterThan128Bytes =
          faker.internet.url({ appendSlash: true }) + sentenceWithoutSpace;

        await proposalSubmissionPage.metadataUrlInput.fill(
          metadataAnchorGreaterThan128Bytes
        );

        await expect(page.getByTestId("url-input-error-text")).toBeVisible(); // BUG better to add different test id compare to invalid url testid
      });
    });

    test("7K. Should reject invalid proposal metadata", async ({ page }) => {
      await proposalSubmissionPage.metadataUrlInput.fill(faker.internet.url());
      await proposalSubmissionPage.submitBtn.click();

      await expect(page.getByTestId("url-error-modal-title")).toHaveText(
        /the url you entered cannot be found/i
      );
    });
  });
});

test.describe("Info Proposal Draft", () => {
  test("7C. Should list unfinished Draft ", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal03.json",
      wallet: proposal03Wallet,
    });
    const proposalSubmissionPage = new ProposalSubmissionPage(page);

    await proposalSubmissionPage.createDraft(ProposalType.info);
    const getAllDrafts = await proposalSubmissionPage.getAllDrafts();

    expect(getAllDrafts.length).toBeGreaterThan(0);
  });

  test("7L. Should save proposal as a draft", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal04.json",
      wallet: proposal04Wallet,
    });

    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const { proposalFormValue } = await proposalSubmissionPage.createDraft(
      ProposalType.info
    );
    const draftCard = proposalSubmissionPage.getFirstDraft();
    const draftCardAllInnerText = await (await draftCard).allInnerTexts();

    expect(draftCardAllInnerText.includes(proposalFormValue.prop_name));
    expect(draftCardAllInnerText.includes(proposalFormValue.prop_abstract));
  });

  test("7M_1. Should edit a info proposal draft", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal05.json",
      wallet: proposal05Wallet,
    });

    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const { proposalFormValue } = await proposalSubmissionPage.createDraft(
      ProposalType.info
    );
    const newTitle = faker.lorem.sentence(6);

    await proposalSubmissionPage.viewFirstDraft();
    await proposalSubmissionPage.titleInput.fill(newTitle);
    await proposalSubmissionPage.continueBtn.click();

    await expect(page.getByTestId("governance-action-type-content")).toHaveText(
      ProposalType.info
    );
    await expect(page.getByTestId("title-content")).toHaveText(newTitle);
    await expect(page.getByTestId("abstract-content")).toHaveText(
      proposalFormValue.prop_abstract
    );
    await expect(page.getByTestId("motivation-content")).toHaveText(
      proposalFormValue.prop_motivation
    );
    await expect(page.getByTestId("rationale-content")).toHaveText(
      proposalFormValue.prop_rationale
    );
    await expect(page.getByTestId("link-0-text-content")).toHaveText(
      proposalFormValue.proposal_links[0].prop_link_text
    );
  });

  test("7N. Should submit a draft proposal", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal06.json",
      wallet: proposal06Wallet,
    });

    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const { proposalFormValue } = await proposalSubmissionPage.createDraft(
      ProposalType.info
    );

    await proposalSubmissionPage.viewFirstDraft();
    await proposalSubmissionPage.continueBtn.click();
    await proposalSubmissionPage.submitBtn.click();

    await expect(page.getByTestId("submit-as-GA-button")).toBeVisible();
    await expect(page.getByTestId("title-content")).toHaveText(
      proposalFormValue.prop_name
    );
    await expect(page.getByTestId("governance-action-type-content")).toHaveText(
      ProposalType.info
    );
    await expect(page.getByTestId("abstract-content")).toHaveText(
      proposalFormValue.prop_abstract
    );
    await expect(page.getByTestId("motivation-content")).toHaveText(
      proposalFormValue.prop_motivation
    );
    await expect(page.getByTestId("rationale-content")).toHaveText(
      proposalFormValue.prop_rationale
    );
    await expect(page.getByTestId("link-0-text-content")).toHaveText(
      proposalFormValue.proposal_links[0].prop_link_text
    );
  });
});

test.describe("Treasury Proposal Draft", () => {
  test.use({ storageState: ".auth/proposal07.json", wallet: proposal07Wallet });

  test("7M_2. Should edit a treasury proposal draft", async ({ page }) => {
    await skipIfTreasuryAndBootstrapping(ProposalType.treasury);

    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const { proposalFormValue } = await proposalSubmissionPage.createDraft(
      ProposalType.treasury
    );

    const newTitle = faker.lorem.sentence(6);

    await proposalSubmissionPage.viewFirstDraft();
    await proposalSubmissionPage.titleInput.fill(newTitle);
    await proposalSubmissionPage.continueBtn.click();

    await expect(page.getByTestId("governance-action-type-content")).toHaveText(
      ProposalType.treasury
    );
    await expect(page.getByTestId("title-content")).toHaveText(newTitle);
    await expect(page.getByTestId("abstract-content")).toHaveText(
      proposalFormValue.prop_abstract
    );
    await expect(page.getByTestId("motivation-content")).toHaveText(
      proposalFormValue.prop_motivation
    );
    await expect(page.getByTestId("rationale-content")).toHaveText(
      proposalFormValue.prop_rationale
    );
    await expect(page.getByTestId("receiving-address-content")).toHaveText(
      proposalFormValue.prop_receiving_address
    );
    await expect(page.getByTestId("amount-content")).toHaveText(
      proposalFormValue.prop_amount
    );
    await expect(page.getByTestId("link-0-text-content")).toHaveText(
      proposalFormValue.proposal_links[0].prop_link_text
    );
  });
});
