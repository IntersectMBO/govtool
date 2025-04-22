import { proposal05AuthFile, proposal06AuthFile } from "@constants/auth";
import environments from "@constants/environments";
import {
  proposal01Wallet,
  proposal03Wallet,
  proposal04Wallet,
  proposal06Wallet,
} from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { getDraftProposalWalletAndState } from "@helpers/auth";
import {
  skipIfNotInfoAndBootstrapping,
  skipIfNotHardFork,
  isBootStrapingPhase,
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
    await page.getByTestId("proposal-discussion-link").click();

    await expect(page.getByText("Proposals", { exact: true })).toHaveCount(2);
  });

  test.describe("Accept valid data", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7E_${index + 1}. Should accept valid data in ${type.toLowerCase()} proposal form`, async ({
        page,
      }) => {
        await skipIfNotInfoAndBootstrapping(type);

        test.slow(); // Brute-force testing with 50 random data

        const proposalSubmissionPage = new ProposalSubmissionPage(page);

        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByTestId(`${type.toLocaleLowerCase()}-button`).click();
        await proposalSubmissionPage.addLinkBtn.click();

        if (type === ProposalType.updatesToTheConstitution) {
          await proposalSubmissionPage.guardrailsScriptCheckbox.click();
        }

        for (let i = 0; i < 50; i++) {
          const rewardAddressBech32 = (
            await ShelleyWallet.generate()
          ).rewardAddressBech32(environments.networkId);
          const formFields: ProposalCreateRequest =
            await proposalSubmissionPage.generateValidProposalFormFields({
              proposalType: type,
              receivingAddress: rewardAddressBech32,
              forValidation: true,
            });
          await proposalSubmissionPage.validateForm(formFields);
        }

        if (type === ProposalType.treasury) {
          for (let i = 0; i < 9; i++) {
            await expect(
              proposalSubmissionPage.addWithdrawalAddressBtn
            ).toBeVisible();
            await proposalSubmissionPage.addWithdrawalAddressBtn.click();
          }
        }

        await expect(
          proposalSubmissionPage.addWithdrawalAddressBtn
        ).toBeHidden();

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
        await skipIfNotInfoAndBootstrapping(type);

        test.slow(); // Brute-force testing with 50 random data

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByTestId(`${type.toLocaleLowerCase()}-button`).click();
        await proposalSubmissionPage.addLinkBtn.click();

        if (type === ProposalType.updatesToTheConstitution) {
          await proposalSubmissionPage.guardrailsScriptCheckbox.click();
        }

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
        await skipIfNotInfoAndBootstrapping(type);

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.addLinkBtn.click();

        const stakeAddressBech32 = ShelleyWallet.fromJson(
          wallet
        ).rewardAddressBech32(environments.networkId);
        const proposal: ProposalCreateRequest =
          await proposalSubmissionPage.generateValidProposalFormFields({
            proposalType: type,
            receivingAddress: stakeAddressBech32,
          });

        await proposalSubmissionPage.fillupForm(proposal);
        await proposalSubmissionPage.continueBtn.click();
        await proposalSubmissionPage.submitBtn.click();

        await expect(page.getByTestId("submit-as-GA-button")).toBeVisible();
        const proposalDetailsPage = new ProposalDiscussionDetailsPage(page);

        await expect(proposalSubmissionPage.titleContent).toHaveText(
          proposal.prop_name
        );
        await expect(
          proposalSubmissionPage.governanceActionTypeContent
        ).toHaveText(type);
        await expect(proposalSubmissionPage.abstractContent).toHaveText(
          proposal.prop_abstract
        );
        await expect(proposalSubmissionPage.motivationContent).toHaveText(
          proposal.prop_motivation
        );
        await expect(proposalSubmissionPage.rationaleContent).toHaveText(
          proposal.prop_rationale
        );
        await expect(proposalSubmissionPage.linkTextContent).toHaveText(
          proposal.proposal_links[0].prop_link_text
        );

        if (type === ProposalType.updatesToTheConstitution) {
          await expect(
            proposalSubmissionPage.constitutionUrlContent
          ).toHaveText(proposal.prop_constitution_url);
          await expect(
            proposalSubmissionPage.guardrailsScriptUrlContent
          ).toHaveText(proposal.prop_guardrails_script_url);
          await expect(
            proposalSubmissionPage.guardrailsScriptHashContent
          ).toHaveText(proposal.prop_guardrails_script_hash);
        }

        // cleanup
        await proposalDetailsPage.deleteProposal();
      });
    });
  });

  test.describe("Review fillup form", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7I_${index + 1}. Should valid review submission in ${type.toLowerCase()} Proposal form`, async ({
        page,
      }) => {
        await skipIfNotInfoAndBootstrapping(type);

        const proposalSubmissionPage = new ProposalSubmissionPage(page);
        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.addLinkBtn.click();

        const rewardAddressBech32 = ShelleyWallet.fromJson(
          proposal01Wallet
        ).rewardAddressBech32(environments.networkId);
        const proposal: ProposalCreateRequest =
          await proposalSubmissionPage.generateValidProposalFormFields({
            proposalType: type,
            receivingAddress: rewardAddressBech32,
          });

        await proposalSubmissionPage.fillupForm(proposal);
        await proposalSubmissionPage.continueBtn.click();

        await expect(
          proposalSubmissionPage.governanceActionTypeContent
        ).toHaveText(type);
        await expect(proposalSubmissionPage.titleContent).toHaveText(
          proposal.prop_name
        );
        await expect(proposalSubmissionPage.abstractContent).toHaveText(
          proposal.prop_abstract
        );
        await expect(proposalSubmissionPage.motivationContent).toHaveText(
          proposal.prop_motivation
        );
        await expect(proposalSubmissionPage.rationaleContent).toHaveText(
          proposal.prop_rationale
        );
        await expect(proposalSubmissionPage.linkTextContent).toHaveText(
          proposal.proposal_links[0].prop_link_text
        );
        if (type === ProposalType.treasury) {
          await expect(
            proposalSubmissionPage.receivingAddressContent
          ).toHaveText(proposal.prop_receiving_address);
          await expect(proposalSubmissionPage.amountContent).toHaveText(
            proposal.prop_amount
          );
        }

        if (type === ProposalType.updatesToTheConstitution) {
          await expect(
            proposalSubmissionPage.constitutionUrlContent
          ).toHaveText(proposal.prop_constitution_url);
          await expect(
            proposalSubmissionPage.guardrailsScriptUrlContent
          ).toHaveText(proposal.prop_guardrails_script_url);
          await expect(
            proposalSubmissionPage.guardrailsScriptHashContent
          ).toHaveText(proposal.prop_guardrails_script_hash);
        }
      });
    });
  });

  test.describe("Verify Proposal form", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7D_${index + 1}. Verify ${type.toLocaleLowerCase()} proposal form`, async ({
        page,
      }) => {
        await skipIfNotInfoAndBootstrapping(type);

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

        if (type === ProposalType.updatesToTheConstitution) {
          await expect(
            proposalSubmissionPage.constitutionUrlInput
          ).toBeVisible();

          await expect(
            proposalSubmissionPage.guardrailsScriptCheckbox
          ).toBeVisible();

          await proposalSubmissionPage.guardrailsScriptCheckbox.click();

          await expect(
            proposalSubmissionPage.guardrailsScriptUrlInput
          ).toBeVisible();
          await expect(
            proposalSubmissionPage.guardrailsScriptHashInput
          ).toBeVisible();
        }
      });
    });
  });

  test.describe("Proposed as a governance action", () => {
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

test.describe("Proposal Draft", () => {
  test("7C. Should list unfinished Draft ", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal03.json",
      wallet: proposal03Wallet,
    });
    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const proposalType =
      Object.values(ProposalType)[
        Math.floor(Math.random() * Object.values(ProposalType).length)
      ];
    await proposalSubmissionPage.createDraft(proposalType);
    const getAllDrafts = await proposalSubmissionPage.getAllDrafts();

    expect(getAllDrafts.length).toBeGreaterThan(0);
  });

  test("7L. Should save proposal as a draft", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal04.json",
      wallet: proposal04Wallet,
    });

    const proposalType =
      Object.values(ProposalType)[
        Math.floor(Math.random() * Object.values(ProposalType).length)
      ];

    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const createProposalType = (await isBootStrapingPhase())
      ? ProposalType.info
      : proposalType;
    const { proposalFormValue } =
      await proposalSubmissionPage.createDraft(createProposalType);
    const draftCard = proposalSubmissionPage.getFirstDraft();
    const draftCardAllInnerText = await (await draftCard).allInnerTexts();

    expect(draftCardAllInnerText.includes(proposalFormValue.prop_name));
    expect(draftCardAllInnerText.includes(proposalFormValue.prop_abstract));

    (await draftCard)
      .locator('[data-testid^="draft-"][data-testid$="-start-editing"]')
      .click();

    await expect(proposalSubmissionPage.governanceActionType).toHaveText(
      createProposalType,
      { timeout: 60_000 }
    );
    await expect(proposalSubmissionPage.titleInput).toHaveValue(
      proposalFormValue.prop_name
    );
    await expect(proposalSubmissionPage.abstractInput).toHaveValue(
      proposalFormValue.prop_abstract
    );
    await expect(proposalSubmissionPage.motivationInput).toHaveValue(
      proposalFormValue.prop_motivation
    );
    await expect(proposalSubmissionPage.rationaleInput).toHaveValue(
      proposalFormValue.prop_rationale
    );

    if (createProposalType === ProposalType.treasury) {
      await expect(proposalSubmissionPage.receivingAddressInput).toHaveValue(
        proposalFormValue.prop_receiving_address
      );
      await expect(proposalSubmissionPage.amountInput).toHaveValue(
        proposalFormValue.prop_amount
      );
    }

    if (createProposalType === ProposalType.updatesToTheConstitution) {
      await expect(proposalSubmissionPage.constitutionUrlInput).toHaveValue(
        proposalFormValue.prop_constitution_url
      );
      await expect(proposalSubmissionPage.guardrailsScriptUrlInput).toHaveValue(
        proposalFormValue.prop_guardrails_script_url
      );
      await expect(
        proposalSubmissionPage.guardrailsScriptHashInput
      ).toHaveValue(proposalFormValue.prop_guardrails_script_hash);
    }

    await expect(proposalSubmissionPage.linkUrlInput).toHaveValue(
      proposalFormValue.proposal_links[0].prop_link
    );
    await expect(proposalSubmissionPage.linkTextInput).toHaveValue(
      proposalFormValue.proposal_links[0].prop_link_text
    );
  });

  Object.values(ProposalType).map((proposalType, index) => {
    test(`7M_${index + 1}. Should edit a ${proposalType.toLowerCase()} proposal draft`, async ({
      browser,
    }) => {
      test.slow();
      const { storageState, wallet } =
        getDraftProposalWalletAndState(proposalType);

      const page = await createNewPageWithWallet(browser, {
        storageState: storageState,
        wallet: wallet,
      });

      const proposalSubmissionPage = new ProposalSubmissionPage(page);
      const { proposalFormValue } = await proposalSubmissionPage.createDraft(
        proposalType as ProposalType
      );
      const newTitle = faker.lorem.sentence(6);
      const newTreasuryAddress = (
        await ShelleyWallet.generate()
      ).rewardAddressBech32(environments.networkId);
      const newConstitutionUrl = faker.internet.url();

      await proposalSubmissionPage.viewFirstDraft();
      await proposalSubmissionPage.titleInput.fill(newTitle);
      if (proposalType === ProposalType.treasury) {
        await proposalSubmissionPage.receivingAddressInput.fill(
          newTreasuryAddress
        );
      }
      if (proposalType === ProposalType.updatesToTheConstitution) {
        await proposalSubmissionPage.constitutionUrlInput.fill(
          newConstitutionUrl
        );
      }
      await proposalSubmissionPage.continueBtn.click();

      await expect(
        proposalSubmissionPage.governanceActionTypeContent
      ).toHaveText(proposalType);
      await expect(proposalSubmissionPage.titleContent).toHaveText(newTitle);
      await expect(proposalSubmissionPage.abstractContent).toHaveText(
        proposalFormValue.prop_abstract
      );
      await expect(proposalSubmissionPage.motivationContent).toHaveText(
        proposalFormValue.prop_motivation
      );
      await expect(proposalSubmissionPage.rationaleContent).toHaveText(
        proposalFormValue.prop_rationale
      );
      await expect(proposalSubmissionPage.linkTextContent).toHaveText(
        proposalFormValue.proposal_links[0].prop_link_text
      );

      if (proposalType === ProposalType.treasury) {
        await expect(proposalSubmissionPage.receivingAddressContent).toHaveText(
          newTreasuryAddress
        );
        await expect(proposalSubmissionPage.amountContent).toHaveText(
          proposalFormValue.prop_amount
        );
      }

      if (proposalType === ProposalType.updatesToTheConstitution) {
        await expect(proposalSubmissionPage.constitutionUrlContent).toHaveText(
          newConstitutionUrl
        );
        await expect(
          proposalSubmissionPage.guardrailsScriptUrlContent
        ).toHaveText(proposalFormValue.prop_guardrails_script_url);
        await expect(
          proposalSubmissionPage.guardrailsScriptHashContent
        ).toHaveText(proposalFormValue.prop_guardrails_script_hash);
      }
    });
  });

  test("7N. Should submit a draft proposal", async ({ browser }) => {
    const page = await createNewPageWithWallet(browser, {
      storageState: proposal06AuthFile,
      wallet: proposal06Wallet,
    });

    test.slow();

    const proposalType =
      Object.values(ProposalType)[
        Math.floor(Math.random() * Object.values(ProposalType).length)
      ];

    const proposalSubmissionPage = new ProposalSubmissionPage(page);
    const { proposalFormValue } =
      await proposalSubmissionPage.createDraft(proposalType);

    await proposalSubmissionPage.viewFirstDraft();
    await proposalSubmissionPage.continueBtn.click();
    await proposalSubmissionPage.submitBtn.click();

    await expect(page.getByTestId("submit-as-GA-button")).toBeVisible();
    const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(
      page
    );
    await expect(proposalSubmissionPage.titleContent).toHaveText(
      proposalFormValue.prop_name
    );
    await expect(proposalSubmissionPage.governanceActionTypeContent).toHaveText(
      proposalType
    );
    await expect(proposalSubmissionPage.abstractContent).toHaveText(
      proposalFormValue.prop_abstract
    );
    await expect(proposalSubmissionPage.motivationContent).toHaveText(
      proposalFormValue.prop_motivation
    );
    await expect(proposalSubmissionPage.rationaleContent).toHaveText(
      proposalFormValue.prop_rationale
    );
    await expect(proposalSubmissionPage.linkTextContent).toHaveText(
      proposalFormValue.proposal_links[0].prop_link_text
    );

    if (proposalType === ProposalType.updatesToTheConstitution) {
      await expect(proposalSubmissionPage.constitutionUrlContent).toHaveText(
        proposalFormValue.prop_constitution_url
      );
      await expect(
        proposalSubmissionPage.guardrailsScriptUrlContent
      ).toHaveText(proposalFormValue.prop_guardrails_script_url);
      await expect(
        proposalSubmissionPage.guardrailsScriptHashContent
      ).toHaveText(proposalFormValue.prop_guardrails_script_hash);
    }

    //cleanup
    await proposalDiscussionDetailsPage.deleteProposal();
  });
});
