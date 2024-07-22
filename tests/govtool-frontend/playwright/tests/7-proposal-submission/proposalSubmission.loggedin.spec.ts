import { proposal01Wallet } from "@constants/staticWallets";
import { createTempUserAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { invalid, valid } from "@mock/index";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { Page, expect } from "@playwright/test";
import { ProposalCreateRequest, ProposalType } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
});

test.describe("Proposal created logged state", () => {
  test.use({ storageState: ".auth/proposal01.json", wallet: proposal01Wallet });
  test("7B. Should access proposal creation page", async ({ page }) => {
    await page.goto("/");
    await page.getByTestId("propose-governance-actions-button").click();

    await expect(
      page.getByText("Create a Governance Action", { exact: true })
    ).toBeVisible();
  });

  test.describe("Accept valid data", () => {
    Object.values(ProposalType).map((type: ProposalType, index) => {
      test(`7E_${index + 1}. Should accept valid data in ${type.toLowerCase()} proposal form`, async ({
        page,
      }) => {
        test.slow(); // Brute-force testing with 100 random data

        const proposalSubmissionPage = new ProposalSubmissionPage(page);

        await proposalSubmissionPage.goto();

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByTestId(`${type.toLocaleLowerCase()}-button`).click();
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

        await proposalSubmissionPage.governanceActionType.click();
        await page.getByTestId(`${type.toLocaleLowerCase()}-button`).click();
        await proposalSubmissionPage.addLinkBtn.click();

        const formFields: ProposalCreateRequest =
          proposalSubmissionPage.generateInValidProposalFormFields(type);

        for (let i = 0; i < 100; i++) {
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
        await expect(page.getByText(type, { exact: true })).toBeVisible(); // BUG missing test id
        await expect(page.getByText(proposal.prop_name)).toBeVisible(); // BUG missing test id
        await expect(page.getByText(proposal.prop_abstract)).toBeVisible(); // BUG missing test id
        await expect(page.getByText(proposal.prop_rationale)).toBeVisible(); // BUG missing test id
        await expect(page.getByText(proposal.prop_motivation)).toBeVisible(); // BUG missing test id
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
        await expect(page.getByTestId("link-text-content")).toHaveText(
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
        for (let i = 0; i < 100; i++) {
          await proposalSubmissionPage.metadataUrlInput.fill(
            faker.internet.url()
          );
          await expect(page.getByTestId("url-input-error-text")).toBeHidden();
        }
      });

      test("7J_2. Should reject invalid metadata anchor on proposal submission", async ({
        page,
      }) => {
        test.slow(); // Brute-force testing with 100 random data
        for (let i = 0; i < 100; i++) {
          await proposalSubmissionPage.metadataUrlInput.fill(invalid.url());
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

test.describe("Temporary proposal users", () => {
  let userPage: Page;
  test.beforeEach(async ({ page, browser }) => {
    const wallet = (await ShelleyWallet.generate()).json();
    const tempUserAuth = await createTempUserAuth(page, wallet);

    userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();
    await proposalDiscussionPage.verifyIdentityBtn.click();

    await proposalDiscussionPage.setUsername(valid.username());
  });

  test.describe("Info Proposal Draft", () => {
    let proposalSubmissionPage: ProposalSubmissionPage;
    let proposalFormValue: ProposalCreateRequest;

    test.beforeEach(async () => {
      proposalSubmissionPage = new ProposalSubmissionPage(userPage);
      await proposalSubmissionPage.proposalCreateBtn.click();
      await proposalSubmissionPage.continueBtn.click();

      await proposalSubmissionPage.addLinkBtn.click();
      proposalFormValue =
        proposalSubmissionPage.generateValidProposalFormFields(
          ProposalType.info,
          true
        );
      await proposalSubmissionPage.fillupForm(proposalFormValue);

      await proposalSubmissionPage.saveDraftBtn.click();
      await proposalSubmissionPage.closeDraftSuccessModalBtn.click();

      await proposalSubmissionPage.proposalCreateBtn.click();
    });

    test("7C. Should list unfinished Draft ", async () => {
      const getAllDrafts = await proposalSubmissionPage.getAllDrafts();

      expect(getAllDrafts.length).toBeGreaterThan(0);
    });

    test("7L. Should save proposal as a draft", async () => {
      const draftCard = proposalSubmissionPage.getFirstDraft();
      const draftCardAllInnerText = await (await draftCard).allInnerTexts();

      expect(draftCardAllInnerText.includes(proposalFormValue.prop_name));
      expect(draftCardAllInnerText.includes(proposalFormValue.prop_abstract));
    });

    test("7M_1. Should edit a info proposal draft", async ({}) => {
      const newTitle = faker.lorem.sentence(6);

      await proposalSubmissionPage.viewFirstDraft();
      await proposalSubmissionPage.titleInput.fill(newTitle);
      await proposalSubmissionPage.continueBtn.click();

      await expect(
        userPage.getByTestId("governance-action-type-content")
      ).toHaveText(ProposalType.info);
      await expect(userPage.getByTestId("title-content")).toHaveText(newTitle);
      await expect(userPage.getByTestId("abstract-content")).toHaveText(
        proposalFormValue.prop_abstract
      );
      await expect(userPage.getByTestId("motivation-content")).toHaveText(
        proposalFormValue.prop_motivation
      );
      await expect(userPage.getByTestId("rationale-content")).toHaveText(
        proposalFormValue.prop_rationale
      );
      await expect(userPage.getByTestId("link-text-content")).toHaveText(
        proposalFormValue.proposal_links[0].prop_link_text
      );
    });

    test("7N. Should submit a draft proposal", async ({}) => {
      await proposalSubmissionPage.viewFirstDraft();
      await proposalSubmissionPage.continueBtn.click();
      await proposalSubmissionPage.submitBtn.click();

      await expect(userPage.getByTestId("submit-as-GA-button")).toBeVisible();
      await expect(
        userPage.getByText(ProposalType.info, { exact: true })
      ).toBeVisible(); // BUG missing test id
      await expect(
        userPage.getByText(proposalFormValue.prop_name)
      ).toBeVisible(); // BUG missing test id
      await expect(
        userPage.getByText(proposalFormValue.prop_abstract)
      ).toBeVisible(); // BUG missing test id
      await expect(
        userPage.getByText(proposalFormValue.prop_rationale)
      ).toBeVisible(); // BUG missing test id
      await expect(
        userPage.getByText(proposalFormValue.prop_motivation)
      ).toBeVisible(); // BUG missing test id
    });
  });

  test.describe("Treasury Proposal Draft", () => {
    let proposalSubmissionPage: ProposalSubmissionPage;
    let proposalFormValue: ProposalCreateRequest;

    test.beforeEach(async () => {
      proposalSubmissionPage = new ProposalSubmissionPage(userPage);
      await proposalSubmissionPage.proposalCreateBtn.click();
      await proposalSubmissionPage.continueBtn.click();

      await proposalSubmissionPage.addLinkBtn.click();
      proposalFormValue =
        proposalSubmissionPage.generateValidProposalFormFields(
          ProposalType.treasury,
          true,
          ShelleyWallet.fromJson(proposal01Wallet).rewardAddressBech32(0)
        );
      await proposalSubmissionPage.fillupForm(proposalFormValue);

      await proposalSubmissionPage.saveDraftBtn.click();
      await proposalSubmissionPage.closeDraftSuccessModalBtn.click();

      await proposalSubmissionPage.proposalCreateBtn.click();
    });

    test("7M_2. Should edit a treasury proposal draft", async () => {
      const newTitle = faker.lorem.sentence(6);

      await proposalSubmissionPage.viewFirstDraft();
      await proposalSubmissionPage.titleInput.fill(newTitle);
      await proposalSubmissionPage.continueBtn.click();

      await expect(
        userPage.getByTestId("governance-action-type-content")
      ).toHaveText(ProposalType.treasury);
      await expect(userPage.getByTestId("title-content")).toHaveText(newTitle);
      await expect(userPage.getByTestId("abstract-content")).toHaveText(
        proposalFormValue.prop_abstract
      );
      await expect(userPage.getByTestId("motivation-content")).toHaveText(
        proposalFormValue.prop_motivation
      );
      await expect(userPage.getByTestId("rationale-content")).toHaveText(
        proposalFormValue.prop_rationale
      );
      await expect(
        userPage.getByTestId("receiving-address-content")
      ).toHaveText(proposalFormValue.prop_receiving_address);
      await expect(userPage.getByTestId("amount-content")).toHaveText(
        proposalFormValue.prop_amount
      );
      await expect(userPage.getByTestId("link-text-content")).toHaveText(
        proposalFormValue.proposal_links[0].prop_link_text
      );
    });
  });
});
