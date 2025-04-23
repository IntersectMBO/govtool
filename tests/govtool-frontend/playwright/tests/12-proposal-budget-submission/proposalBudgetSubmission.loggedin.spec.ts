import {
  budgetProposal01Wallet,
  budgetProposal02Wallet,
  budgetProposal03Wallet,
  budgetProposal04Wallet,
} from "@constants/staticWallets";
import { test } from "@fixtures/budgetProposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import BudgetDiscussionSubmissionPage from "@pages/budgetDiscussionSubmissionPage";
import { expect } from "@playwright/test";
import {
  BudgetProposalOwnershipProps,
  BudgetProposalProps,
  BudgetProposalStageEnum,
  CompanyEnum,
} from "@types";

test.beforeEach(async () => {
  await setAllureEpic("12. Proposal Budget Submission");
});

test.describe("Budget proposal 01 wallet", () => {
  test.use({
    storageState: ".auth/budgetProposal01.json",
    wallet: budgetProposal01Wallet,
  });

  test("12B. Should access proposal creation page in connected state", async ({
    page,
  }) => {
    await page.goto("/");
    await page.getByTestId("budget-discussion-link").click();
    await page.getByTestId("verify-identity-button").click();

    await expect(
      page.getByTestId("propose-a-budget-discussion-button")
    ).toBeVisible({ timeout: 60_000 });
  });

  test.describe("Budget proposal with proposalSubmissionPageNavigation", () => {
    let budgetProposalSubmissionPage: BudgetDiscussionSubmissionPage;
    test.beforeEach(async ({ page }) => {
      budgetProposalSubmissionPage = new BudgetDiscussionSubmissionPage(page);
      await budgetProposalSubmissionPage.goto();
    });

    test.describe("Budget proposal field verification", () => {
      test("12D_1. Should verify all field of “proposal ownership” section", async () => {
        // default field
        await expect(
          budgetProposalSubmissionPage.companyTypeSelect
        ).toBeVisible();

        await expect(
          budgetProposalSubmissionPage.contactDetailsInput
        ).toBeVisible();

        // company type field
        await budgetProposalSubmissionPage.companyTypeSelect.click();
        await budgetProposalSubmissionPage.currentPage
          .getByRole("option", { name: CompanyEnum.Company })
          .click(); //BUG missing testId

        await expect(
          budgetProposalSubmissionPage.companyNameInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.companyDomainNameInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.countryOfIncorporationBtn
        ).toBeVisible();

        // group type field
        await budgetProposalSubmissionPage.companyTypeSelect.click();
        await budgetProposalSubmissionPage.currentPage
          .getByRole("option", { name: CompanyEnum.Group })
          .click(); //BUG missing testId
        await expect(budgetProposalSubmissionPage.groupNameInput).toBeVisible();
        await expect(budgetProposalSubmissionPage.groupTypeInput).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.keyInformationOfGroupInput
        ).toBeVisible();

        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });

      test("12D_2. Should verify all field of “problem statements and proposal benefits” section", async () => {
        const proposalOwnership =
          budgetProposalSubmissionPage.generateValidProposalOwnerShip();
        await budgetProposalSubmissionPage.fillupProposalOwnershipForm(
          proposalOwnership
        );

        await expect(
          budgetProposalSubmissionPage.problemStatementInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.proposalBenefitInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.roadmapNameSelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.budgetDiscussionTypeSelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.committeeAlignmentTypeSelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.suplimentaryEndorsementInput
        ).toBeVisible();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });

      test("12D_3. Should verify all field of “proposal details” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.ProblemStatementAndBenefits
        );

        await expect(
          budgetProposalSubmissionPage.proposalNameInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.proposalDescriptionInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.proposalKeyDependenciesInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.milestonesInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.teamSizeAndDurationInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.previousExperienceInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.contractingTypeSelect
        ).toBeVisible();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });

      test("12D_4. Should verify all field of “costing” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.ProposalDetails
        );

        await expect(budgetProposalSubmissionPage.adaAmountInput).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.usaToAdaCnversionRateInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.preferredCurrencySelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.preferredCurrencyInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.costBreakdownInput
        ).toBeVisible();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });

      test("12D_5. Should verify all field of “further information” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.Costing
        );

        await expect(budgetProposalSubmissionPage.linkTextInput).toBeVisible();
        await expect(budgetProposalSubmissionPage.linkUrlInput).toBeVisible();
        await expect(budgetProposalSubmissionPage.addLinkBtn).toBeVisible();
      });

      test("12D_6. Should verify all field of “administration and auditing” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.FurtherInformation
        );

        await expect(
          budgetProposalSubmissionPage.intersectNamedAdministratorSelect
        ).toBeVisible();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });

      test("12D_7. Should verify all field of “Submit” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.AdministrationAndAuditing
        );

        await expect(budgetProposalSubmissionPage.submitCheckbox).toBeVisible();
        await expect(budgetProposalSubmissionPage.saveDraftBtn).toBeVisible();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });
    });

    test("12G. Should validate and review submitted budget proposal", async () => {
      const proposalInformations =
        budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
      await budgetProposalSubmissionPage.fillupForm(proposalInformations);

      await budgetProposalSubmissionPage.validateReviewBudgetProposal(
        proposalInformations
      );
    });
    test.describe("Budget proposal field validation", () => {
      test("12E_1. Should accept valid data in “Costing” section", async () => {
        test.slow(); // Brute-force testing with 50 random data
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.ProposalDetails
        );

        for (let i = 0; i < 50; i++) {
          await budgetProposalSubmissionPage.validateCostingSection();
        }

        await budgetProposalSubmissionPage.fillCostingSectionExceptAmountInputs();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeEnabled();
      });

      test("12E_2. Should accept valid data in “further information” section", async () => {
        test.slow(); // Brute-force testing with 50 random data
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.Costing
        );

        for (let i = 0; i < 50; i++) {
          await budgetProposalSubmissionPage.validateFurtherInformationSection();
        }

        for (let i = 0; i < 18; i++) {
          await expect(budgetProposalSubmissionPage.addLinkBtn).toBeVisible();
          await budgetProposalSubmissionPage.addLinkBtn.click();
        }
        await expect(budgetProposalSubmissionPage.addLinkBtn).toBeHidden();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeEnabled();
      });

      test("12F_1. Should reject valid data in “Costing” section", async () => {
        test.slow(); // Brute-force testing with 50 random data
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.ProposalDetails
        );

        for (let i = 0; i < 50; i++) {
          await budgetProposalSubmissionPage.validateCostingSection(false);
        }

        await budgetProposalSubmissionPage.fillCostingSectionExceptAmountInputs();
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });

      test("12F_2. Should reject invalid data in “further information” section", async () => {
        test.slow(); // Brute-force testing with 50 random data
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(
          proposalInformation,
          BudgetProposalStageEnum.Costing
        );

        for (let i = 0; i < 50; i++) {
          await budgetProposalSubmissionPage.validateFurtherInformationSection(
            false
          );
        }
        await expect(budgetProposalSubmissionPage.continueBtn).toBeDisabled();
      });
    });
  });
});

test("12C. Should save and view draft proposal", async ({ browser }) => {
  const page = await createNewPageWithWallet(browser, {
    storageState: ".auth/budgetProposal02.json",
    wallet: budgetProposal02Wallet,
  });

  const budgetSubmissionPage = new BudgetDiscussionSubmissionPage(page);
  await budgetSubmissionPage.goto();
  const draftProposalOwnership =
    (await budgetSubmissionPage.createDraftBudgetProposal()) as BudgetProposalOwnershipProps;
  const getAddDrafts = await budgetSubmissionPage.getAllDrafts();

  expect(getAddDrafts.length).toBeGreaterThan(0);

  await budgetSubmissionPage.viewLastDraft();

  await expect(budgetSubmissionPage.companyTypeSelect).toHaveText(
    draftProposalOwnership.companyType
  );

  await expect(budgetSubmissionPage.contactDetailsInput).toHaveValue(
    draftProposalOwnership.contactDetails
  );

  if (draftProposalOwnership.companyType === "Group") {
    await expect(budgetSubmissionPage.groupNameInput).toHaveValue(
      draftProposalOwnership.groupName
    );
    await expect(budgetSubmissionPage.groupTypeInput).toHaveValue(
      draftProposalOwnership.groupType
    );
    await expect(budgetSubmissionPage.keyInformationOfGroupInput).toHaveValue(
      draftProposalOwnership.groupKeyIdentity
    );
  }
  if (draftProposalOwnership.companyType === "Company") {
    await expect(budgetSubmissionPage.companyNameInput).toHaveValue(
      draftProposalOwnership.companyName
    );
    await expect(budgetSubmissionPage.companyDomainNameInput).toHaveValue(
      draftProposalOwnership.companyDomainName
    );
    await expect(budgetSubmissionPage.countryOfIncorporationBtn).toHaveText(
      draftProposalOwnership.countryOfIncorportation
    );
  }
});

test("12H. Should submit a valid budget proposal", async ({ browser }) => {
  const page = await createNewPageWithWallet(browser, {
    storageState: ".auth/budgetProposal03.json",
    wallet: budgetProposal03Wallet,
  });
  const budgetSubmissionPage = new BudgetDiscussionSubmissionPage(page);
  await budgetSubmissionPage.goto();
  const { proposalId, proposalDetails } =
    await budgetSubmissionPage.createBudgetProposal();

  const budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
  await budgetDiscussionDetailsPage.goto(proposalId);

  await budgetDiscussionDetailsPage.validateProposalDetails(proposalDetails);

  await budgetDiscussionDetailsPage.deleteProposal();
});

test("12I. Should submit a valid draft budget proposal", async ({
  browser,
}) => {
  test.slow();
  const page = await createNewPageWithWallet(browser, {
    storageState: ".auth/budgetProposal04.json",
    wallet: budgetProposal04Wallet,
  });

  const budgetSubmissionPage = new BudgetDiscussionSubmissionPage(page);
  await budgetSubmissionPage.goto();
  const draftContact = (await budgetSubmissionPage.createDraftBudgetProposal(
    true
  )) as BudgetProposalProps;

  await budgetSubmissionPage.viewLastDraft();

  for (let i = 0; i < 7; i++) {
    await budgetSubmissionPage.continueBtn.click();
  }
  await budgetSubmissionPage.submitBtn.click();

  const budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
  await budgetDiscussionDetailsPage.validateProposalDetails(draftContact);

  await budgetDiscussionDetailsPage.deleteProposal();
});
