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
  BudgetProposalContactInformationProps,
  BudgetProposalProps,
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
      test("12D_1. Should verify all field of “contact information” section", async () => {
        await expect(
          budgetProposalSubmissionPage.beneficiaryFullNameInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.beneficiaryEmailInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.beneficiaryCountrySelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.beneficiaryNationalitySelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.submissionLeadFullNameInput
        ).toBeVisible();
      });

      test("12D_2. Should verify all field of “proposal ownership” section", async () => {
        const proposalContactInformationContent =
          budgetProposalSubmissionPage.generateValidBudgetProposalContactInformation();
        await budgetProposalSubmissionPage.fillupContactInformationForm(
          proposalContactInformationContent
        );

        // default field
        await expect(
          budgetProposalSubmissionPage.companyTypeSelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.publicChampionSelect
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
      });

      test("12D_3. Should verify all field of “problem statements and proposal benefits” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(proposalInformation, 3);

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
      });

      test("12D_4. Should verify all field of “proposal details” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(proposalInformation, 4);

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
      });

      test("12D_5. Should verify all field of “costing” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(proposalInformation, 5);

        await expect(budgetProposalSubmissionPage.adaAmountInput).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.usaToAdaCnversionRateInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.preferredCurrencySelect
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.preferredCurrencyAmountInput
        ).toBeVisible();
        await expect(
          budgetProposalSubmissionPage.costBreakdownInput
        ).toBeVisible();
      });

      test("12D_6. Should verify all field of “further information” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(proposalInformation, 6);

        await expect(budgetProposalSubmissionPage.linkTextInput).toBeVisible();
        await expect(budgetProposalSubmissionPage.linkUrlInput).toBeVisible();
        await expect(budgetProposalSubmissionPage.addLinkBtn).toBeVisible();
      });

      test("12D_7. Should verify all field of “administration and auditing” section", async () => {
        const proposalInformation =
          budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
        await budgetProposalSubmissionPage.fillupForm(proposalInformation, 7);

        await expect(
          budgetProposalSubmissionPage.intersectNamedAdministratorSelect
        ).toBeVisible();
      });
    });

    test.describe("Budget proposal field validation", () => {
      test("12E_1. Should accept valid data in “contact information” section", async ({}) => {});
      test("12E_2. Should accept valid data in “proposal ownership” section", async ({}) => {});
      test("12E_3. Should accept valid data in “problem statements and proposal benefits” section", async ({}) => {});
      test("12E_4. Should accept valid data in “proposal details” section", async ({}) => {});
      test("12E_5. Should accept valid data in “costing” section", async ({}) => {});
      test("12E_6. Should accept valid data in “further information” section", async ({}) => {});

      test("12F_1. Should reject invalid data in “contact information” section", async ({}) => {});
      test("12F_2. Should reject invalid data in “proposal ownership” section", async ({}) => {});
      test("12F_3. Should reject invalid data in “problem statements and proposal benefits” section", async ({}) => {});
      test("12E_4. Should accept invalid data in “proposal details” section", async ({}) => {});
      test("12F_5. Should reject invalid data in “costing” section", async ({}) => {});
      test("12F_6. Should reject invalid data in “further information” section", async ({}) => {});
    });

    test("12G. Should validate and review submitted budget proposal", async () => {
      const proposalInformations =
        budgetProposalSubmissionPage.generateValidBudgetProposalInformation();
      await budgetProposalSubmissionPage.fillupForm(proposalInformations);

      await budgetProposalSubmissionPage.validateReviewBudgetProposal(
        proposalInformations
      );
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
  const draftContactInformationContent =
    (await budgetSubmissionPage.createDraftBudgetProposal()) as BudgetProposalContactInformationProps;
  const getAddDrafts = await budgetSubmissionPage.getAllDrafts();

  expect(getAddDrafts.length).toBeGreaterThan(0);

  await budgetSubmissionPage.viewLastDraft();

  await expect(budgetSubmissionPage.beneficiaryFullNameInput).toHaveValue(
    draftContactInformationContent.beneficiaryFullName
  );
  await expect(budgetSubmissionPage.beneficiaryEmailInput).toHaveValue(
    draftContactInformationContent.beneficiaryEmail
  );
  await expect(budgetSubmissionPage.beneficiaryCountrySelect).toHaveText(
    draftContactInformationContent.beneficiaryCountry
  );
  await expect(budgetSubmissionPage.beneficiaryNationalitySelect).toHaveText(
    draftContactInformationContent.beneficiaryNationality
  );
  await expect(budgetSubmissionPage.submissionLeadFullNameInput).toHaveValue(
    draftContactInformationContent.submissionLeadFullName
  );
  await expect(budgetSubmissionPage.submissionLeadEmailInput).toHaveValue(
    draftContactInformationContent.submissionLeadEmail
  );
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

  for (let i = 0; i < 8; i++) {
    await budgetSubmissionPage.continueBtn.click();
  }
  await budgetSubmissionPage.submitBtn.click();

  const budgetDiscussionDetailsPage = new BudgetDiscussionDetailsPage(page);
  await budgetDiscussionDetailsPage.validateProposalDetails(draftContact);

  await budgetDiscussionDetailsPage.deleteProposal();
});
