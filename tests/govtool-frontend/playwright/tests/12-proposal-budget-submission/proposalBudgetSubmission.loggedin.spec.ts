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
    const budgetProposalSubmissionPage = new BudgetDiscussionSubmissionPage(
      page
    );
    await budgetProposalSubmissionPage.goto();
    await expect(
      budgetProposalSubmissionPage.beneficiaryFullNameInput
    ).toBeVisible();
  });

  test.describe("Budget proposal field verification", () => {
    test("12D_1. Should verify all field of “contact information” section", async ({}) => {});
    test("12D_2. Should verify all field of “proposal ownership” section", async ({}) => {});
    test("12D_3. Should verify all field of “problem statements and proposal benefits” section", async ({}) => {});
    test("12D_4. Should verify all field of “costing” section", async ({}) => {});
    test("12D_5. Should verify all field of “further information” section", async ({}) => {});
    test("12D_6. Should verify all field of “administration and auditing” section", async ({}) => {});
    test("12D_7. Should verify all field of “submit” section", async ({}) => {});
  });

  test.describe("Budget proposal field validation", () => {
    test("12E_1. Should accept valid data in “contact information” section", async ({}) => {});
    test("12E_2. Should accept valid data in “proposal ownership” section", async ({}) => {});
    test("12E_3. Should accept valid data in “problem statements and proposal benefits” section", async ({}) => {});
    test("12E_4. Should accept valid data in “costing” section", async ({}) => {});
    test("12E_5. Should accept valid data in “further information” section", async ({}) => {});
    test("12E_6. Should accept valid data in “administration and auditing” section", async ({}) => {});
    test("12E_7. Should accept valid data in “submit” section", async ({}) => {});
    test("12F_1. Should reject invalid data in “contact information” section", async ({}) => {});
    test("12F_2. Should reject invalid data in “proposal ownership” section", async ({}) => {});
    test("12F_3. Should reject invalid data in “problem statements and proposal benefits” section", async ({}) => {});
    test("12F_4. Should reject invalid data in “costing” section", async ({}) => {});
    test("12F_5. Should reject invalid data in “further information” section", async ({}) => {});
    test("12F_6. Should reject invalid data in “administration and auditing” section", async ({}) => {});
    test("12F_7. Should reject invalid data in “submit” section", async ({}) => {});
  });

  test("12G. Should validate and review submitted budget proposal", async ({}) => {});
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
