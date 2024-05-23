import environments from "@constants/environments";
import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { IProposalForm, ProposalType } from "@types";
import { bech32 } from "bech32";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
});

test.describe("Accept valid data", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7E.${index + 1} Should accept valid data in ${type.toLowerCase()} proposal form`, async ({
      page,
    }) => {
      test.slow();

      const proposalSubmissionPage = new ProposalSubmissionPage(page);

      await proposalSubmissionPage.goto();

      await page.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      for (let i = 0; i < 100; i++) {
        const randomBytes = new Uint8Array(10);
        const bech32Address = bech32.encode("addr_test", randomBytes);
        const formFields: IProposalForm =
          proposalSubmissionPage.generateValidProposalFormFields(
            type,
            bech32Address
          );
        await proposalSubmissionPage.validateForm(formFields);
      }

      for (let i = 0; i < 7; i++) {
        await expect(proposalSubmissionPage.addLinkBtn).toBeVisible();
        await proposalSubmissionPage.addLinkBtn.click();
      }

      await expect(proposalSubmissionPage.addLinkBtn).toBeHidden();
    });
  });
});

test.describe("Reject invalid  data", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7F.${index + 1} Should reject invalid data in ${type.toLowerCase()} Proposal form`, async ({
      page,
    }) => {
      test.slow();

      const proposalSubmissionPage = new ProposalSubmissionPage(page);

      await proposalSubmissionPage.goto();

      await page.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      const formFields: IProposalForm =
        proposalSubmissionPage.generateInValidProposalFormFields(type);
      for (let i = 0; i < 100; i++) {
        await proposalSubmissionPage.inValidateForm(formFields);
      }
    });
  });
});
