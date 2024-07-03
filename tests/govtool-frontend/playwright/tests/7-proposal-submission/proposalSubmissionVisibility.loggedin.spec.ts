import { proposal01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { ProposalType } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
});

test.use({ storageState: ".auth/proposal01.json", wallet: proposal01Wallet });

test("7B. Should access proposal creation page", async ({ page }) => {
  await page.goto("/");
  await page.getByTestId("propose-governance-actions-button").click();

  await expect(
    page.getByText("Create a Governance Action", { exact: true })
  ).toBeVisible();
});

test("7C. Should list unfinished Draft ", async ({ page }) => {
  const proposalSubmissionPage = new ProposalSubmissionPage(page);
  await proposalSubmissionPage.goto();

  await proposalSubmissionPage.continueBtn.click();
  await proposalSubmissionPage.fillupForm(
    proposalSubmissionPage.generateValidProposalFormFields(0, true)
  );
  await proposalSubmissionPage.saveDraftBtn.click();
  await proposalSubmissionPage.closeDraftSuccessModalBtn.click();

  await proposalSubmissionPage.proposalCreateBtn.click();

  const getAllDrafts = await proposalSubmissionPage.getAllDrafts();

  expect(getAllDrafts.length).toBeGreaterThan(0);
});

test.describe("Verify Proposal form", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7D_${index + 1}. Verify ${type.toLocaleLowerCase()} proposal form`, async ({
      page,
    }) => {
      const proposalSubmissionPage = new ProposalSubmissionPage(page);
      await proposalSubmissionPage.goto();

      await page.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

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
