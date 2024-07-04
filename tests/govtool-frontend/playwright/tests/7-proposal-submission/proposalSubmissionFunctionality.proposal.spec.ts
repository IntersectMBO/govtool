import environments from "@constants/environments";
import { proposal01Wallet } from "@constants/staticWallets";
import { createTempUserAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { ProposalCreateRequest, ProposalType } from "@types";
import walletManager from "lib/walletManager";

let proposalSubmissionPage: ProposalSubmissionPage;
test.use({ storageState: ".auth/proposal01.json", wallet: proposal01Wallet });

test.beforeEach(async ({ page, proposalId }) => {
  await setAllureEpic("7. Proposal submission");

  const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
  await proposalDiscussionDetailsPage.goto(proposalId);

  await proposalDiscussionDetailsPage.verifyIdentityBtn.click();
  await proposalDiscussionDetailsPage.submitAsGABtn.click();

  proposalSubmissionPage = new ProposalSubmissionPage(page);
  await page.click("input#submission-checkbox"); // BUG missing test id
  await page.getByRole("button", { name: "Continue" }).click();
});

test("7H. should submit a proposal", async ({ page, browser }, testInfo) => {
  test.setTimeout(testInfo.timeout + environments.txTimeOut);

  const wallet = await walletManager.popWallet("proposalSubmission");

  const tempUserAuth = await createTempUserAuth(page, wallet);

  const userPage = await createNewPageWithWallet(browser, {
    storageState: tempUserAuth,
    wallet,
  });

  const proposalSubmissionPage = new ProposalSubmissionPage(userPage);
  await proposalSubmissionPage.goto();

  await userPage.getByTestId(`${ProposalType.info}-radio`).click();
  await proposalSubmissionPage.continueBtn.click();

  const proposal: ProposalCreateRequest =
    proposalSubmissionPage.generateValidProposalFormFields(ProposalType.info);

  await proposalSubmissionPage.register({ ...proposal });

  await expect(proposalSubmissionPage.registrationSuccessModal).toBeVisible({
    timeout: 10_000,
  });

  await waitForTxConfirmation(userPage);
});
