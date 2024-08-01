import environments from "@constants/environments";
import { createTempUserAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
});

test("7H. Should submit a proposal as governance action", async ({
  page,
  browser,
}, testInfo) => {
  test.setTimeout(testInfo.timeout + environments.txTimeOut);

  const wallet = await walletManager.popWallet("proposalSubmission");

  const tempUserAuth = await createTempUserAuth(page, wallet);

  const userPage = await createNewPageWithWallet(browser, {
    storageState: tempUserAuth,
    wallet,
  });

  const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
  await proposalDiscussionPage.goto();
  await proposalDiscussionPage.verifyIdentityBtn.click();

  await proposalDiscussionPage.setUsername(
    faker.internet.userName().toLowerCase()
  );

  const proposalSubmissionPage = new ProposalSubmissionPage(userPage);
  await proposalSubmissionPage.proposalCreateBtn.click();
  await proposalDiscussionPage.continueBtn.click();

  await proposalSubmissionPage.createProposal();

  await userPage.getByTestId("submit-as-GA-button").click();

  await userPage.getByTestId("agree-checkbox").click();
  proposalSubmissionPage.continueBtn.click();

  await proposalSubmissionPage.fillUpValidMetadata();

  await expect(userPage.getByTestId("ga-submitted-modal-title")).toHaveText(
    /governance action submitted!/i,
    {
      timeout: 10_000,
    }
  );

  await waitForTxConfirmation(userPage);
});
